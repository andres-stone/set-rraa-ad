
# obtencion bases finales 
# bases mensuales flujos laborales
# 5.2 clasificacion codificacion
# 5.5 nuevas variables unidades
#  sec

# seteo previo ----
rm(list = ls())
options(scipen = 999, digits = 4)
gc()

# librerias
library(dplyr)
library(arrow)
library(purrr)
library(duckdb)
library(duckdbfs)
library(lubridate)
library(glue)

# conexion
con <- duckdbfs::cached_connection()

duckdbfs::duckdb_s3_config(
  conn = con,
  s3_access_key_id = Sys.getenv("ACCESS"),
  s3_secret_access_key = Sys.getenv("SECRET"),
  s3_endpoint = "api-minio.ine.gob.cl",
  s3_region = "us-east-1",
  s3_url_style = "path",  # Automático si defines endpoint
  s3_use_ssl = TRUE,
  s3_uploader_thread_limit = 25 # Limita la concurrencia de subida
  
)

DBI::dbExecute(con, "SET http_retries = 5")               # Reintenta hasta 5 veces si hay error HTTP
DBI::dbExecute(con, "SET http_retry_wait_ms = 1000")      # Espera 1 segundo entre reintentos
DBI::dbExecute(con, "SET http_keep_alive = false")        # Previene que MinIO mantenga sockets colgados

# rutas
ruta_imputadas    <- "s3://desarrollo/ooee/trl/5_procesamiento/5_4_edicion_imputacion/espejo/"
ruta_tamano_movil <- "s3://desarrollo/ooee/trl/5_procesamiento/5_2_clasificacion_codificacion/espejo"
ruta_tamano_estatico <- "s3://desarrollo/ooee/trl/5_procesamiento/5_2_clasificacion_codificacion/espejo"
ruta_salida <- "s3://desarrollo/ooee/trl/5_procesamiento/5_5_nuevas_variables_unidades/espejo"

# obtencion bases mensuales imputadas
dt_imputado <- 
  duckdbfs::open_dataset(
    ruta_imputadas,
    format = "parquet",
    unify_schemas = TRUE
  )

# calculo de tamano movil mensual ----
DBI::dbExecute(
  con,
  "DROP VIEW IF EXISTS imputado_view"
)
DBI::dbExecute(
  con, 
  glue(
    "
  CREATE TEMPORARY VIEW imputado_view AS
  SELECT *,
    CAST(anno_devengamiento_remuneracion AS INTEGER) * 100 +
    CAST(mes_devengamiento_remuneracion  AS INTEGER) AS periodo_int,
    MAKE_DATE(
      CAST(anno_devengamiento_remuneracion AS INTEGER),
      CAST(mes_devengamiento_remuneracion  AS INTEGER),
      1
    ) AS fecha
  FROM ({dbplyr::remote_query(dt_imputado)})
"
  )
)

# tamano movil: n trabajadores unicos por empresa y mes
DBI::dbExecute(
  con, glue("
  COPY (
    SELECT
      id_ine_id_empresa,
      fecha,
      COUNT(DISTINCT id_ine_id_trabajador) AS n_trabajadores_movil
    FROM imputado_view
    GROUP BY id_ine_id_empresa, fecha
    ORDER BY id_ine_id_empresa, fecha
  ) TO '{ruta_tamano_movil}/pt_empresas.parquet'
  (FORMAT PARQUET, OVERWRITE_OR_IGNORE TRUE)
")
)

# vista tamano movil en duckdb
DBI::dbExecute(
  con, 
  "DROP VIEW IF EXISTS tamano_movil_view"
)
DBI::dbExecute(
  con, 
  glue(
    "
  CREATE TEMPORARY VIEW tamano_movil_view AS
  SELECT * FROM parquet_scan('{ruta_tamano_movil}/pt_empresas.parquet')"
  )
)

DBI::dbExecute(con, glue("
  COPY (
    WITH base AS (
      SELECT
        id_ine_id_empresa,
        SUM(n_trabajadores_movil) AS pt,
        COUNT(DISTINCT fecha)     AS t
      FROM tamano_movil_view
      GROUP BY id_ine_id_empresa
    )
    SELECT
      id_ine_id_empresa,
      pt,
      t,
      pt * 1.0 / t AS media_pt,
      CASE
        WHEN pt * 1.0 / t = 1   THEN 'unipersonal'
        WHEN pt * 1.0 / t < 5   THEN 'micro'
        WHEN pt * 1.0 / t < 50  THEN 'pequena'
        WHEN pt * 1.0 / t < 200 THEN 'mediana'
        ELSE 'grande'
      END AS tamano
    FROM base
  ) TO '{ruta_tamano_estatico}/tamano_estatico.parquet'
  (FORMAT PARQUET, OVERWRITE_OR_IGNORE TRUE)
"))

# media movil 12 meses en duckdb
# usando window function con frame de 12 meses hacia atras
DBI::dbExecute(
  con, 
  "DROP VIEW IF EXISTS media_movil_view"
)

DBI::dbExecute(con, glue("
  CREATE TEMPORARY VIEW media_movil_view AS
  WITH
  con_lag AS (
    SELECT
      id_ine_id_empresa,
      fecha,
      n_trabajadores_movil,
      LAG(fecha) OVER (
        PARTITION BY id_ine_id_empresa
        ORDER BY fecha
      ) AS fecha_anterior
    FROM tamano_movil_view
  ),
  con_grupos AS (
    SELECT
      id_ine_id_empresa,
      fecha,
      n_trabajadores_movil,
      SUM(
        CASE
          WHEN fecha_anterior IS NULL
            OR DATEDIFF('month', fecha_anterior, fecha) > 12
          THEN 1
          ELSE 0
        END
      ) OVER (
        PARTITION BY id_ine_id_empresa
        ORDER BY fecha
      ) AS grupo
    FROM con_lag
  ),
  con_media AS (
    SELECT
      a.id_ine_id_empresa,
      a.fecha,
      a.n_trabajadores_movil,
      a.grupo,
      AVG(b.n_trabajadores_movil) AS media_movil_12m,
      COUNT(b.fecha)              AS n_meses_utilizados
    FROM con_grupos a
    INNER JOIN con_grupos b
      ON  a.id_ine_id_empresa = b.id_ine_id_empresa
      AND a.grupo             = b.grupo
      AND b.fecha             > (a.fecha - INTERVAL 12 MONTHS)
      AND b.fecha            <= a.fecha
    GROUP BY
      a.id_ine_id_empresa,
      a.fecha,
      a.n_trabajadores_movil,
      a.grupo
  )
  SELECT
    id_ine_id_empresa,
    fecha,
    n_trabajadores_movil AS pt,
    media_movil_12m,
    n_meses_utilizados,
    CASE
      WHEN media_movil_12m < 5   THEN 'micro'
      WHEN media_movil_12m < 50  THEN 'pequena'
      WHEN media_movil_12m < 200 THEN 'mediana'
      ELSE 'grande'
    END AS tamano_empresa_movil
  FROM con_media
"))

# message("[OK] media movil calculada")

# periodos disponibles
periodos <-
  dplyr::tbl(
    con, "tamano_movil_view"
  ) %>%
  distinct(
    fecha
  ) %>%
  collect() %>%
  arrange(
    fecha
  ) %>%
  mutate(
    anno = year(fecha),
    mes  = month(fecha)
  )

# log de fallidos 
fallidos <- c()

# integrando tamano segun media movil y escribir por periodo ----

integrar_escribiri <- 
  function(anio, mes, reintentos = 3) {
    
    mes_fmt       <- formatC(mes, width = 2, flag = "0")
    periodo_label <- glue("{anio}-{mes_fmt}")
    ruta_archivo  <- glue("{ruta_salida}/anno={anio}/mes={mes_fmt}/trl_{anio}{mes_fmt}.parquet")
    fecha_periodo <- glue("{anio}-{mes_fmt}-01")
    
    intento   <- 1
    resultado <- FALSE
    
    while (intento <= reintentos) {
      
      intento_actual <- intento
      
      resultado <- tryCatch({
        
        DBI::dbExecute(con, glue("
        COPY (
          SELECT
            b.*,
            m.pt,
            m.media_movil_12m,
            m.tamano_empresa_movil
          FROM (
            SELECT * EXCLUDE (periodo_int, fecha)
            FROM imputado_view
            WHERE fecha = '{fecha_periodo}'
          ) b
          LEFT JOIN media_movil_view m
            ON  b.id_ine_id_empresa = m.id_ine_id_empresa
            AND m.fecha = '{fecha_periodo}'
        ) TO '{ruta_archivo}'
        (FORMAT PARQUET, OVERWRITE_OR_IGNORE TRUE)
      "))
        
        message(glue("[OK]     {periodo_label}"))
        TRUE
        
      }, error = function(e) {
        
        if (grepl("502|HTTP", e$message)) {
          message(glue("[502]    {periodo_label} - intento {intento_actual}/{reintentos} - esperando 5s..."))
        } else {
          message(glue("[ERROR]  {periodo_label} - intento {intento_actual}/{reintentos}: {e$message}"))
        }
        Sys.sleep(5)
        FALSE
      })
      
      if (resultado) break
      intento <- intento + 1
    }
    
    if (!resultado) {
      warning(glue("[FALLO]  {periodo_label} - agotados {reintentos} reintentos"))
      fallidos <<- c(fallidos, periodo_label)
    }
    
  }


# ejecucion
walk2(
  periodos$anno,
  periodos$mes,
  integrar_escribiri,
  reintentos = 3
)

# reporte final ----
if (length(fallidos) == 0) {
  message(
    "\nProceso completado sin errores en: ", ruta_salida
  )
} else {
  message(
    "\nProceso completado con ", length(fallidos), " periodo(s) fallido(s):"
  )
  walk(
    fallidos, ~ message("  - ", .x)
  )
  message(
    "\nPara reprocesar los fallidos:"
  )
  message(
    "  walk(
    fallidos, 
    ~ integrar_y_escribir(as.integer(substr(.x,1,4)), as.integer(substr(.x,6,7)))
    )"
  )
}

tictoc::toc()
