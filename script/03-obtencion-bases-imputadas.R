
# obtencion bases editadas e imputadas 
# bases mensuales flujos laborales
# 5.4 edicion imputacion: edicion a tope imponible e imputacion de lagunas
# 2530.667 sec

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
# insumos
ruta_preprocesadas <- "s3://desarrollo/ooee/trl/5_procesamiento/5_1_integracion_datos/espejo/"
# output
ruta_imputadas <- "s3://desarrollo/ooee/trl/5_procesamiento/5_4_edicion_imputacion/espejo"

# obtencion base mensual preprocesada
dt_suseso <-
  duckdbfs::open_dataset(
    ruta_preprocesadas,
    format = "parquet"
  )

# truncando a topes imponibles ----
# topes imponibles
valor_uf <-
  list(
    valor_uf_2016 = c(25629.09, 25629.09, 25721.82, 25814.55, 25910.25, 25995.56, 26053.81, 26145.01, 26210.79, 26224.30, 26263.20, 26315.28),
    valor_uf_2017 = c(26348.83, 26316.51, 26396.79, 26473.65, 26564.95, 26632.70, 26665.98, 26593.89, 26605.81, 26658.56, 26633.18, 26736.45),
    valor_uf_2018 = c(26799.01, 26825.81, 26928.49, 26966.89, 27006.43, 27080.94, 27161.48, 27203.36, 27291.08, 27359.27, 27434.76, 27536.46),
    valor_uf_2019 = c(27565.79, 27545.34, 27557.89, 27565.76, 27666.77, 27765.23, 27908.86, 27953.42, 27994.89, 28050.40, 28065.35, 28229.83),
    valor_uf_2020 = c(28310.86, 28339.17, 28469.54, 28601.15, 28693.59, 28716.52, 28695.46, 28666.51, 28680.37, 28708.80, 28844.20, 29036.92),
    valor_uf_2021 = c(29069.39, 29126.55, 29294.68, 29396.67, 29498.06, 29617.07, 29712.80, 29758.60, 29942.78, 30092.38, 30392.22, 30776.05),
    valor_uf_2022 = c(30996.73, 31220.68, 31552.64, 31730.80, 32196.69, 32694.20, 33099.99, 33426.92, 33851.69, 34271.85, 34610.35, 34817.58),
    valor_uf_2023 = c(35122.26, 35290.91, 35519.79, 35574.33, 35851.62, 36036.37, 36090.68, 36046.72, 36134.97, 36198.73, 36396.26, 36568.74),
    valor_uf_2024 = c(36797.64, 36727.10, 36865.37, 37100.68, 37266.94, 37444.94, 37575.61, 37577.74, 37762.97, 37914.20, 37972.65, 38260.61),
    valor_uf_2025 = c(39485.65, 39485.65, 39485.65, 39485.65, 39485.65, 39485.65, 39485.65, 39485.65, 39485.65, 39485.65, 39485.65, 39485.65)
  )

topes_imponibles <-
  tibble::tibble(
    anno_devengamiento_remuneracion = 2016:2025,
    tope_imponible_uf = c(
      74.3, 75.7, 78.3, 79.3, 80.2,
      81.7, 81.6, 81.6, 84.3, 87.8
    )
  ) %>%
  tidyr::expand_grid(mes_devengamiento_remuneracion = 1:12) %>%
  arrange(anno_devengamiento_remuneracion, mes_devengamiento_remuneracion) %>%
  mutate(
    valor_uf            = unlist(valor_uf),
    tope_imponible_peso = as.integer(trunc(valor_uf * tope_imponible_uf)),
    fecha               = as.Date(sprintf("%d-%02d-01", anno_devengamiento_remuneracion, mes_devengamiento_remuneracion))
  ) %>%
  select(
    anno_devengamiento_remuneracion, mes_devengamiento_remuneracion, 
    tope_imponible_peso
  )

# topes imponibles en duckdb
DBI::dbExecute(
  con, 
  "DROP TABLE IF EXISTS topes_imponibles"
)
DBI::dbWriteTable(
  con, 
  "topes_imponibles", 
  topes_imponibles
)

tbl_topes <- 
  dplyr::tbl(
    con, "topes_imponibles"
  )

# truncar a tope imponible
dt_truncada <-
  dt_suseso %>%
  left_join(
    tbl_topes,
    by = c(
      "anno_devengamiento_remuneracion",# = "anno",
      "mes_devengamiento_remuneracion"  #= "mes"
    )
  ) %>%
  mutate(
    monto_remuneracion = if_else(
      monto_remuneracion > tope_imponible_peso,
      as.double(tope_imponible_peso),
      monto_remuneracion
    )
  ) %>%
  select(
    -tope_imponible_peso
  )

# vista base en duckdb 
DBI::dbExecute(
  con, "DROP VIEW IF EXISTS base_view"
)
DBI::dbExecute(con, glue("
  CREATE TEMPORARY VIEW base_view AS
  SELECT *,
    CAST(anno_devengamiento_remuneracion AS INTEGER) * 100 +
    CAST(mes_devengamiento_remuneracion  AS INTEGER) AS periodo_int
  FROM ({dbplyr::remote_query(dt_suseso)})
"))

# periodos disponibles
periodos <-
  dt_truncada %>%
  distinct(
    anno_devengamiento_remuneracion,
    mes_devengamiento_remuneracion
  ) %>%
  collect() %>%
  arrange(
    anno_devengamiento_remuneracion,
    mes_devengamiento_remuneracion
  ) %>%
  mutate(
    periodo_int = 
      anno_devengamiento_remuneracion * 100 + mes_devengamiento_remuneracion
  )

dt_truncada <-
  dt_truncada %>% 
  select(
    -c(
      tipo_trabajador, anno, mes
    )
  )

# log de fallidos
fallidos <- c()

# imputacion ----

# funcion para imputar y escribir segun periodo
# agregando tipo_trabajador
imputar_y_escribir <- 
  function(anio, mes, reintentos = 3) {
    mes_fmt        <- formatC(mes, width = 2, flag = "0")
    periodo_label  <- glue("{anio}-{mes_fmt}")
    ruta_archivo   <- glue("{ruta_imputadas}/anno={anio}/mes={mes_fmt}/trl_{anio}{mes_fmt}.parquet")
    periodo_actual <- anio * 100 + mes
    
    idx        <- which(periodos$periodo_int == periodo_actual)
    es_primero <- idx == 1
    es_ultimo  <- idx == nrow(periodos)
    
    periodo_ant <- if (!es_primero) periodos$periodo_int[idx - 1] else NA_integer_
    periodo_sig <- if (!es_ultimo)  periodos$periodo_int[idx + 1] else NA_integer_
    
    intento   <- 1
    resultado <- FALSE
    
    while (intento <= reintentos) {
      
      intento_actual <- intento
      
      resultado <- tryCatch({
        
        # CASO BORDE
        if (es_primero || es_ultimo) {
          
          DBI::dbExecute(con, glue("
          COPY (
            SELECT * EXCLUDE (periodo_int)
            FROM base_view
            WHERE periodo_int = {periodo_actual}
          ) TO '{ruta_archivo}'
          (FORMAT PARQUET, OVERWRITE_OR_IGNORE TRUE)
        "))
          
        } else {
          
          DBI::dbExecute(con, glue("
          COPY (
            
            WITH
            t AS (
              SELECT * FROM base_view
              WHERE periodo_int = {periodo_actual}
            ),
            
            t_ant AS (
              SELECT
                id_ine_id_trabajador,
                id_ine_id_empresa,
                monto_remuneracion AS rem_ant,
                n_dias_trabajados  AS dias_ant,
                sexo,
                nacionalidad,
                fecha_nac,
                fecha_def_cor_rc,
                codigo_comuna,
                codigo_region
              FROM base_view
              WHERE periodo_int = {periodo_ant}
            ),
            
            t_sig AS (
              SELECT
                id_ine_id_trabajador,
                id_ine_id_empresa,
                monto_remuneracion AS rem_sig,
                n_dias_trabajados  AS dias_sig
              FROM base_view
              WHERE periodo_int = {periodo_sig}
            ),
            
            ausentes AS (
              SELECT
                t_ant.id_ine_id_trabajador,
                t_ant.id_ine_id_empresa,
                t_ant.rem_ant,
                t_ant.dias_ant,
                t_sig.rem_sig,
                t_sig.dias_sig,
                t_ant.sexo,
                t_ant.nacionalidad,
                t_ant.fecha_nac,
                t_ant.fecha_def_cor_rc,
                t_ant.codigo_comuna,
                t_ant.codigo_region
              FROM t_ant
              INNER JOIN t_sig
                ON  t_ant.id_ine_id_trabajador = t_sig.id_ine_id_trabajador
                AND t_ant.id_ine_id_empresa     = t_sig.id_ine_id_empresa
              WHERE NOT EXISTS (
                SELECT 1 FROM t
                WHERE t.id_ine_id_trabajador = t_ant.id_ine_id_trabajador
                  AND t.id_ine_id_empresa    = t_ant.id_ine_id_empresa
              )
            ),
            
            imputados AS (
              SELECT
                id_ine_id_trabajador,
                id_ine_id_empresa,
                
                CASE
                  WHEN COALESCE(rem_ant, 0) > 0 AND COALESCE(rem_sig, 0) > 0
                    THEN (rem_ant + rem_sig) / 2.0
                  ELSE GREATEST(COALESCE(rem_ant, 0), COALESCE(rem_sig, 0))
                END AS monto_remuneracion,
                
                CASE
                  WHEN COALESCE(dias_ant, 0) > 0 AND COALESCE(dias_sig, 0) > 0
                    THEN CAST((dias_ant + dias_sig) / 2.0 AS INTEGER)
                  ELSE GREATEST(COALESCE(dias_ant, 0), COALESCE(dias_sig, 0))
                END AS n_dias_trabajados,
                
                {anio} AS anno_devengamiento_remuneracion,
                {mes}  AS mes_devengamiento_remuneracion,
                
                make_date({anio}, {mes}, 1) AS refdate,
                
                sexo,
                nacionalidad,
                fecha_nac,
                fecha_def_cor_rc,
                codigo_comuna,
                codigo_region,
                
                -- edad en el periodo imputado (t)
                (
                  ({anio} * 12 + {mes}) -
                  (CAST(strftime('%Y', fecha_nac) AS INTEGER) * 12 +
                   CAST(strftime('%m', fecha_nac) AS INTEGER))
                ) AS edad_meses,
                
                -- fallecimiento en t
                (
                  ({anio} * 12 + {mes}) -
                  (CAST(strftime('%Y', fecha_def_cor_rc) AS INTEGER) * 12 +
                   CAST(strftime('%m', fecha_def_cor_rc) AS INTEGER))
                ) AS meses_fallecimiento
                
              FROM ausentes
            )
            
            SELECT
              id_ine_id_trabajador,
              id_ine_id_empresa,
              monto_remuneracion,
              n_dias_trabajados,
              anno_devengamiento_remuneracion,
              mes_devengamiento_remuneracion,
              sexo,
              nacionalidad,
              fecha_nac,
              fecha_def_cor_rc,
              codigo_comuna,
              codigo_region,
              refdate,
              edad_meses
            FROM t
            
            UNION ALL
            
            SELECT
              id_ine_id_trabajador,
              id_ine_id_empresa,
              monto_remuneracion,
              n_dias_trabajados,
              anno_devengamiento_remuneracion,
              mes_devengamiento_remuneracion,
              sexo,
              nacionalidad,
              fecha_nac,
              fecha_def_cor_rc,
              codigo_comuna,
              codigo_region,
              refdate,
              edad_meses
            FROM imputados
            
          ) TO '{ruta_archivo}'
          (FORMAT PARQUET, OVERWRITE_OR_IGNORE TRUE)
        "))
          
        }
        
        message(glue("[OK]     {periodo_label}"))
        TRUE
        
      }, error = function(e) {
        
        if (grepl("502|HTTP", e$message)) {
          message(glue("[502]    {periodo_label} - intento {intento_actual}/{reintentos}"))
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

tictoc::tic()

# ejecucion
walk2(
  periodos$anno_devengamiento_remuneracion,
  periodos$mes_devengamiento_remuneracion,
  imputar_y_escribir,
  reintentos = 3
)

tictoc::toc()

# reporte final ----
if (length(fallidos) == 0) {
  message(
    "\nProceso completado sin errores en: ", ruta_imputadas
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
    ~ imputar_y_escribir(as.integer(substr(.x,1,4)), as.integer(substr(.x,6,7)))
    )"
  )
}

