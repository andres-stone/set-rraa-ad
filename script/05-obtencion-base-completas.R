
# procesamiento 
# bases mensuales flujos laborales

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
ruta_final  <- "s3://desarrollo/ooee/trl/5_procesamiento/5_5_nuevas_variables_unidades/espejo"
ruta_mme    <- "s3://activos/infraestructura/mme/pseudonimizado/id_ine/marco_maestro_empresas_2024.parquet"
ruta_ep     <- "s3://desarrollo/ooee/trl/20260414_sector_publico.parquet"
ruta_uni    <- "s3://desarrollo/ooee/trl/5_procesamiento/5_2_clasificacion_codificacion/espejo/tamano_estatico.parquet"

# nuevas rutas (nacionalidad - region)
ruta_nacionalidad <- "s3://desarrollo/ooee/trl/5_procesamiento/5_1_integracion_datos/202605/nacionalidad_suseso/nacionalidad_anual.parquet"
ruta_region <- "s3://desarrollo/ooee/trl/5_procesamiento/5_1_integracion_datos/202605/region_suseso/region_para_imputar.parquet"

# ruta salida
ruta_salida <- "s3://desarrollo/ooee/trl/5_procesamiento/5_8_finalizacion_datos/es/202512"

# obtencion datasets ----
# base suseso final
dt_suseso <-
  duckdbfs::open_dataset(
    ruta_final,
    format = "parquet",
    unify_schemas = TRUE
  )

DBI::dbExecute(con, "DROP VIEW IF EXISTS suseso_view")
DBI::dbExecute(
  con,
  glue(
    "CREATE TEMPORARY VIEW suseso_view AS
     SELECT *,
       MAKE_DATE(
         CAST(anno_devengamiento_remuneracion AS INTEGER),
         CAST(mes_devengamiento_remuneracion  AS INTEGER),
         1
       ) AS fecha
     FROM ({dbplyr::remote_query(dt_suseso)})"
  )
)

# # rue (marco maestro empresas)
# DBI::dbExecute(con, "DROP VIEW IF EXISTS mme_view")
# DBI::dbExecute(
#   con,
#   glue(
#     "CREATE TEMPORARY VIEW mme_view AS
#      SELECT
#        id_ine_rut,
#        seccion_ciiu4cl,
#        division_ciiu4cl,
#        comuna_cut
#      FROM parquet_scan('{ruta_mme}')"
#   )
# )

# rue (marco maestro empresas)
DBI::dbExecute(con, "DROP VIEW IF EXISTS mme_view")
DBI::dbExecute(
  con,
  glue(
    "CREATE TEMPORARY VIEW mme_view AS
     SELECT
       id_ine_rut,
       seccion_ciiu_4cl,
       division_ciiu_4cl,
       comuna_cut
     FROM parquet_scan('{ruta_mme}')"
  )
)

# empleo publico
DBI::dbExecute(con, "DROP VIEW IF EXISTS ep_view")
DBI::dbExecute(
  con,
  glue(
    "CREATE TEMPORARY VIEW ep_view AS
     SELECT
       CAST(id_ine AS DOUBLE) AS id_ine,
       razon_social_unidad_legal,
       seccion_ciiu4cl_prin,
       sector_institucional,
       subsector_institucional,
       1 AS ep
     FROM parquet_scan('{ruta_ep}')"
  )
)

# unipersonales / tamano estatico
DBI::dbExecute(con, "DROP VIEW IF EXISTS uni_view")
DBI::dbExecute(
  con,
  glue(
    "CREATE TEMPORARY VIEW uni_view AS
     SELECT
       id_ine_id_empresa,
       tamano
     FROM parquet_scan('{ruta_uni}')"
  )
)

# nacionalidad
DBI::dbExecute(con, "DROP VIEW IF EXISTS nc_view")
DBI::dbExecute(
  con,
  glue(
    "CREATE TEMPORARY VIEW nc_view AS
     SELECT
       id_ine_id_trabajador,
       moda
     FROM parquet_scan('{ruta_nacionalidad}')"
  )
)

# region
DBI::dbExecute(con, "DROP VIEW IF EXISTS region_view")
DBI::dbExecute(
  con,
  glue(
    "CREATE TEMPORARY VIEW region_view AS
     SELECT
       id_ine_id_empresa,
       region_trabajador
     FROM parquet_scan('{ruta_region}')"
  )
)

# vista completa con tramo de edad (joins en duckdb) ----
DBI::dbExecute(con, "DROP VIEW IF EXISTS enriquecida_view")
DBI::dbExecute(
  con,
  "CREATE TEMPORARY VIEW enriquecida_view AS
   SELECT
     s.*,
     
     r.seccion_ciiu_4cl,
     r.division_ciiu_4cl,
     r.comuna_cut,
     
     e.razon_social_unidad_legal,
     e.seccion_ciiu4cl_prin,
     e.sector_institucional,
     e.subsector_institucional,
     
     COALESCE(e.ep, 0) AS ep,
     
     u.tamano,
     
     nc.moda AS nc_moda,
     
     rg.region_trabajador,
     
     -- nacionalidad depurada
     CASE
       WHEN s.nacionalidad IN (88, 99) THEN NULL
       ELSE CAST(s.nacionalidad AS INTEGER)
     END AS nacionalidad_depurada,
     
     -- codigo_region depurado
     CASE
       WHEN s.codigo_region IN ('17.0', '88.0', '99.0') THEN NULL
       ELSE CAST(s.codigo_region AS INTEGER)
     END AS codigo_region_depurada,
     
     -- nacionalidad imputada
     CASE
       WHEN (
         CASE
           WHEN s.nacionalidad IN (88, 99) THEN NULL
           ELSE CAST(s.nacionalidad AS INTEGER)
         END
       ) IS NOT NULL
       THEN (
         CASE
           WHEN s.nacionalidad IN (88, 99) THEN NULL
           ELSE CAST(s.nacionalidad AS INTEGER)
         END
       )
       
       WHEN nc.moda IN (88, 99) THEN NULL
       
       ELSE nc.moda
     END AS nacionalidad_final,
     
     -- region imputada
     CASE
       WHEN (
         CASE
           WHEN s.codigo_region IN ('17.0', '88.0', '99.0') THEN NULL
           ELSE CAST(s.codigo_region AS INTEGER)
         END
       ) IS NOT NULL
       THEN (
         CASE
           WHEN s.codigo_region IN ('17.0', '88.0', '99.0') THEN NULL
           ELSE CAST(s.codigo_region AS INTEGER)
         END
       )
       
       WHEN rg.region_trabajador IN (88, 99) THEN NULL
       
       ELSE rg.region_trabajador
     END AS region_final,
     
     -- tramo etario
     CASE 
       WHEN edad_meses < 180 THEN 0
       WHEN edad_meses >= 180 AND edad_meses < 300 THEN 1
       WHEN edad_meses >= 300 AND edad_meses < 420 THEN 2
       WHEN edad_meses >= 420 AND edad_meses < 540 THEN 3
       WHEN edad_meses >= 540 AND edad_meses < 660 THEN 4
       WHEN edad_meses >= 660 AND edad_meses < 780 THEN 5
       WHEN edad_meses >= 780 THEN 6
       ELSE NULL
     END AS tramo_edad,
     
     -- tramo etario 2
     CASE
        WHEN edad_meses < 180 THEN 0                    -- [0-14]
        WHEN edad_meses >= 180 AND edad_meses < 240 THEN 1  -- [15-19]
        WHEN edad_meses >= 240 AND edad_meses < 300 THEN 2  -- [20-24]
        WHEN edad_meses >= 300 AND edad_meses < 360 THEN 3  -- [25-29]
        WHEN edad_meses >= 360 AND edad_meses < 420 THEN 4  -- [30-34]
        WHEN edad_meses >= 420 AND edad_meses < 480 THEN 5  -- [35-39]
        WHEN edad_meses >= 480 AND edad_meses < 540 THEN 6  -- [40-44]
        WHEN edad_meses >= 540 AND edad_meses < 600 THEN 7  -- [45-49]
        WHEN edad_meses >= 600 AND edad_meses < 660 THEN 8  -- [50-54]
        WHEN edad_meses >= 660 AND edad_meses < 720 THEN 9  -- [55-59]
        WHEN edad_meses >= 720 AND edad_meses < 780 THEN 10 -- [60-64]
        WHEN edad_meses >= 780 AND edad_meses < 840 THEN 11 -- [65-69]
        WHEN edad_meses >= 840 THEN 12                      -- [70+]
        ELSE NULL
     END AS tramo_edad_2
     
   FROM suseso_view s
   
   LEFT JOIN mme_view r
     ON s.id_ine_id_empresa = r.id_ine_rut
   
   LEFT JOIN ep_view e
     ON s.id_ine_id_empresa = e.id_ine
   
   LEFT JOIN uni_view u
     ON s.id_ine_id_empresa = u.id_ine_id_empresa
     
   LEFT JOIN nc_view nc
     ON s.id_ine_id_trabajador = nc.id_ine_id_trabajador
     
   LEFT JOIN region_view rg
     ON s.id_ine_id_empresa = rg.id_ine_id_empresa
   "
)

# DBI::dbExecute(
#   con,
#   "CREATE TEMPORARY VIEW enriquecida_view AS
#    SELECT
#      s.*,
#      r.seccion_ciiu4cl,
#      r.division_ciiu4cl,
#      r.comuna_cut,
#      e.razon_social_unidad_legal,
#      e.seccion_ciiu4cl_prin,
#      e.sector_institucional,
#      e.subsector_institucional,
#      COALESCE(e.ep, 0) AS ep,
#      u.tamano,
#      nc.moda AS nc_moda,
#      rg.region_trabajador,
#      
#      -- variables depuradas
#      CASE
#        WHEN s.nacionalidad IN (88, 99) THEN NULL
#        ELSE CAST(s.nacionalidad AS INTEGER)
#      END AS nacionalidad_depurada,
#      
#      CASE
#        WHEN s.codigo_region IN ('17.0', '88.0', '99.0') THEN NULL
#        ELSE CAST(s.codigo_region AS INTEGER)
#      END AS codigo_region_depurada,
#      
#      -- imputacion nacionalidad
#      CASE
#        WHEN (
#          CASE
#            WHEN s.nacionalidad IN (88, 99) THEN NULL
#            ELSE CAST(s.nacionalidad AS INTEGER)
#          END
#        ) IS NOT NULL
#        THEN (
#          CASE
#            WHEN s.nacionalidad IN (88, 99) THEN NULL
#            ELSE CAST(s.nacionalidad AS INTEGER)
#          END
#        )
#        ELSE nc.moda
#      END AS nacionalidad_final,
#      
#      -- imputacion region
#      CASE
#        WHEN (
#          CASE
#            WHEN s.codigo_region IN ('17.0', '88.0', '99.0') THEN NULL
#            ELSE CAST(s.codigo_region AS INTEGER)
#          END
#        ) IS NOT NULL
#        THEN (
#          CASE
#            WHEN s.codigo_region IN ('17.0', '88.0', '99.0') THEN NULL
#            ELSE CAST(s.codigo_region AS INTEGER)
#          END
#        )
#        ELSE rg.region_trabajador
#      END AS region_final,
#      
#      CASE 
#        WHEN edad_meses < 180 THEN 0
#        WHEN edad_meses >= 180 AND edad_meses < 300 THEN 1
#        WHEN edad_meses >= 300 AND edad_meses < 420 THEN 2
#        WHEN edad_meses >= 420 AND edad_meses < 540 THEN 3
#        WHEN edad_meses >= 540 AND edad_meses < 660 THEN 4
#        WHEN edad_meses >= 660 AND edad_meses < 780 THEN 5
#        WHEN edad_meses >= 780 THEN 6
#        ELSE NULL
#      END AS tramo_edad
#      
#    FROM suseso_view s
#    
#    LEFT JOIN mme_view r
#      ON s.id_ine_id_empresa = r.id_ine_rut
#    
#    LEFT JOIN ep_view e
#      ON s.id_ine_id_empresa = e.id_ine
#    
#    LEFT JOIN uni_view u
#      ON s.id_ine_id_empresa = u.id_ine_id_empresa
#      
#    LEFT JOIN nc_view nc
#      ON s.id_ine_id_trabajador = nc.id_ine_id_trabajador
#      
#    LEFT JOIN region_view rg
#      ON s.id_ine_id_empresa = rg.id_ine_id_empresa
#    "
# )

# periodos disponibles ----
periodos <-
  DBI::dbGetQuery(
    con,
    "SELECT DISTINCT
       fecha,
       anno_devengamiento_remuneracion AS anno,
       mes_devengamiento_remuneracion  AS mes
     FROM enriquecida_view
     ORDER BY fecha"
  )

# log de fallidos
fallidos <- c()

# funcion: escribir un periodo enriquecido ----
integrar_escribir <-
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
        
        DBI::dbExecute(
          con,
          glue(
            "COPY (
               SELECT * EXCLUDE (fecha)
               FROM enriquecida_view
               WHERE fecha = '{fecha_periodo}'
             ) TO '{ruta_archivo}'
             (FORMAT PARQUET, OVERWRITE_OR_IGNORE TRUE)"
          )
        )
        
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

tictoc::tic()
# ejecucion por periodo
walk2(
  periodos$anno,
  periodos$mes,
  integrar_escribir,
  reintentos = 3
)
tictoc::toc()

# reporte final ----
if (length(fallidos) == 0) {
  message("\nProceso completado sin errores en: ", ruta_salida)
} else {
  message("\nProceso completado con ", length(fallidos), " periodo(s) fallido(s):")
  walk(fallidos, ~ message("  - ", .x))
  message("\nPara reprocesar los fallidos:")
  message(
    "  walk2(
    as.integer(substr(fallidos, 1, 4)),
    as.integer(substr(fallidos, 6, 7)),
    integrar_escribir
  )"
  )
}
