
# Script 04 — Cálculo de tamaño de empresa e integración al panel
# Produce:
#   - pt_empresas.parquet        (plantilla mensual por empresa)
#   - tamano_estatico.parquet    (tamaño histórico promedio)
#   - bases con tamaño móvil     (panel + pt, media_movil_12m, tamano_empresa_movil)

rm(list = ls())
options(scipen = 999, digits = 4)
gc()

devtools::load_all(here::here())

# conexion ----
con <- setup_conexion()

# rutas ----
ruta_imputadas       <- "s3://desarrollo/ooee/trl/5_procesamiento/5_4_edicion_imputacion/espejo/"
ruta_tamano_base     <- "s3://desarrollo/ooee/trl/5_procesamiento/5_2_clasificacion_codificacion/espejo"
ruta_salida          <- "s3://desarrollo/ooee/trl/5_procesamiento/5_5_nuevas_variables_unidades/espejo"

# obtencion dataset ----
dt_imputado <- open_parquet(ruta_imputadas, unify_schemas = TRUE)

# vista con periodo_int y fecha ----
DBI::dbExecute(con, "DROP VIEW IF EXISTS imputado_view")
DBI::dbExecute(con, glue::glue(
  "CREATE TEMPORARY VIEW imputado_view AS
   SELECT *,
     CAST(anno_devengamiento_remuneracion AS INTEGER) * 100 +
     CAST(mes_devengamiento_remuneracion  AS INTEGER) AS periodo_int,
     MAKE_DATE(
       CAST(anno_devengamiento_remuneracion AS INTEGER),
       CAST(mes_devengamiento_remuneracion  AS INTEGER),
       1
     ) AS fecha
   FROM ({dbplyr::remote_query(dt_imputado)})"
))

# plantilla mensual por empresa ----
calcular_plantilla_mensual(con, ruta_tamano_base)

# vista de plantilla en duckdb ----
DBI::dbExecute(con, "DROP VIEW IF EXISTS tamano_movil_view")
DBI::dbExecute(con, glue::glue(
  "CREATE TEMPORARY VIEW tamano_movil_view AS
   SELECT * FROM parquet_scan('{ruta_tamano_base}/pt_empresas.parquet')"
))

# tamaño estático ----
calcular_tamano_estatico(con, ruta_tamano_base)

# media móvil 12 meses ----
crear_media_movil_view(con)

# períodos disponibles ----
periodos <- dplyr::tbl(con, "tamano_movil_view") %>%
  dplyr::distinct(fecha) %>%
  dplyr::collect() %>%
  dplyr::arrange(fecha) %>%
  dplyr::mutate(
    anno = lubridate::year(fecha),
    mes  = lubridate::month(fecha)
  )

# escritura por período con tamaño móvil integrado ----
tictoc::tic()

fn_sql <- function(anio, mes) {
  mes_fmt       <- formatC(mes, width = 2, flag = "0")
  fecha_periodo <- glue::glue("{anio}-{mes_fmt}-01")
  glue::glue(
    "SELECT
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
       AND m.fecha = '{fecha_periodo}'"
  )
}

fn_ruta <- function(anio, mes) {
  mes_fmt <- formatC(mes, width = 2, flag = "0")
  glue::glue("{ruta_salida}/anno={anio}/mes={mes_fmt}/trl_{anio}{mes_fmt}.parquet")
}

fallidos <- escribir_periodos(periodos, fn_sql, fn_ruta, con, reintentos = 3)

tictoc::toc()
reporte_fallidos(fallidos, ruta_salida)
