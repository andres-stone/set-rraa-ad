
# Script 05 — Bases completas: enriquecimiento demográfico y económico
# Produce: panel mensual final con todas las variables
# Este es el único punto del pipeline donde se agregan:
#   - Demografía REP completa (sexo, nacionalidad, fecha_nac, etc.)
#   - edad_meses (calculada aquí, una sola vez)
#   - Actividad económica (MME: CIIU, comuna)
#   - Flag de empleo público
#   - Tamaño estático
#   - Imputación de nacionalidad y región
#   - Tramos etarios (7 grupos decenales y 13 grupos quinquenales)

rm(list = ls())
options(scipen = 999, digits = 4)
gc()

devtools::load_all(here::here())

# conexion ----
con <- setup_conexion()

# rutas ----
ruta_final        <- "s3://desarrollo/ooee/trl/5_procesamiento/5_5_nuevas_variables_unidades/espejo"
ruta_personas     <- "s3://desarrollo/ooee/trl/5_procesamiento/5_2_clasificacion_codificacion/espejo/pqt_personas_nuevo.parquet"
ruta_mme          <- "s3://activos/infraestructura/mme/pseudonimizado/id_ine/marco_maestro_empresas_2024.parquet"
ruta_ep           <- "s3://desarrollo/ooee/trl/20260414_sector_publico.parquet"
ruta_uni          <- "s3://desarrollo/ooee/trl/5_procesamiento/5_2_clasificacion_codificacion/espejo/tamano_estatico.parquet"
ruta_nacionalidad <- "s3://desarrollo/ooee/trl/5_procesamiento/5_1_integracion_datos/202605/nacionalidad_suseso/nacionalidad_anual.parquet"
ruta_region       <- "s3://desarrollo/ooee/trl/5_procesamiento/5_1_integracion_datos/202605/region_suseso/region_para_imputar.parquet"
ruta_salida       <- "s3://desarrollo/ooee/trl/5_procesamiento/5_8_finalizacion_datos/es/202512"

# obtencion dataset base ----
dt_suseso <- open_parquet(ruta_final, unify_schemas = TRUE)

DBI::dbExecute(con, "DROP VIEW IF EXISTS suseso_view")
DBI::dbExecute(con, glue::glue(
  "CREATE TEMPORARY VIEW suseso_view AS
   SELECT *,
     MAKE_DATE(
       CAST(anno_devengamiento_remuneracion AS INTEGER),
       CAST(mes_devengamiento_remuneracion  AS INTEGER),
       1
     ) AS fecha
   FROM ({dbplyr::remote_query(dt_suseso)})"
))

# vistas auxiliares ----
crear_vista_rep(con, ruta_personas)
crear_vista_mme(con, ruta_mme)
crear_vista_ep(con, ruta_ep)
crear_vista_tamano_estatico(con, ruta_uni)
crear_vista_nacionalidad(con, ruta_nacionalidad)
crear_vista_region(con, ruta_region)

# vista enriquecida final ----
crear_vista_enriquecida(con)

# períodos disponibles ----
periodos <- DBI::dbGetQuery(con,
  "SELECT DISTINCT
     fecha,
     anno_devengamiento_remuneracion AS anno,
     mes_devengamiento_remuneracion  AS mes
   FROM enriquecida_view
   ORDER BY fecha"
)

# escritura por período ----
tictoc::tic()

fn_sql <- function(anio, mes) {
  mes_fmt       <- formatC(mes, width = 2, flag = "0")
  fecha_periodo <- glue::glue("{anio}-{mes_fmt}-01")
  glue::glue(
    "SELECT * EXCLUDE (fecha)
     FROM enriquecida_view
     WHERE fecha = '{fecha_periodo}'"
  )
}

fn_ruta <- function(anio, mes) {
  mes_fmt <- formatC(mes, width = 2, flag = "0")
  glue::glue("{ruta_salida}/anno={anio}/mes={mes_fmt}/trl_{anio}{mes_fmt}.parquet")
}

fallidos <- escribir_periodos(periodos, fn_sql, fn_ruta, con, reintentos = 3)

tictoc::toc()
reporte_fallidos(fallidos, ruta_salida)
