
# Script 03 — Edición (tope imponible) e imputación de lagunas
# Produce: bases mensuales imputadas
# Columnas: id_ine_id_trabajador, id_ine_id_empresa,
#            anno_devengamiento_remuneracion, mes_devengamiento_remuneracion,
#            monto_remuneracion (topado), n_dias_trabajados
# La imputación solo opera sobre variables laborales; la demografía se agrega en Script 05

rm(list = ls())
options(scipen = 999, digits = 4)
gc()

devtools::load_all(here::here())

# conexion ----
con <- setup_conexion()

# rutas ----
ruta_preprocesadas <- "s3://desarrollo/ooee/trl/5_procesamiento/5_1_integracion_datos/espejo/"
ruta_imputadas     <- "s3://desarrollo/ooee/trl/5_procesamiento/5_4_edicion_imputacion/espejo"

# obtencion dataset ----
dt_suseso <- open_parquet(ruta_preprocesadas)

# tope imponible ----
tbl_topes    <- registrar_topes_en_duckdb(con)
dt_truncada  <- truncar_a_tope(dt_suseso, tbl_topes)

# vista base para imputación (usa dt_truncada con tope ya aplicado)
crear_base_view(con, dt_truncada)

# períodos disponibles ----
periodos <- get_periodos(dt_suseso)

# imputación y escritura ----
tictoc::tic()
fallidos <- imputar_lagunas(con, periodos, ruta_imputadas, reintentos = 3)
tictoc::toc()

reporte_fallidos(fallidos, ruta_imputadas)
