
# Script 02 — Preprocesamiento e integración SUSESO
# Produce: bases mensuales preprocesadas (variables laborales + filtros de edad/fallecimiento)
# Columnas: id_ine_id_trabajador, id_ine_id_empresa,
#            anno_devengamiento_remuneracion, mes_devengamiento_remuneracion,
#            monto_remuneracion, n_dias_trabajados
# NO incluye demografía REP (sexo, nacionalidad, etc.) — se agrega en Script 05

rm(list = ls())
options(scipen = 999, digits = 4)
gc()

devtools::load_all(here::here())

# conexion ----
con <- setup_conexion()

# rutas ----
ruta_suseso   <- "s3://activos/rraa_oae/suseso/cotizaciones_trabajadores/pseudonimizado/"
ruta_rue      <- "s3://desarrollo/rue/compartido/seit"
ruta_personas <- "s3://desarrollo/ooee/trl/5_procesamiento/5_2_clasificacion_codificacion/espejo/pqt_personas_nuevo.parquet"
ruta_salida   <- "s3://desarrollo/ooee/trl/5_procesamiento/5_1_integracion_datos/espejo"

# obtencion datasets ----
dt_suseso  <- open_parquet(ruta_suseso)
dt_rue     <- open_parquet(ruta_rue)
dt_personas <- open_parquet(ruta_personas, recursive = FALSE)

# preprocesamiento ----
dt_preprocesado <- dt_suseso %>%
  dplyr::select(
    -c(anno, mes, nacionalidad, tipo_canal_cotizacion, estado_cotizacion, fecha_pago)
  ) %>%
  # solo asalariados dependientes desde nov-2016
  filtrar_tipo_trabajador(tipo = 1) %>%
  dplyr::filter(
    anno_devengamiento_remuneracion > 2016 |
      (anno_devengamiento_remuneracion == 2016 & mes_devengamiento_remuneracion >= 11)
  ) %>%
  # join mínimo REP: solo fecha_nac y fecha_def_cor_rc para filtrar
  join_fechas_personas(dt_personas, con) %>%
  # filtros de elegibilidad
  filtrar_edad(min_anios = 15, max_anios = 90) %>%
  filtrar_fallecidos() %>%
  # quitar columnas de fecha (ya no necesarias — no se propagan)
  dplyr::select(-c(fecha_nac, fecha_def_cor_rc)) %>%
  # consolidar puestos de trabajo
  consolidar_puestos_trabajo() %>%
  # excluir tipo 6 predichos del RUE
  dplyr::anti_join(
    dplyr::select(dt_rue, -pred_tipo_6),
    by = c(
      "id_ine_id_trabajador", "id_ine_id_empresa",
      "anno_devengamiento_remuneracion", "mes_devengamiento_remuneracion"
    )
  )

# obtener períodos disponibles ----
periodos <- get_periodos(dt_suseso)

# escritura por período ----
tictoc::tic()

fn_sql <- function(anio, mes) {
  glue::glue(
    "SELECT *
     FROM ({dbplyr::remote_query(
       dplyr::filter(
         dt_preprocesado,
         anno_devengamiento_remuneracion == {anio},
         mes_devengamiento_remuneracion  == {mes}
       )
     )})"
  )
}

fn_ruta <- function(anio, mes) {
  mes_fmt <- formatC(mes, width = 2, flag = "0")
  glue::glue("{ruta_salida}/anno={anio}/mes={mes_fmt}/trl_{anio}{mes_fmt}.parquet")
}

fallidos <- escribir_periodos(periodos, fn_sql, fn_ruta, con, reintentos = 5)

tictoc::toc()
reporte_fallidos(fallidos, ruta_salida)
