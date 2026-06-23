#' Filtra cotizaciones por tipo de trabajador
#'
#' @param dt Lazy table de cotizaciones SUSESO
#' @param tipo Tipo de trabajador a conservar (default: 1, asalariados dependientes)
#' @return Lazy table filtrada
#' @export
filtrar_tipo_trabajador <- function(dt, tipo = 1) {
  dplyr::filter(dt, tipo_trabajador == tipo)
}

#' Une solo las columnas de fecha del REP necesarias para filtrar
#'
#' Realiza un join mínimo con la tabla de personas: solo fecha_nac y
#' fecha_def_cor_rc. No transporta sexo, nacionalidad ni región para
#' mantener el esquema de preprocesadas liviano.
#'
#' @param dt Lazy table de cotizaciones
#' @param dt_personas Lazy table de personas (output de Script 01)
#' @param con Conexión DuckDB
#' @return Lazy table con columnas fecha_nac y fecha_def_cor_rc agregadas
#' @export
join_fechas_personas <- function(dt, dt_personas, con) {
  dt_fechas <- dt_personas %>%
    dplyr::select(id_ine, fecha_nac, fecha_def_cor_rc)

  dt %>%
    dplyr::left_join(dt_fechas, by = c("id_ine_id_trabajador" = "id_ine"))
}

#' Filtra trabajadores fuera del rango de edad válido
#'
#' Calcula edad en meses respecto al período de devengamiento y filtra
#' a quienes tienen entre min_anios y max_anios. Conserva registros
#' sin fecha de nacimiento (edad_meses NA).
#'
#' @param dt Lazy table con fecha_nac, anno_devengamiento_remuneracion, mes_devengamiento_remuneracion
#' @param min_anios Edad mínima en años (default: 15)
#' @param max_anios Edad máxima en años (default: 90)
#' @return Lazy table filtrada por edad, sin la columna auxiliar edad_meses_filtro
#' @export
filtrar_edad <- function(dt, min_anios = 15, max_anios = 90) {
  min_meses <- min_anios * 12L
  max_meses <- max_anios * 12L

  dt %>%
    dplyr::mutate(
      fecha_nac_ok = dplyr::if_else(
        fecha_nac %in% c("--", "", " "), NA_character_, fecha_nac
      ),
      fecha_nac_ok = as.Date(fecha_nac_ok),
      anno_nac_    = lubridate::year(fecha_nac_ok),
      mes_nac_     = lubridate::month(fecha_nac_ok),
      edad_meses_filtro = as.integer(
        (anno_devengamiento_remuneracion - anno_nac_) * 12L +
          (mes_devengamiento_remuneracion  - mes_nac_)
      )
    ) %>%
    dplyr::filter(
      (edad_meses_filtro >= min_meses & edad_meses_filtro < max_meses) |
        is.na(edad_meses_filtro)
    ) %>%
    dplyr::select(-c(fecha_nac_ok, anno_nac_, mes_nac_, edad_meses_filtro))
}

#' Excluye trabajadores fallecidos antes del período de devengamiento
#'
#' Un trabajador se excluye si su fecha de defunción es anterior al
#' inicio del período (meses_fallecimiento > 0).
#'
#' @param dt Lazy table con fecha_def_cor_rc, anno_devengamiento_remuneracion,
#'   mes_devengamiento_remuneracion
#' @return Lazy table sin fallecidos previos al período
#' @export
filtrar_fallecidos <- function(dt) {
  dt %>%
    dplyr::mutate(
      fecha_def_ok = dplyr::if_else(
        fecha_def_cor_rc %in% c("--", "", " "), NA_character_, fecha_def_cor_rc
      ),
      fecha_def_ok = as.Date(fecha_def_ok),
      anno_def_    = lubridate::year(fecha_def_ok),
      mes_def_     = lubridate::month(fecha_def_ok),
      meses_fall_  = as.integer(
        (anno_devengamiento_remuneracion - anno_def_) * 12L +
          (mes_devengamiento_remuneracion  - mes_def_)
      )
    ) %>%
    dplyr::filter(
      is.na(fecha_def_ok) | meses_fall_ <= 0
    ) %>%
    dplyr::select(-c(fecha_def_ok, anno_def_, mes_def_, meses_fall_))
}
