#' Calcula tasas y variaciones de flujos laborales
#'
#' Calcula tasas de entrada, salida, rotación, rotación neta y permanencia,
#' más variaciones a 12 meses y 1 mes de todas las métricas.
#' Las tasas usan pt del período t-12 como denominador.
#'
#' @param df Tibble con columnas fecha, pt, pt_ent_12, pt_sal_12
#'   y opcionalmente columnas de clasificación
#' @return Tibble con todas las tasas y variaciones calculadas
#' @export
calcular_tasas <- function(df) {
  vars_grupo <- setdiff(
    names(df),
    c("fecha", "pt", "pt_ent_12", "pt_sal_12")
  )

  df <- dplyr::arrange(df, fecha)

  if (length(vars_grupo) > 0) {
    df <- dplyr::group_by(df, dplyr::across(dplyr::all_of(vars_grupo)))
  }

  df %>%
    dplyr::mutate(
      # tasas
      t_ent      = pt_ent_12 / dplyr::lag(pt, 12),
      t_sal      = pt_sal_12 / dplyr::lag(pt, 12),
      t_rot      = (t_ent + t_sal) / 2,
      t_rot_neta = t_ent - t_sal,
      t_per      = 1 - t_sal,

      # variación 12 meses
      var12_pt         = (pt           / dplyr::lag(pt, 12))       - 1,
      var12_pt_ent     = (pt_ent_12    / dplyr::lag(pt_ent_12, 12)) - 1,
      var12_pt_sal     = (pt_sal_12    / dplyr::lag(pt_sal_12, 12)) - 1,
      var12_t_ent      = t_ent         - dplyr::lag(t_ent, 12),
      var12_t_rot_neta = t_rot_neta    - dplyr::lag(t_rot_neta, 12),
      var12_t_rot      = t_rot         - dplyr::lag(t_rot, 12),
      var12_t_sal      = t_sal         - dplyr::lag(t_sal, 12),
      var12_t_per      = t_per         - dplyr::lag(t_per, 12),

      # variación 1 mes
      var1_pt         = (pt           / dplyr::lag(pt, 1))       - 1,
      var1_pt_ent     = (pt_ent_12    / dplyr::lag(pt_ent_12, 1)) - 1,
      var1_pt_sal     = (pt_sal_12    / dplyr::lag(pt_sal_12, 1)) - 1,
      var1_t_ent      = t_ent         - dplyr::lag(t_ent, 1),
      var1_t_rot_neta = t_rot_neta    - dplyr::lag(t_rot_neta, 1),
      var1_t_rot      = t_rot         - dplyr::lag(t_rot, 1),
      var1_t_sal      = t_sal         - dplyr::lag(t_sal, 1),
      var1_t_per      = t_per         - dplyr::lag(t_per, 1)
    ) %>%
    dplyr::ungroup()
}

#' Calcula indicadores de incidencia sobre el total de puestos
#'
#' Divide los flujos entre el total de puestos desplazado 12 meses hacia adelante,
#' produciendo tasas de incidencia comparables entre dominios.
#'
#' @param df Tibble con columnas fecha, pt_ent_12, pt_sal_12
#' @param pt_total_shifted Tibble con columnas fecha, pt_total_12
#'   (el total de puestos con fecha desplazada +12 meses)
#' @return Tibble con incidencias: inc_ent, inc_sal, inc_rot, inc_rot_neta, inc_per
#' @export
calcular_incidencia <- function(df, pt_total_shifted) {
  df <- dplyr::left_join(df, pt_total_shifted, by = "fecha")

  vars_grupo <- setdiff(
    names(df),
    c("fecha", "pt", "pt_ent_12", "pt_sal_12", "pt_total_12")
  )

  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(vars_grupo))) %>%
    dplyr::mutate(
      inc_ent      = pt_ent_12 / pt_total_12,
      inc_sal      = pt_sal_12 / pt_total_12,
      inc_rot      = (inc_ent + inc_sal) / 2,
      inc_rot_neta = inc_ent - inc_sal,
      inc_per      = 1 - inc_sal
    ) %>%
    dplyr::ungroup()
}

#' Transforma una tabla de indicadores a formato largo con anno y mes
#'
#' @param df Tibble con columna fecha y columnas de indicadores
#' @return Tibble en formato largo con columnas: anno, mes, fecha, clasificación, indicador, valor
#' @export
transformar_general <- function(df) {
  indicadores <- grep("^pt|^t_|^var|^inc_", names(df), value = TRUE)

  df %>%
    dplyr::mutate(
      fecha = lubridate::ymd(fecha),
      anno  = lubridate::year(fecha),
      mes   = lubridate::month(fecha)
    ) %>%
    tidyr::pivot_longer(
      cols      = dplyr::all_of(indicadores),
      names_to  = "indicador",
      values_to = "valor"
    ) %>%
    dplyr::select(anno, mes, dplyr::everything())
}

#' Calcula la incidencia en la variación de 12 meses de la tasa de rotación
#'
#' @param df Tibble en formato largo con columna indicador e inc_rot
#' @return Tibble con inc_rot, inc_rot_12 e inc_var_trl por grupo de clasificación
#' @export
calcular_inc_var_trl <- function(df) {
  cols_fijas <- c("anno", "mes", "fecha", "indicador", "valor")
  vars_grupo <- setdiff(names(df), cols_fijas)

  df %>%
    dplyr::filter(indicador == "inc_rot") %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(vars_grupo)), fecha) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(vars_grupo))) %>%
    dplyr::mutate(
      inc_rot     = valor,
      inc_rot_12  = dplyr::lag(inc_rot, 12),
      inc_var_trl = inc_rot - inc_rot_12
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(fecha, dplyr::all_of(vars_grupo), inc_rot, inc_rot_12, inc_var_trl)
}

#' Integra inc_var_trl al listado largo de indicadores
#'
#' @param dt_largo Lista nombrada de tibbles en formato largo
#' @param dt_var_trl Lista nombrada de tibbles con inc_var_trl
#' @return Lista con tablas integradas
#' @export
integrar_inc_var_trl <- function(dt_largo, dt_var_trl) {
  purrr::imap(dt_largo, function(tabla_original, nombre_tabla) {
    res <- dt_var_trl[[nombre_tabla]]
    if (is.null(res) || nrow(res) == 0) return(tabla_original)

    res_largo <- res %>%
      tidyr::pivot_longer(
        cols      = c(inc_rot, inc_rot_12, inc_var_trl),
        names_to  = "indicador",
        values_to = "valor"
      )
    dplyr::bind_rows(tabla_original, res_largo)
  })
}

#' Determina las claves de join según el nombre de la tabla de indicadores
#'
#' @param nombre Nombre de la tabla (ej. "flujos_sx_sector")
#' @return Vector de columnas para el join
#' @export
get_join_keys <- function(nombre) {
  base <- c("fecha")
  if (grepl("sx",     nombre)) base <- c(base, "sexo")
  if (grepl("nc",     nombre)) base <- c(base, "nacionalidad_final")
  if (grepl("re",     nombre)) base <- c(base, "region_final")
  if (grepl("sector", nombre)) base <- c(base, "seccion_ciiu4cl")
  if (grepl("tamano", nombre)) base <- c(base, "tamano_empresa_movil")
  if (grepl("te",     nombre)) base <- c(base, "tramo_edad")
  base
}

#' Pivotea flujos dinámicos de largo a ancho (entrada/salida como columnas)
#'
#' @param dt Tibble con columnas tipo_flujo, n y variables de clasificación
#' @return Tibble con columnas pt_ent_12 y pt_sal_12
#' @export
pivotar_flujos_dinamicos <- function(dt) {
  dt_filtrado <- dt[dt$tipo_flujo %in% c("entrada", "salida"), ]
  vars_clave  <- setdiff(names(dt_filtrado), c("tipo_flujo", "n"))

  dt_cast <- data.table::dcast(
    data.table::as.data.table(dt_filtrado),
    formula    = stats::as.formula(paste(paste(vars_clave, collapse = " + "), "~ tipo_flujo")),
    value.var  = "n"
  )
  data.table::setnames(dt_cast, old = "entrada", new = "pt_ent_12")
  data.table::setnames(dt_cast, old = "salida",  new = "pt_sal_12")
  tibble::as_tibble(dt_cast)
}
