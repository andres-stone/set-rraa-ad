#' Consolida puestos de trabajo SUSESO en un registro único por período
#'
#' Aplica los 4 pasos de deduplicación y consolidación:
#'   1. Máximo tipo de declaración por puesto de trabajo
#'   2. Distinct por variables clave del puesto
#'   3. Anulación de remuneración/días para tipo_pago 2 o 3
#'   4. Suma de montos y tope de 30 días trabajados
#'
#' @param dt Lazy table de cotizaciones SUSESO ya filtrada
#' @return Lazy table con un registro por (trabajador, empresa, período)
#' @export
consolidar_puestos_trabajo <- function(dt) {
  dt %>%
    dplyr::mutate(
      tipo_pago = dplyr::if_else(is.na(tipo_pago), 1, tipo_pago)
    ) %>%

    # paso 1: máximo tipo de declaración por puesto
    dplyr::group_by(
      id_ine_id_trabajador, id_ine_id_empresa,
      anno_devengamiento_remuneracion, mes_devengamiento_remuneracion,
      tipo_trabajador
    ) %>%
    dplyr::mutate(
      max_tipo_declaracion = max(tipo_declaracion, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%

    # paso 2: distinct por puesto + tipo_pago + montos + días + mutual
    dplyr::distinct(
      id_ine_id_trabajador, id_ine_id_empresa,
      anno_devengamiento_remuneracion, mes_devengamiento_remuneracion,
      tipo_trabajador, tipo_pago,
      monto_remuneracion, n_dias_trabajados,
      codigo_mutual,
      .keep_all = TRUE
    ) %>%

    # paso 3: anular monto y días si tipo_pago es licencia/subsidio (2 o 3)
    dplyr::mutate(
      n_dias_trabajados  = as.integer(n_dias_trabajados),
      monto_remuneracion = dplyr::if_else(tipo_pago %in% c(2, 3), 0, monto_remuneracion),
      n_dias_trabajados  = dplyr::if_else(tipo_pago %in% c(2, 3), 0L, n_dias_trabajados)
    ) %>%

    # paso 4: suma por período y tope de 30 días
    dplyr::group_by(
      id_ine_id_trabajador, id_ine_id_empresa,
      anno_devengamiento_remuneracion, mes_devengamiento_remuneracion,
      tipo_trabajador
    ) %>%
    dplyr::summarise(
      monto_remuneracion = sum(monto_remuneracion, na.rm = TRUE),
      n_dias_trabajados  = sum(n_dias_trabajados,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      n_dias_trabajados = dplyr::if_else(n_dias_trabajados > 30L, 30L, n_dias_trabajados)
    )
}
