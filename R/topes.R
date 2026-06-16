#' Genera tabla de topes imponibles mensuales en pesos
#'
#' Calcula el tope imponible en pesos para cada mes/año del período
#' 2016-2025 a partir de los valores UF y los topes en UF vigentes.
#'
#' @return Tibble con columnas: anno_devengamiento_remuneracion,
#'   mes_devengamiento_remuneracion, tope_imponible_peso
#' @export
get_topes_imponibles <- function() {
  valor_uf <- list(
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

  tibble::tibble(
    anno_devengamiento_remuneracion = 2016:2025,
    tope_imponible_uf = c(74.3, 75.7, 78.3, 79.3, 80.2, 81.7, 81.6, 81.6, 84.3, 87.8)
  ) %>%
    tidyr::expand_grid(mes_devengamiento_remuneracion = 1:12) %>%
    dplyr::arrange(anno_devengamiento_remuneracion, mes_devengamiento_remuneracion) %>%
    dplyr::mutate(
      valor_uf            = unlist(valor_uf),
      tope_imponible_peso = as.integer(trunc(valor_uf * tope_imponible_uf))
    ) %>%
    dplyr::select(
      anno_devengamiento_remuneracion,
      mes_devengamiento_remuneracion,
      tope_imponible_peso
    )
}

#' Trunca remuneraciones al tope imponible mensual
#'
#' @param dt Lazy table con monto_remuneracion y columnas de período
#' @param tbl_topes Tabla de topes en DuckDB (creada con registrar_topes_en_duckdb)
#' @return Lazy table con monto_remuneracion topado
#' @export
truncar_a_tope <- function(dt, tbl_topes) {
  dt %>%
    dplyr::left_join(
      tbl_topes,
      by = c("anno_devengamiento_remuneracion", "mes_devengamiento_remuneracion")
    ) %>%
    dplyr::mutate(
      monto_remuneracion = dplyr::if_else(
        monto_remuneracion > tope_imponible_peso,
        as.double(tope_imponible_peso),
        monto_remuneracion
      )
    ) %>%
    dplyr::select(-tope_imponible_peso)
}

#' Registra la tabla de topes imponibles en DuckDB y retorna tbl
#'
#' @param con Conexión DuckDB
#' @return dplyr::tbl apuntando a la tabla topes_imponibles en DuckDB
#' @export
registrar_topes_en_duckdb <- function(con) {
  DBI::dbExecute(con, "DROP TABLE IF EXISTS topes_imponibles")
  DBI::dbWriteTable(con, "topes_imponibles", get_topes_imponibles())
  dplyr::tbl(con, "topes_imponibles")
}
