#' Recodifica nombres de indicadores al estándar común AD/RP
#'
#' @param df Tibble con columna indicador
#' @return Tibble con indicadores renombrados
#' @export
recode_indicadores <- function(df) {
  df %>%
    dplyr::mutate(
      indicador = dplyr::recode(
        indicador,
        pt        = "pt_cur",
        pt_ent_12 = "pt_ent",
        pt_sal_12 = "pt_sal",
        inc_trln  = "inc_rot_neta"
      )
    )
}

#' Compara indicadores entre la base AD y la base RP
#'
#' Une por anno, mes, indicador y dimensiones de clasificación,
#' calculando la diferencia absoluta entre ambas fuentes.
#'
#' @param tbl_ad Tibble de indicadores AD (formato largo con columna valor)
#' @param tbl_rp Tibble de indicadores RP (formato largo con columna valor)
#' @param dim_cols Named vector de dimensiones: names = nombre en AD, values = nombre en RP
#' @param rp_rename_time Si TRUE, renombra anno_dev.cur/mes_dev.cur a anno/mes en RP
#' @param filter_na_dims Si TRUE, excluye filas con NA en las dimensiones de clasificación
#' @param extra_mutate_ad Función opcional aplicada al tibble AD antes del join
#' @param extra_mutate_rp Función opcional aplicada al tibble RP antes del join
#' @return Tibble con columnas valor_ad, valor_rp y d (diferencia)
#' @export
compare_flujos <- function(
    tbl_ad,
    tbl_rp,
    dim_cols,
    rp_rename_time  = TRUE,
    filter_na_dims  = TRUE,
    extra_mutate_ad = NULL,
    extra_mutate_rp = NULL
) {
  # preparar AD
  dt_ad <- tbl_ad %>%
    dplyr::filter(!is.na(valor)) %>%
    dplyr::select(-dplyr::any_of("fecha")) %>%
    recode_indicadores()

  if (filter_na_dims && length(dim_cols) > 0) {
    ad_dim_names <- names(dim_cols)
    dt_ad <- dt_ad %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(ad_dim_names), ~ !is.na(.x)))
  }

  if (!is.null(extra_mutate_ad)) dt_ad <- extra_mutate_ad(dt_ad)

  # preparar RP
  dt_rp <- tbl_rp %>%
    dplyr::filter(!is.na(valor))

  if (!is.null(extra_mutate_rp)) dt_rp <- extra_mutate_rp(dt_rp)

  if (rp_rename_time) {
    dt_rp <- dt_rp %>%
      dplyr::rename(anno = anno_dev.cur, mes = mes_dev.cur)
  }

  # renombrar dimensiones RP para que coincidan con AD
  rp_rename_map <- dim_cols[names(dim_cols) != unname(dim_cols)]
  if (length(rp_rename_map) > 0) {
    rename_vec <- stats::setNames(unname(rp_rename_map), names(rp_rename_map))
    dt_rp <- dt_rp %>% dplyr::rename(dplyr::all_of(rename_vec))
  }

  # join y diferencia
  join_cols <- c("anno", "mes", names(dim_cols), "indicador")

  dt_ad %>%
    dplyr::inner_join(dt_rp, by = join_cols, suffix = c("_ad", "_rp")) %>%
    dplyr::mutate(d = valor_ad - valor_rp)
}

#' Genera resumen de diferencias entre AD y RP para todas las comparaciones
#'
#' @param tbl_resultados Lista nombrada de tibbles resultado de compare_flujos
#' @return Tibble resumen con n_filas, n_dif_nz, max_abs_d, mean_abs_d por comparación
#' @export
resumen_cuadratura <- function(tbl_resultados) {
  tbl_resultados %>%
    purrr::compact() %>%
    purrr::imap_dfr(function(dt, nm) {
      tibble::tibble(
        comparacion = nm,
        n_filas     = nrow(dt),
        n_dif_nz    = sum(dt$d != 0, na.rm = TRUE),
        max_abs_d   = max(abs(dt$d), na.rm = TRUE),
        mean_abs_d  = mean(abs(dt$d), na.rm = TRUE)
      )
    })
}
