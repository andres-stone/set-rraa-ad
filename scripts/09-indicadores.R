devtools::load_all(here::here())

# ── Configuración ──────────────────────────────────────────────────────────────
RUTA_LOCAL <- here::here("outputs", "indicadores")
dir.create(RUTA_LOCAL, showWarnings = FALSE, recursive = TRUE)

# ── Carga de insumos ──────────────────────────────────────────────────────────
message("Cargando insumos desde MinIO...")
dt_pt        <- leer_rds_minio("ooee/trl/resultados/puestos_trabajo.rds")
dt_fijo      <- leer_rds_minio("ooee/trl/resultados/flujos_fijo.rds")
dt_movil     <- leer_rds_minio("ooee/trl/resultados/flujos_movil.rds")

# ── Total de puestos desplazado +12 meses (denominador de incidencia) ─────────
pt_total_shifted <- dt_pt$pt_total %>%
  dplyr::mutate(
    fecha         = lubridate::ymd(fecha) + lubridate::period(12, "months"),
    pt_total_12   = pt
  ) %>%
  dplyr::select(fecha, pt_total_12)

# ── Función auxiliar: combinar pt + flujos y calcular indicadores ─────────────
calcular_tabla <- function(nombre, dt_pt_list, dt_flujos_list, tipo = c("fijo", "movil")) {
  tipo <- match.arg(tipo)

  join_keys <- get_join_keys(nombre)
  vars_dim  <- setdiff(join_keys, "fecha")

  tbl_pt     <- dt_pt_list[[sub("flujos_", "pt_", nombre)]]
  tbl_flujos <- dt_flujos_list[[nombre]]

  if (is.null(tbl_pt) || is.null(tbl_flujos)) return(NULL)

  df_base <- dplyr::full_join(tbl_pt, tbl_flujos, by = join_keys) %>%
    dplyr::mutate(fecha = lubridate::ymd(fecha)) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(c(vars_dim, "fecha"))))

  df_tasas <- calcular_tasas(df_base)
  df_inc   <- calcular_incidencia(df_tasas, pt_total_shifted)

  transformar_general(df_inc)
}

# ── Calcular indicadores para todos los dominios ──────────────────────────────
message("Calculando indicadores de dominio fijo...")
nombres_fijo <- names(dt_fijo)

dt_largo_fijo <- purrr::set_names(nombres_fijo) %>%
  purrr::map(~ calcular_tabla(.x, dt_pt, dt_fijo, tipo = "fijo")) %>%
  purrr::compact()

message("Calculando indicadores de dominio móvil...")
nombres_movil <- names(dt_movil)

dt_largo_movil <- purrr::set_names(nombres_movil) %>%
  purrr::map(~ calcular_tabla(.x, dt_pt, dt_movil, tipo = "movil")) %>%
  purrr::compact()

# ── Incidencia en variación TRL ───────────────────────────────────────────────
message("Calculando inc_var_trl...")

dt_var_trl_fijo  <- purrr::imap(dt_largo_fijo,  ~ calcular_inc_var_trl(.x))
dt_var_trl_movil <- purrr::imap(dt_largo_movil, ~ calcular_inc_var_trl(.x))

dt_largo_fijo  <- integrar_inc_var_trl(dt_largo_fijo,  dt_var_trl_fijo)
dt_largo_movil <- integrar_inc_var_trl(dt_largo_movil, dt_var_trl_movil)

# ── Guardado ──────────────────────────────────────────────────────────────────
message("Guardando resultados...")
guardar_rds_minio(dt_largo_fijo,  "ooee/trl/resultados/indicadores_fijo.rds")
guardar_rds_minio(dt_largo_movil, "ooee/trl/resultados/indicadores_movil.rds")

guardar_excel_por_tabla(dt_largo_fijo,  file.path(RUTA_LOCAL, "fijo"))
guardar_excel_por_tabla(dt_largo_movil, file.path(RUTA_LOCAL, "movil"))

message("Etapa 09 completada.")
