devtools::load_all(here::here())

# ── Configuración ──────────────────────────────────────────────────────────────
# Rutas de los indicadores de referencia (período anterior / RP)
RUTA_RP_FIJO  <- "ooee/trl/referencia/indicadores_fijo.rds"
RUTA_RP_MOVIL <- "ooee/trl/referencia/indicadores_movil.rds"

RUTA_LOCAL <- here::here("outputs", "cuadratura")
dir.create(RUTA_LOCAL, showWarnings = FALSE, recursive = TRUE)

# ── Carga de insumos ──────────────────────────────────────────────────────────
message("Cargando indicadores AD (actuales) y RP (referencia)...")
dt_ad_fijo  <- leer_rds_minio("ooee/trl/resultados/indicadores_fijo.rds")
dt_ad_movil <- leer_rds_minio("ooee/trl/resultados/indicadores_movil.rds")

dt_rp_fijo  <- leer_rds_minio(RUTA_RP_FIJO)
dt_rp_movil <- leer_rds_minio(RUTA_RP_MOVIL)

# ── Definición de dimensiones por tabla ──────────────────────────────────────
# dim_cols: names = nombre en AD, values = nombre en RP (si son iguales, mismo nombre)
get_dim_cols <- function(nombre) {
  dims <- character(0)
  if (grepl("sx",     nombre)) dims <- c(dims, "sexo")
  if (grepl("nc",     nombre)) dims <- c(dims, "nacionalidad_final")
  if (grepl("re",     nombre)) dims <- c(dims, "region_final")
  if (grepl("sector", nombre)) dims <- c(dims, "seccion_ciiu4cl")
  if (grepl("tamano", nombre)) dims <- c(dims, "tamano_empresa_movil")
  if (grepl("te",     nombre)) dims <- c(dims, "tramo_edad")
  stats::setNames(dims, dims)
}

# ── Comparación para cada tabla ───────────────────────────────────────────────
message("Ejecutando cuadratura dominio fijo...")
nombres_comunes_fijo <- intersect(names(dt_ad_fijo), names(dt_rp_fijo))

resultados_fijo <- purrr::set_names(nombres_comunes_fijo) %>%
  purrr::map(function(nombre) {
    tryCatch(
      compare_flujos(
        tbl_ad       = dt_ad_fijo[[nombre]],
        tbl_rp       = dt_rp_fijo[[nombre]],
        dim_cols     = get_dim_cols(nombre),
        rp_rename_time = FALSE
      ),
      error = function(e) {
        message(glue::glue("ERROR en {nombre}: {e$message}"))
        NULL
      }
    )
  })

message("Ejecutando cuadratura dominio móvil...")
nombres_comunes_movil <- intersect(names(dt_ad_movil), names(dt_rp_movil))

resultados_movil <- purrr::set_names(nombres_comunes_movil) %>%
  purrr::map(function(nombre) {
    tryCatch(
      compare_flujos(
        tbl_ad       = dt_ad_movil[[nombre]],
        tbl_rp       = dt_rp_movil[[nombre]],
        dim_cols     = get_dim_cols(nombre),
        rp_rename_time = FALSE
      ),
      error = function(e) {
        message(glue::glue("ERROR en {nombre}: {e$message}"))
        NULL
      }
    )
  })

# ── Resumen ───────────────────────────────────────────────────────────────────
resumen_fijo  <- resumen_cuadratura(resultados_fijo)
resumen_movil <- resumen_cuadratura(resultados_movil)

message("\n── Resumen cuadratura FIJO ──")
print(resumen_fijo)
message("\n── Resumen cuadratura MÓVIL ──")
print(resumen_movil)

# ── Guardado ──────────────────────────────────────────────────────────────────
message("Guardando resultados...")
guardar_excel(
  lista_resultados = c(
    list(resumen_fijo = resumen_fijo, resumen_movil = resumen_movil),
    purrr::set_names(resultados_fijo,  paste0("f_", names(resultados_fijo))),
    purrr::set_names(resultados_movil, paste0("m_", names(resultados_movil)))
  ),
  ruta_local = file.path(RUTA_LOCAL, "cuadratura.xlsx")
)

guardar_rds_minio(
  list(fijo = resultados_fijo, movil = resultados_movil),
  "ooee/trl/resultados/cuadratura.rds"
)

message("Etapa 10 completada.")
