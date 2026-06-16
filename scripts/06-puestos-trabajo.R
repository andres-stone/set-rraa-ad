devtools::load_all(here::here())

# ── Configuración ──────────────────────────────────────────────────────────────
RUTA_CALCULO  <- "s3://desarrollo/ooee/trl/completas"
RUTA_SALIDA   <- "s3://desarrollo/ooee/trl/resultados"
RUTA_LOCAL    <- here::here("outputs", "puestos")
dir.create(RUTA_LOCAL, showWarnings = FALSE, recursive = TRUE)

bucket <- setup_bucket_arrow()
con    <- setup_conexion(
  access  = Sys.getenv("ACCESS"),
  secret  = Sys.getenv("SECRET")
)

lista_archivos <- get_lista_archivos(con, RUTA_CALCULO)

# Columnas necesarias
cols_pt <- c(
  "id_ine_id_trabajador", "id_ine_id_empresa",
  "sexo", "nacionalidad_final", "region_final",
  "seccion_ciiu4cl", "tamano_empresa_movil", "tramo_edad"
)

# ── Cálculo de puestos de trabajo ─────────────────────────────────────────────
message("Calculando puestos de trabajo para todos los períodos...")
tbl_pt_completo <- ejecutar_puestos(
  lista_archivos    = lista_archivos,
  bucket            = bucket,
  cols              = cols_pt,
  vars_clasificacion = c("sexo", "nacionalidad_final", "region_final",
                         "seccion_ciiu4cl", "tamano_empresa_movil", "tramo_edad")
)

# ── Agregados por dominio ─────────────────────────────────────────────────────
message("Calculando agregados...")

dt_pt <- list(
  pt_total          = calcular_agregado_pt(tbl_pt_completo, character(0)),
  pt_sx             = calcular_agregado_pt(tbl_pt_completo, "sexo"),
  pt_nc             = calcular_agregado_pt(tbl_pt_completo, "nacionalidad_final"),
  pt_re             = calcular_agregado_pt(tbl_pt_completo, "region_final"),
  pt_sector         = calcular_agregado_pt(tbl_pt_completo, "seccion_ciiu4cl"),
  pt_tamano         = calcular_agregado_pt(tbl_pt_completo, "tamano_empresa_movil"),
  pt_te             = calcular_agregado_pt(tbl_pt_completo, "tramo_edad"),
  pt_sx_sector      = calcular_agregado_pt(tbl_pt_completo, c("sexo", "seccion_ciiu4cl")),
  pt_sx_tamano      = calcular_agregado_pt(tbl_pt_completo, c("sexo", "tamano_empresa_movil")),
  pt_sx_re          = calcular_agregado_pt(tbl_pt_completo, c("sexo", "region_final")),
  pt_sx_te          = calcular_agregado_pt(tbl_pt_completo, c("sexo", "tramo_edad")),
  pt_nc_sector      = calcular_agregado_pt(tbl_pt_completo, c("nacionalidad_final", "seccion_ciiu4cl")),
  pt_re_sector      = calcular_agregado_pt(tbl_pt_completo, c("region_final", "seccion_ciiu4cl")),
  pt_re_tamano      = calcular_agregado_pt(tbl_pt_completo, c("region_final", "tamano_empresa_movil"))
)

# ── Guardado ──────────────────────────────────────────────────────────────────
message("Guardando resultados...")
guardar_rds_minio(dt_pt, "ooee/trl/resultados/puestos_trabajo.rds")
guardar_excel(dt_pt, file.path(RUTA_LOCAL, "puestos_trabajo.xlsx"))

message("Etapa 06 completada.")
DBI::dbDisconnect(con)
