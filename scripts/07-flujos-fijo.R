devtools::load_all(here::here())

# ── Configuración ──────────────────────────────────────────────────────────────
RUTA_CALCULO <- "s3://desarrollo/ooee/trl/completas"
RUTA_LOCAL   <- here::here("outputs", "flujos_fijo")
dir.create(RUTA_LOCAL, showWarnings = FALSE, recursive = TRUE)

bucket <- setup_bucket_arrow()
con    <- setup_conexion(
  access = Sys.getenv("ACCESS"),
  secret = Sys.getenv("SECRET")
)

lista_archivos <- get_lista_archivos(con, RUTA_CALCULO)

# Columnas necesarias
cols_flujo <- c(
  "id_ine_id_trabajador", "id_ine_id_empresa",
  "sexo", "nacionalidad_final", "region_final",
  "seccion_ciiu4cl", "tamano_empresa_movil", "tramo_edad"
)

vars_clasificacion <- c(
  "sexo", "nacionalidad_final", "region_final",
  "seccion_ciiu4cl", "tamano_empresa_movil", "tramo_edad"
)

# ── Flujos fijo (dominio fijo, t vs t-12) ─────────────────────────────────────
message("Calculando flujos de dominio fijo...")
res_fijo <- ejecutar_flujos(
  lista_archivos    = lista_archivos,
  fn_flujo          = procesar_flujo_fijo,
  bucket            = bucket,
  cols              = cols_flujo,
  vars_clasificacion = vars_clasificacion
)

tbl_flujos_fijo <- res_fijo$tbl_maestra
if (length(res_fijo$fallidos) > 0) {
  message("Períodos fallidos: ", paste(res_fijo$fallidos, collapse = ", "))
}

# ── Agregados por dominio ─────────────────────────────────────────────────────
message("Calculando agregados...")

dt_flujos_fijo <- list(
  flujos_total          = calcular_agregado_flujos(tbl_flujos_fijo, character(0)),
  flujos_sx             = calcular_agregado_flujos(tbl_flujos_fijo, "sexo"),
  flujos_nc             = calcular_agregado_flujos(tbl_flujos_fijo, "nacionalidad_final"),
  flujos_re             = calcular_agregado_flujos(tbl_flujos_fijo, "region_final"),
  flujos_sector         = calcular_agregado_flujos(tbl_flujos_fijo, "seccion_ciiu4cl"),
  flujos_tamano         = calcular_agregado_flujos(tbl_flujos_fijo, "tamano_empresa_movil"),
  flujos_te             = calcular_agregado_flujos(tbl_flujos_fijo, "tramo_edad"),
  flujos_sx_sector      = calcular_agregado_flujos(tbl_flujos_fijo, c("sexo", "seccion_ciiu4cl")),
  flujos_sx_tamano      = calcular_agregado_flujos(tbl_flujos_fijo, c("sexo", "tamano_empresa_movil")),
  flujos_sx_re          = calcular_agregado_flujos(tbl_flujos_fijo, c("sexo", "region_final")),
  flujos_sx_te          = calcular_agregado_flujos(tbl_flujos_fijo, c("sexo", "tramo_edad")),
  flujos_nc_sector      = calcular_agregado_flujos(tbl_flujos_fijo, c("nacionalidad_final", "seccion_ciiu4cl")),
  flujos_re_sector      = calcular_agregado_flujos(tbl_flujos_fijo, c("region_final", "seccion_ciiu4cl")),
  flujos_re_tamano      = calcular_agregado_flujos(tbl_flujos_fijo, c("region_final", "tamano_empresa_movil"))
)

# ── Guardado ──────────────────────────────────────────────────────────────────
message("Guardando resultados...")
guardar_rds_minio(dt_flujos_fijo, "ooee/trl/resultados/flujos_fijo.rds")
guardar_excel(dt_flujos_fijo, file.path(RUTA_LOCAL, "flujos_fijo.xlsx"))

message("Etapa 07 completada.")
DBI::dbDisconnect(con)
