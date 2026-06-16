devtools::load_all(here::here())

# ── Configuración ──────────────────────────────────────────────────────────────
RUTA_CALCULO <- "s3://desarrollo/ooee/trl/completas"
RUTA_LOCAL   <- here::here("outputs", "flujos_movil")
dir.create(RUTA_LOCAL, showWarnings = FALSE, recursive = TRUE)

bucket <- setup_bucket_arrow()
con    <- setup_conexion(
  access = Sys.getenv("ACCESS"),
  secret = Sys.getenv("SECRET")
)

lista_archivos <- get_lista_archivos(con, RUTA_CALCULO)

# Columnas necesarias — incluye vars de reclasificación
cols_flujo <- c(
  "id_ine_id_trabajador", "id_ine_id_empresa",
  "sexo", "nacionalidad_final", "region_final",
  "seccion_ciiu4cl", "tamano_empresa_movil", "tramo_edad"
)

vars_clasificacion <- c(
  "sexo", "nacionalidad_final", "region_final",
  "seccion_ciiu4cl", "tamano_empresa_movil", "tramo_edad"
)

# ── Flujos móvil (dominio móvil, con reclasificaciones) ───────────────────────
message("Calculando flujos de dominio móvil...")
res_movil <- ejecutar_flujos(
  lista_archivos     = lista_archivos,
  fn_flujo           = procesar_flujo_dinamico,
  bucket             = bucket,
  cols               = cols_flujo,
  vars_clasificacion = vars_clasificacion,
  vars_reclasif      = c("tamano_empresa_movil", "tramo_edad")
)

tbl_flujos_movil <- res_movil$tbl_maestra
if (length(res_movil$fallidos) > 0) {
  message("Períodos fallidos: ", paste(res_movil$fallidos, collapse = ", "))
}

# ── Pivotar a formato ancho (entrada / salida) ────────────────────────────────
message("Pivotando flujos a formato ancho...")

pivotar_y_agregar <- function(data, vars_agrupacion) {
  data %>%
    calcular_agregado_flujos_dinamico(vars_agrupacion) %>%
    pivotar_flujos_dinamicos()
}

dt_flujos_movil <- list(
  flujos_total          = pivotar_y_agregar(tbl_flujos_movil, character(0)),
  flujos_sx             = pivotar_y_agregar(tbl_flujos_movil, "sexo"),
  flujos_nc             = pivotar_y_agregar(tbl_flujos_movil, "nacionalidad_final"),
  flujos_re             = pivotar_y_agregar(tbl_flujos_movil, "region_final"),
  flujos_sector         = pivotar_y_agregar(tbl_flujos_movil, "seccion_ciiu4cl"),
  flujos_tamano         = pivotar_y_agregar(tbl_flujos_movil, "tamano_empresa_movil"),
  flujos_te             = pivotar_y_agregar(tbl_flujos_movil, "tramo_edad"),
  flujos_sx_sector      = pivotar_y_agregar(tbl_flujos_movil, c("sexo", "seccion_ciiu4cl")),
  flujos_sx_tamano      = pivotar_y_agregar(tbl_flujos_movil, c("sexo", "tamano_empresa_movil")),
  flujos_sx_re          = pivotar_y_agregar(tbl_flujos_movil, c("sexo", "region_final")),
  flujos_sx_te          = pivotar_y_agregar(tbl_flujos_movil, c("sexo", "tramo_edad")),
  flujos_nc_sector      = pivotar_y_agregar(tbl_flujos_movil, c("nacionalidad_final", "seccion_ciiu4cl")),
  flujos_re_sector      = pivotar_y_agregar(tbl_flujos_movil, c("region_final", "seccion_ciiu4cl")),
  flujos_re_tamano      = pivotar_y_agregar(tbl_flujos_movil, c("region_final", "tamano_empresa_movil"))
)

# ── Guardado ──────────────────────────────────────────────────────────────────
message("Guardando resultados...")
guardar_rds_minio(dt_flujos_movil, "ooee/trl/resultados/flujos_movil.rds")
guardar_excel(dt_flujos_movil, file.path(RUTA_LOCAL, "flujos_movil.xlsx"))

message("Etapa 08 completada.")
DBI::dbDisconnect(con)
