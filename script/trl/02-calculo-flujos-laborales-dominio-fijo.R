
# procesamiento
# bases mensuales flujos laborales (entradas y salidas t vs t-12)
# sec

# seteo previo ----
rm(list = ls())
options(scipen = 999, digits = 4)
gc()

# librerias
library(dplyr)
library(arrow)
library(purrr)
library(duckdb)
library(duckdbfs)
library(lubridate)
library(glue)
library(openxlsx)
library(stringr)

# conexion
con <- duckdbfs::cached_connection()
duckdbfs::duckdb_s3_config(
  conn = con,
  s3_access_key_id     = Sys.getenv("ACCESS"),
  s3_secret_access_key = Sys.getenv("SECRET"),
  s3_endpoint          = "api-minio.ine.gob.cl",
  s3_region            = "us-east-1",
  s3_url_style         = "path",
  s3_use_ssl           = TRUE,
  s3_uploader_thread_limit = 25
)
DBI::dbExecute(con, "SET http_retries = 5")
DBI::dbExecute(con, "SET http_retry_wait_ms = 1000")
DBI::dbExecute(con, "SET http_keep_alive = false")

# bucket arrow autenticado
bucket <- arrow::s3_bucket(
  "desarrollo",
  access_key        = Sys.getenv("ACCESS"),
  secret_key        = Sys.getenv("SECRET"),
  endpoint_override = "api-minio.ine.gob.cl",
  region            = "us-east-1",
  scheme            = "https"
)

# rutas
# ruta_calculo <- "s3://desarrollo/ooee/trl/5_procesamiento/5_8_finalizacion_datos/espejo"
ruta_calculo <- "s3://desarrollo/ooee/trl/5_procesamiento/5_8_finalizacion_datos/es/202512"

# archivos ordenados
lista_archivos <- dbGetQuery(
  con,
  glue("SELECT file FROM glob('{ruta_calculo}/**/*.parquet') ORDER BY file")
)$file

# Indexar por fecha para facilitar el lookup t-12
fechas_vec    <- ymd(paste0(substr(basename(lista_archivos), 5, 10), "01"))
names(lista_archivos) <- as.character(fechas_vec)

# columnas a usar en flujos
cols_flujo <- c(
  "id_ine_id_trabajador", "id_ine_id_empresa",
  "sexo", "nacionalidad_final", "region_final",
  "seccion_ciiu4cl"
)

# vars de clasificacion (sin llaves de puesto)
vars_clasificacion <- c(
  "sexo", "nacionalidad_final", "region_final",
  "seccion_ciiu4cl"
)

# funcion lectora auxiliar ----
# Lee un parquet desde S3, aplica filtros base y devuelve df con distinct por puesto
leer_mes <- 
  function(archivo) {
    ruta_en_bucket <- sub("^s3://desarrollo/", "", archivo)
    
    arrow::read_parquet(
      bucket$path(ruta_en_bucket),
      as_data_frame = FALSE
    ) %>%
      filter(
        tamano != "unipersonal",
        !seccion_ciiu4cl %in% c("T", "U") | is.na(seccion_ciiu4cl)
      ) %>%
      select(all_of(cols_flujo)) %>%
      collect() %>%
      distinct(id_ine_id_trabajador, id_ine_id_empresa, .keep_all = TRUE)
  }

# funcion core: flujo mensual t vs t-12 ----
procesar_flujo_mensual <- 
  function(idx) {
    archivo_t   <- lista_archivos[idx]
    archivo_t12 <- lista_archivos[idx - 12]
    fecha_t     <- ymd(names(lista_archivos)[idx])
    
    message(glue("  Procesando flujo: {fecha_t}  (t={basename(archivo_t)} | t-12={basename(archivo_t12)})"))
    
    df_t   <- leer_mes(archivo_t)
    df_t12 <- leer_mes(archivo_t12)
    
    # Entradas: puestos en t que NO estaban en t-12
    entradas <- 
      anti_join(df_t, df_t12, by = c("id_ine_id_trabajador", "id_ine_id_empresa")) %>%
      group_by(across(all_of(vars_clasificacion))) %>%
      summarise(pt_ent_12 = n(), .groups = "drop")
    
    # Salidas: puestos en t-12 que NO están en t
    salidas <- 
      anti_join(df_t12, df_t, by = c("id_ine_id_trabajador", "id_ine_id_empresa")) %>%
      group_by(across(all_of(vars_clasificacion))) %>%
      summarise(pt_sal_12 = n(), .groups = "drop")
    
    # Combinar
    flujo <- full_join(entradas, salidas, by = vars_clasificacion) %>%
      mutate(
        fecha     = fecha_t,
        pt_ent_12 = tidyr::replace_na(pt_ent_12, 0L),
        pt_sal_12 = tidyr::replace_na(pt_sal_12, 0L)
      ) %>%
      select(fecha, all_of(vars_clasificacion), pt_ent_12, pt_sal_12)
    
    rm(df_t, df_t12, entradas, salidas)
    gc()
    
    return(flujo)
  }

# ejecucion ----
# Solo procesar desde el mes 13 en adelante (necesitamos t-12)
indices_procesar <- 13:length(lista_archivos)
fallidos         <- character(0)

message(
  glue("Iniciando flujos laborales: {length(indices_procesar)} meses a procesar...")
)

tbl_flujos_maestra <-
  purrr::map(
    indices_procesar,
    \(idx) {
      tryCatch(
        procesar_flujo_mensual(idx),
        error = function(e) {
          archivo_fallido <- basename(lista_archivos[idx])
          message(glue("ERROR en {archivo_fallido}: {e$message}"))
          fallidos <<- c(fallidos, archivo_fallido)
          NULL
        }
      )
    }
  ) %>%
  purrr::compact() %>%
  bind_rows()

if (length(fallidos) > 0) {
  message(
    glue("ADVERTENCIA: {length(fallidos)} mes(es) fallaron: {paste(fallidos, collapse=', ')}")
  )
} else {
  message(
    "todos los meses procesados correctamente."
  )
}

# roll-up / reportes ----
calcular_agregado_flujos <- 
  function(data, variables_agrupacion) {
    vars_group <- c("fecha", variables_agrupacion)
    data %>%
      group_by(across(all_of(vars_group))) %>%
      summarise(
        pt_ent_12 = sum(pt_ent_12),
        pt_sal_12 = sum(pt_sal_12),
        .groups = "drop"
      )
  }

configuracion_reportes <- 
  list(
    # 1.
    "total" = c(),
    
    # nivel 1
    "sx" = c("sexo"),
    "nc" = c("nacionalidad_final"),
    "re" = c("region_final"),
    # "te" = c("tramo_edad"),
    "sector" = c("seccion_ciiu4cl"),
    # "tamano" = c("tamano_empresa_movil"),
    
    # nivel 2
    "sx-sector" = c("sexo", "seccion_ciiu4cl"),
    "sx-re" = c("sexo", "region_final")

    # nivel 3
    # "sx-tamano-sector" = c("sexo", "tamano_empresa_movil", "seccion_ciiu4cl")
  )

lista_resultados <- 
  map(
    configuracion_reportes,
    ~ calcular_agregado_flujos(tbl_flujos_maestra, .x)
  )

# guardado Excel ----
message("Guardando Excel...")
wb <- createWorkbook()

iwalk(lista_resultados, function(datos, nombre_hoja) {
  nombre_hoja_safe <- str_sub(nombre_hoja, 1, 31)
  addWorksheet(wb, nombre_hoja_safe)
  writeData(wb, sheet = nombre_hoja_safe, x = datos)
})

saveWorkbook(wb, "output/trl/tbl_trl_flujos_laborales.xlsx", overwrite = TRUE)

message("¡Proceso completado!")

# guardado en .rds (S3/MinIO) via aws.s3 ----
local({
  Sys.setenv(
    AWS_ACCESS_KEY_ID     = Sys.getenv("ACCESS"),
    AWS_SECRET_ACCESS_KEY = Sys.getenv("SECRET")
  )
  
  tmp <- tempfile(fileext = ".rds")
  saveRDS(lista_resultados, file = tmp)
  on.exit(unlink(tmp))
  
  aws.s3::put_object(
    file      = tmp,
    object    = "ooee/trl/6_analisis/6_1_preparacion/es/202512/tbl_trl_flujos_laborales.rds",
    bucket    = "desarrollo",
    region    = "",
    use_https = TRUE,
    base_url  = "api-minio.ine.gob.cl",
    url_style = "path"
  )
})
