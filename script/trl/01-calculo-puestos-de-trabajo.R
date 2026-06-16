
# procesamiento 
# calculo de puestos de trabajo
# 852.487 sec

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

# conexion
con <- duckdbfs::cached_connection()

duckdbfs::duckdb_s3_config(
  conn = con,
  s3_access_key_id = Sys.getenv("ACCESS"),
  s3_secret_access_key = Sys.getenv("SECRET"),
  s3_endpoint = "api-minio.ine.gob.cl",
  s3_region = "us-east-1",
  s3_url_style = "path",  # Automático si defines endpoint
  s3_use_ssl = TRUE,
  s3_uploader_thread_limit = 25 # Limita la concurrencia de subida
  
)

DBI::dbExecute(con, "SET http_retries = 5")               # Reintenta hasta 5 veces si hay error HTTP
DBI::dbExecute(con, "SET http_retry_wait_ms = 1000")      # Espera 1 segundo entre reintentos
DBI::dbExecute(con, "SET http_keep_alive = false")        # Previene que MinIO mantenga sockets colgados

# rutas 
# ruta_calculo <- "s3://desarrollo/ooee/trl/5_procesamiento/5_8_finalizacion_datos/espejo"
ruta_calculo <- "s3://desarrollo/ooee/trl/5_procesamiento/5_8_finalizacion_datos/es/202512"

# archivos
lista_archivos <- dbGetQuery(
  con,
  glue("SELECT file FROM glob('{ruta_calculo}/**/*.parquet') ORDER BY file")
)$file

# bucket arrow autenticado 
bucket <- arrow::s3_bucket(
  "desarrollo",
  access_key        = Sys.getenv("ACCESS"),
  secret_key        = Sys.getenv("SECRET"),
  endpoint_override = "api-minio.ine.gob.cl",
  region            = "us-east-1",
  scheme            = "https"
)

# funcion de estimacion ----

procesar_archivo_mensual <- 
  function(archivo) {
    # fecha
    fecha_archivo <- ymd(paste0(substr(basename(archivo), 5, 10), "01"))
    
    # extraer solo el path interno al bucket (sin "s3://desarrollo/")
    ruta_en_bucket <- sub("^s3://desarrollo/", "", archivo)
    
    # leer parquet como Arrow Table
    ds <- 
      arrow::read_parquet(
        bucket$path(ruta_en_bucket),
        as_data_frame = FALSE
      )
    
    # preprocesamiento
    df_preprocesado <-
      ds %>%
      filter(
        tamano != "unipersonal",
        !seccion_ciiu4cl %in% c("T", "U") | is.na(seccion_ciiu4cl)
      )
    
    # procesamiento
    df_agrupado <-
      df_preprocesado %>%
      select(
        id_ine_id_trabajador, id_ine_id_empresa,
        sexo, 
        nacionalidad_final, 
        region_final,
        tramo_edad,
        seccion_ciiu4cl,
        tamano_empresa_movil
      ) %>%
      collect() %>%
      
      # distinct solo sobre las llaves del puesto de trabajo
      distinct(
        id_ine_id_trabajador, 
        id_ine_id_empresa, 
        .keep_all = TRUE
      ) %>%
      
      group_by(
        sexo, nacionalidad_final, region_final, tramo_edad, 
        seccion_ciiu4cl,
        tamano_empresa_movil
      ) %>%
      
      summarise(pt = n(), .groups = "drop") %>%
      mutate(fecha = fecha_archivo) %>%
      select(fecha, everything())
    
    gc()
    return(df_agrupado)
  }


# ejecucion puestos de trabajo ----
tictoc::tic()
tbl_base_maestra <-
  purrr::map(
    lista_archivos,
    \(archivo) {
      message(glue("Procesando: {basename(archivo)}"))
      tryCatch(
        procesar_archivo_mensual(archivo),
        error = function(e) {
          message(glue("ERROR en {basename(archivo)}: {e$message}"))
          NULL
        }
      )
    }
  ) %>%
  purrr::compact() %>%
  bind_rows()
tictoc::toc()

# generacion de Reportes ----
calcular_agregado <- 
  function(data, variables_agrupacion) {
    
    vars_group <- c("fecha", variables_agrupacion)
    data %>%
      group_by(across(all_of(vars_group))) %>%
      summarise(pt = sum(pt), .groups = "drop")
    
  }

# configuración de reportes
configuracion_reportes <- 
  list(
    
    # 1.
    "total" = c(),
    
    # nivel 1
    "sx" = c("sexo"),
    "nc" = c("nacionalidad_final"),
    "re" = c("region_final"),
    "te" = c("tramo_edad"),
    "sector" = c("seccion_ciiu4cl"),
    "tamano" = c("tamano_empresa_movil"),
    
    # nivel 2
    # "sx-nc"     = c("sexo", "nacionalidad_final"),
    "sx-sector" = c("sexo", "seccion_ciiu4cl"),
    # "nc-sector" = c("nacionalidad_final", "seccion_ciiu4cl"),
    "sx-tamano" = c("sexo", "tamano_empresa_movil"),
    # "nc-tamano" = c("nacionalidad_final", "tamano_empresa_movil"),
    "tamano-sector" = c("tamano_empresa_movil", "seccion_ciiu4cl"),
    
    # nuevas
    "sx-re" = c("sexo", "region_final"),
    "sx-te" = c("sexo", "tramo_edad"),
    
    # nivel 3
    # "sx-nc-te"      = c("sexo", "nacionalidad_final", "tramo_edad"),
    # "sx-nc-sector"  = c("sexo", "nacionalidad_final", "seccion_ciiu4cl"),
    # "sx-nc-tamano"  = c("sexo", "nacionalidad_final", "tamano_empresa_movil"),
    "sx-tamano-sector" = c("sexo", "tamano_empresa_movil", "seccion_ciiu4cl")
    # "nc-tamano-sector" = c("nacionalidad_final", "tamano_empresa_movil", "seccion_ciiu4cl"),
    
    # nivel 4
    # "sx-nc-tamano-sector"   = c("sexo", "nacionalidad_final", "tamano_empresa_movil", "seccion_ciiu4cl")
    
  )

lista_resultados <- 
  map(
    configuracion_reportes, 
    ~ calcular_agregado(tbl_base_maestra, .x)
  )

# guardado en Excel ----
library(openxlsx)
library(stringr)

wb <- createWorkbook()

iwalk(lista_resultados, function(datos, nombre_hoja) {
  nombre_hoja_safe <- str_sub(nombre_hoja, 1, 31)
  addWorksheet(wb, nombre_hoja_safe)
  writeData(wb, sheet = nombre_hoja_safe, x = datos)
})

saveWorkbook(
  wb, 
  "output/trl/tbl_suseso_pt.xlsx", 
  overwrite = TRUE
)

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
    object    = "ooee/trl/6_analisis/6_1_preparacion/es/202512/tbl_suseso_pt.rds",
    bucket    = "desarrollo",
    region    = "",
    use_https = TRUE,
    base_url  = "api-minio.ine.gob.cl",
    url_style = "path"
  )
})
