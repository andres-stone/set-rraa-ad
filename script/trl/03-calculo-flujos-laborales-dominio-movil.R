
# procesamiento
# bases mensuales flujos laborales DINÁMICOS (entradas, salidas, reclasificaciones t vs t-12)
# trl - espejo

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
library(tidyr)

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

# columnas a usar en flujos dinámicos
cols_flujo <- c(
  "id_ine_id_trabajador", "id_ine_id_empresa",
  "tamano_empresa_movil",
  "tramo_edad",
  "sexo", "nacionalidad_final",
  "region_final",                        # <-- nuevo
  "seccion_ciiu4cl"
)

# vars de clasificacion
  vars_clasificacion <- c(
  "tamano_empresa_movil",
  "tramo_edad",
  "sexo", "nacionalidad_final",
  "region_final",                        # <-- nuevo
  "seccion_ciiu4cl"
)

# funcion lectora auxiliar ----
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

# funcion core: flujos dinámicos t vs t-12 ----
procesar_flujo_dinamico <- 
    function(idx) {
      archivo_t   <- lista_archivos[idx]
      archivo_t12 <- lista_archivos[idx - 12]
      fecha_t     <- ymd(names(lista_archivos)[idx])
      
      message(glue("  Procesando flujo: {fecha_t}  (t={basename(archivo_t)} | t-12={basename(archivo_t12)})"))
      
      df_t   <- leer_mes(archivo_t)
      df_t12 <- leer_mes(archivo_t12)
      
      # --- 1. ENTRADAS PURAS (en t, no en t-12) ---
      entradas_agg <- anti_join(df_t, df_t12, by = c("id_ine_id_trabajador", "id_ine_id_empresa")) %>%
        group_by(across(all_of(vars_clasificacion))) %>%
        summarise(n = n(), .groups = "drop") %>%
        mutate(tipo_flujo = "entrada")
      
      # --- 2. SALIDAS PURAS (en t-12, no en t) ---
      salidas_agg <- anti_join(df_t12, df_t, by = c("id_ine_id_trabajador", "id_ine_id_empresa")) %>%
        group_by(across(all_of(vars_clasificacion))) %>%
        summarise(n = n(), .groups = "drop") %>%
        mutate(tipo_flujo = "salida")
      
      # --- 3. RECLASIFICACIONES (en ambos, pero cambió tamano_empresa_movil O tramo_edad) ---
      reclasif_base <- inner_join(
        df_t,
        df_t12,
        by     = c("id_ine_id_trabajador", "id_ine_id_empresa"),
        suffix = c("_t", "_t12")
      ) %>%
        filter(
          tamano_empresa_movil_t != tamano_empresa_movil_t12 |   # <-- condición extendida
            tramo_edad_t         != tramo_edad_t12
        )
      
      # 3a. Reclasificación Salida — atributos de t-12
      reclasif_salidas_agg <- reclasif_base %>%
        group_by(
          tamano_empresa_movil = tamano_empresa_movil_t12,
          tramo_edad           = tramo_edad_t12,                 # <-- nuevo
          sexo                 = sexo_t12,
          nacionalidad_final         = nacionalidad_final_t12,
          seccion_ciiu4cl      = seccion_ciiu4cl_t12
        ) %>%
        summarise(n = n(), .groups = "drop") %>%
        mutate(tipo_flujo = "reclasif_salida")
      
      # 3b. Reclasificación Entrada — atributos de t
      reclasif_entradas_agg <- reclasif_base %>%
        group_by(
          tamano_empresa_movil = tamano_empresa_movil_t,
          tramo_edad           = tramo_edad_t,                   # <-- nuevo
          sexo                 = sexo_t,
          nacionalidad_final         = nacionalidad_final_t,
          seccion_ciiu4cl      = seccion_ciiu4cl_t
        ) %>%
        summarise(n = n(), .groups = "drop") %>%
        mutate(tipo_flujo = "reclasif_entrada")
      
      # --- 4. UNIFICACIÓN ---
      tbl_final <- bind_rows(
        entradas_agg,
        salidas_agg,
        reclasif_salidas_agg,
        reclasif_entradas_agg
      ) %>%
        mutate(fecha = fecha_t) %>%
        select(fecha, tipo_flujo, all_of(vars_clasificacion), n)
      
      rm(df_t, df_t12, reclasif_base, entradas_agg, salidas_agg,
         reclasif_salidas_agg, reclasif_entradas_agg)
      gc()
      
      return(tbl_final)
    }

# ejecucion ----
indices_procesar <- 13:length(lista_archivos)
fallidos         <- character(0)

message(
  glue(
    "iniciando flujos dinámicos TRL: {length(indices_procesar)} meses a procesar..."
  )
)

tbl_flujos_maestra <-
  purrr::map(
    indices_procesar,
    \(idx) {
      tryCatch(
        procesar_flujo_dinamico(idx),
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
  message(glue("ADVERTENCIA: {length(fallidos)} mes(es) fallaron: {paste(fallidos, collapse = ', ')}"))
} else {
  message("Todos los meses procesados correctamente.")
}

# roll-up / reportes ----
generar_reporte <- 
  function(data, vars_agrupacion) {
    vars <- c("fecha", "tipo_flujo", vars_agrupacion)
    data %>%
      group_by(across(all_of(vars))) %>%
      summarise(n = sum(n), .groups = "drop")
  }

configuracion_reportes <- 
  list(
    # tamano
    "tamano" = c("tamano_empresa_movil"),
    "sx-tamano"     = c("sexo", "tamano_empresa_movil"),
    "tamano-sector" = c("tamano_empresa_movil", "seccion_ciiu4cl"),
    "sx-tamano-sector" = c("sexo", "tamano_empresa_movil", "seccion_ciiu4cl"),
    
    "te" = c("tramo_edad"),
    "sx-te" = c("sexo", "tramo_edad")
  )

lista_resultados <- 
  map(
    configuracion_reportes,
    ~ generar_reporte(tbl_flujos_maestra, .x)
  )

# guardado Excel ----
tictoc::tic()
message("Guardando Excel...")
wb <- createWorkbook()

iwalk(lista_resultados, function(datos, nombre_hoja) {
  nombre_hoja_safe <- str_sub(nombre_hoja, 1, 31)
  addWorksheet(wb, nombre_hoja_safe)
  writeData(wb, sheet = nombre_hoja_safe, x = datos)
})

saveWorkbook(wb, "output/trl/tbl_trl_flujos_dinamicos_tamano.xlsx", overwrite = TRUE)

message("¡Proceso completado!")
tictoc::toc()

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
    object    = "ooee/trl/6_analisis/6_1_preparacion/es/202512/tbl_trl_flujos_dinamicos_tamano.rds",
    bucket    = "desarrollo",
    region    = "",
    use_https = TRUE,
    base_url  = "api-minio.ine.gob.cl",
    url_style = "path"
  )
})
