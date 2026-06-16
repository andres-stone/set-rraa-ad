
# procesamiento 
# envejecimiento demografico

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

bucket <- 
  arrow::s3_bucket(
    "desarrollo",
    access_key        = Sys.getenv("ACCESS"),
    secret_key        = Sys.getenv("SECRET"),
    endpoint_override = "api-minio.ine.gob.cl",
    region            = "us-east-1",
    scheme            = "https"
  )

# ruta
ruta_calculo <-
  "s3://desarrollo/ooee/trl/5_procesamiento/5_8_finalizacion_datos/es/202512"

lista_archivos <- dbGetQuery(
  con,
  glue("
       SELECT file
       FROM glob('{ruta_calculo}/**/*.parquet')
       ORDER BY file
       ")
)$file

fechas_vec <- ymd(
  paste0(
    substr(basename(lista_archivos), 5, 10),
    "01"
  )
)

names(lista_archivos) <- as.character(fechas_vec)

# funciones de procesamiento ----

cols_lectura <- 
  c(
    "id_ine_id_trabajador",
    "id_ine_id_empresa",
    "sexo",
    "nacionalidad_final",
    "region_final",
    "tramo_edad_2"
  )

leer_mes <- 
  function(archivo){
    ruta_en_bucket <-
      sub("^s3://desarrollo/", "", archivo)
    
    arrow::read_parquet(
      bucket$path(ruta_en_bucket),
      as_data_frame = FALSE
    ) %>%
      filter(
        tamano != "unipersonal",
        !seccion_ciiu_4cl %in% c("T","U") |
          is.na(seccion_ciiu_4cl)
      ) %>%
      select(all_of(cols_lectura)) %>%
      collect() %>%
      distinct(
        id_ine_id_trabajador,
        id_ine_id_empresa,
        .keep_all = TRUE
      )
  }

# ------------------------------------------------------------
# calcular_tmn generalizada para 1..N variables de desagregacion
# 
# variables: character vector, e.g. c("tramo_edad_2") o
#            c("tramo_edad_2", "nacionalidad_final")
#
# El resultado incluye una columna por cada variable del grupo,
# más las métricas: In, Em, L_t12, L_t, tasas, tmn, var_12.
# La columna `desagregacion` identifica la combinacion usada.
# ------------------------------------------------------------

calcular_tmn <- 
  function(df_t, df_t12, variables, fecha_t){
    
    # join por identidad del vínculo laboral
    base_comun <-
      inner_join(
        df_t,
        df_t12,
        by  = c("id_ine_id_trabajador", "id_ine_id_empresa"),
        suffix = c("_t", "_t12")
      )
    
    # columnas con sufijo para t y t-12
    vars_t   <- paste0(variables, "_t")
    vars_t12 <- paste0(variables, "_t12")
    
    # workers que CAMBIARON en al menos una de las variables del grupo
    # (si el grupo es c("tramo_edad_2","sexo"), cambian si alguna difiere)
    cambio_mask <-
      map(
        seq_along(variables),
        ~ base_comun[[vars_t[.x]]] != base_comun[[vars_t12[.x]]]
      ) %>%
      reduce(`|`)
    
    cambios <- base_comun[cambio_mask, ]
    
    # inmigraciones: conteo por categoría EN t
    in_tbl <-
      cambios %>%
      count(
        across(all_of(vars_t)),
        name = "In"
      ) %>%
      rename_with(~ variables, all_of(vars_t))
    
    # emigraciones: conteo por categoría EN t-12
    em_tbl <-
      cambios %>%
      count(
        across(all_of(vars_t12)),
        name = "Em"
      ) %>%
      rename_with(~ variables, all_of(vars_t12))
    
    # stock t-12
    stock_t12_tbl <-
      df_t12 %>%
      count(
        across(all_of(variables)),
        name = "L_t12"
      )
    
    # stock t
    stock_t_tbl <-
      df_t %>%
      count(
        across(all_of(variables)),
        name = "L_t"
      )
    
    resultado <-
      full_join(in_tbl,       em_tbl,        by = variables) %>%
      full_join(stock_t12_tbl, by = variables) %>%
      full_join(stock_t_tbl,   by = variables) %>%
      mutate(
        In     = coalesce(In,     0L),
        Em     = coalesce(Em,     0L),
        L_t    = coalesce(L_t,    0L),
        L_t12  = coalesce(L_t12,  0L),
        fecha  = fecha_t,
        anno   = lubridate::year(fecha_t),
        mes    = lubridate::month(fecha_t),
        # etiqueta legible de la desagregación usada
        desagregacion        = paste(variables, collapse = " x "),
        tasa_inmigracion     = if_else(L_t12 > 0, In  / L_t12, NA_real_),
        tasa_emigracion      = if_else(L_t12 > 0, Em  / L_t12, NA_real_),
        tmn                  = if_else(L_t12 > 0, (In - Em) / L_t12, NA_real_),
        var_12               = if_else(L_t12 > 0, (L_t - L_t12) / L_t12, NA_real_)
      ) %>%
      relocate(
        anno, mes, fecha,
        desagregacion,
        all_of(variables)   # columnas de grupo al frente
      )
    
    resultado
  }

# ------------------------------------------------------------
# definicion de desagregaciones a calcular
#
# Cada elemento de la lista es un vector de variables.
# Agrega o quita combinaciones libremente.
# ------------------------------------------------------------

combinaciones_tmn <- list(
  # simples (equivalentes al codigo original)
  c("sexo"),
  c("nacionalidad_final"),
  c("region_final"),
  c("tramo_edad_2"),
  # dobles con tramo_edad_2
  c("tramo_edad_2", "sexo"),
  c("tramo_edad_2", "nacionalidad_final"),
  c("tramo_edad_2", "region_final"),
  # triple
  c("tramo_edad_2", "sexo", "nacionalidad_final"),
  c("tramo_edad_2", "sexo", "region_final"),
  # cuadruple
  c("tramo_edad_2", "sexo", "nacionalidad_final", "region_final")
)

# procesamiento ----

indices <- 13:length(lista_archivos)

tictoc::tic()
resultado_tmn <-
  map_dfr(
    indices,
    function(idx){
      
      fecha_t <- ymd(names(lista_archivos)[idx])
      
      message(glue("Procesando {fecha_t}"))
      
      df_t   <- leer_mes(lista_archivos[idx])
      df_t12 <- leer_mes(lista_archivos[idx - 12])
      
      # calcular todas las combinaciones para este par de meses
      map_dfr(
        combinaciones_tmn,
        ~ calcular_tmn(
          df_t      = df_t,
          df_t12    = df_t12,
          variables = .x,
          fecha_t   = fecha_t
        )
      )
    }
  )
tictoc::toc()

# guardando datos ----

# una hoja por combinacion de desagregacion
desags_unicas <- unique(resultado_tmn$desagregacion)

wb <- createWorkbook()

for(d in desags_unicas){
  
  # nombre de hoja: reemplazar " x " por "_" y truncar a 31 chars
  nombre_hoja <- substr(
    str_replace_all(d, " x ", "_"),
    1, 31
  )
  
  addWorksheet(wb, nombre_hoja)
  
  writeData(
    wb,
    sheet = nombre_hoja,
    resultado_tmn %>% filter(desagregacion == d)
  )
}

saveWorkbook(
  wb,
  "output/trl/migracion/tmn_dominios.xlsx",
  overwrite = TRUE
)

saveRDS(
  resultado_tmn,
  "output/trl/migracion/tmn_dominios.rds"
)
