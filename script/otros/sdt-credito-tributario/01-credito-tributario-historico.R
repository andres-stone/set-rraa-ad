
# evaluacion credito fiscal historico

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

# insumos ----
dt_parametros <- 
  read_excel(
    "insumos/utm_usd.xlsx"
  ) %>% 
  mutate(
    fecha = ymd(fecha),
    usd = as.numeric(usd),
    utm = as.numeric(utm)
  )

# funcion de estimacion ----

procesar_credito_tributario <-
  function(archivo, dt_parametros, bucket) {
    

    # fecha archivo
    fecha_archivo <-
      ymd(
        paste0(
          substr(basename(archivo), 5, 10),
          "01"
        )
      )
    
    # mensaje progreso
    cat(
      "\nProcesando:",
      format(fecha_archivo, "%Y-%m"),
      " | ",
      basename(archivo),
      "\n"
    )

    # parametros mensuales
    pars <-
      dt_parametros %>%
      filter(fecha == fecha_archivo)
    
    utm_mes <- pars$utm
    usd_mes <- pars$usd
    
    li_utm <- 7.8 * utm_mes
    ls_utm <- 12 * utm_mes
    

    # ruta bucket
    ruta_en_bucket <-
      sub("^s3://desarrollo/", "", archivo)
    

    # lectura parquet
    ds <-
      arrow::read_parquet(
        bucket$path(ruta_en_bucket),
        as_data_frame = FALSE
      )
    

    # procesamiento
    dt_mes <-
      ds %>%
      filter(
        ep == 0,
        tamano != "unipersonal",
        !seccion_ciiu4cl %in% c("T", "U") | is.na(seccion_ciiu4cl)
      ) %>%
      select(
        id_ine_id_trabajador,
        id_ine_id_empresa,
        sexo,
        nacionalidad,
        codigo_region,
        tramo_edad,
        seccion_ciiu4cl,
        tamano_empresa_movil,
        monto_remuneracion
      ) %>%
      mutate(
        
        # puesto trabajo
        pt_ =
          paste0(
            id_ine_id_trabajador,
            id_ine_id_empresa
          ),
        
        # parametros
        li = li_utm,
        ls = ls_utm,
        
        # tasa credito
        tasa_ct =
          0.15 *
          (ls - monto_remuneracion) /
          (ls - li),
        
        tasa_ct =
          pmax(
            0,
            pmin(tasa_ct, 0.15)
          ),
        
        # credito
        ct =
          monto_remuneracion * tasa_ct,
        
        # usd
        monto_usd =
          monto_remuneracion / usd_mes,
        
        ct_usd =
          ct / usd_mes
      ) %>%
      filter(
        monto_remuneracion >= li,
        monto_remuneracion <= ls
      ) %>%
      collect()
    

    # puestos trabajo
    group_pt <-
      dt_mes %>%
      distinct(
        id_ine_id_trabajador,
        id_ine_id_empresa,
        .keep_all = TRUE
      ) %>%
      group_by(
        sexo,
        nacionalidad,
        codigo_region,
        tramo_edad,
        tamano_empresa_movil,
        seccion_ciiu4cl
      ) %>%
      summarise(
        puestos_de_trabajo =
          n(),
        
        ct_total_clp =
          sum(ct, na.rm = TRUE),
        
        ct_total_usd =
          sum(ct_usd, na.rm = TRUE),
        
        .groups = "drop"
      ) %>%
      mutate(
        fecha = fecha_archivo
      )
    

    # costo laboral empresa
    group_cl <-
      dt_mes %>%
      group_by(
        id_ine_id_empresa,
        tamano_empresa_movil,
        seccion_ciiu4cl
      ) %>%
      summarise(
        cl_clp =
          sum(
            monto_remuneracion,
            na.rm = TRUE
          ),
        
        ct_clp =
          sum(
            ct,
            na.rm = TRUE
          ),
        
        cl_usd =
          sum(
            monto_usd,
            na.rm = TRUE
          ),
        
        ct_usd =
          sum(
            ct_usd,
            na.rm = TRUE
          ),
        
        .groups = "drop"
      ) %>%
      mutate(
        fecha = fecha_archivo
      )
    
    gc()
    
    return(
      list(
        pt = group_pt,
        cl = group_cl
      )
    )
  }

# calculo ----
resultados <- 
  purrr::map(
    lista_archivos,
    procesar_credito_tributario,
    dt_parametros = dt_parametros,
    bucket = bucket
  )

# consolidar resultados ----
# puestos de trabajo
tbl_pt <-
  purrr::map_dfr(
    resultados, 
    "pt"
  )

# costos laborales
tbl_cl <- 
  purrr::map_dfr(
    resultados,
    "cl"
  )

# guardar datos ----
arrow::write_parquet(
  tbl_pt,
  "output/credito-tributario/tbl_pt_credito_tributario.parquet"
)

arrow::write_parquet(
  tbl_cl,
  "output/credito-tributario/tbl_cl_credito_tributario.parquet"
)
