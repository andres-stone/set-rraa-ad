# construccion indice ingreso laboral formal (iilf)
# agregacion nacional

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
  s3_url_style = "path",
  s3_use_ssl = TRUE,
  s3_uploader_thread_limit = 25
)

DBI::dbExecute(con, "SET http_retries = 5")
DBI::dbExecute(con, "SET http_retry_wait_ms = 1000")
DBI::dbExecute(con, "SET http_keep_alive = false")

# rutas 
ruta_calculo <-
  "s3://desarrollo/ooee/trl/5_procesamiento/5_8_finalizacion_datos/espejo"

# listado archivos
lista_archivos <-
  dbGetQuery(
    con,
    glue(
      "SELECT file
       FROM glob('{ruta_calculo}/**/*.parquet')
       ORDER BY file"
    )
  )$file

# bucket autenticado
bucket <-
  arrow::s3_bucket(
    "desarrollo",
    access_key = Sys.getenv("ACCESS"),
    secret_key = Sys.getenv("SECRET"),
    endpoint_override = "api-minio.ine.gob.cl",
    region = "us-east-1",
    scheme = "https"
  )

# funcion principal para calcular indicadores ----
calcular_indicadores <-
  function(archivo, bucket) {
    
    # fecha
    fecha_archivo <-
      ymd(
        paste0(
          substr(
            basename(archivo),
            5,
            10
          ),
          "01"
        )
      )
    
    cat(
      "\nProcesando:",
      format(fecha_archivo, "%Y-%m"),
      "\n"
    )
    
    # ruta bucket
    ruta_en_bucket <-
      sub(
        "^s3://desarrollo/",
        "",
        archivo
      )
    
    # lectura parquet
    ds <-
      arrow::read_parquet(
        bucket$path(ruta_en_bucket),
        as_data_frame = FALSE
      )
    
    # agregacion
    ds %>%
      
      filter(
        !seccion_ciiu4cl %in% c("T", "U") |
          is.na(seccion_ciiu4cl)
      ) %>%
      
      select(
        id_ine_id_trabajador,
        id_ine_id_empresa,
        monto_remuneracion,
        n_dias_trabajados
      ) %>%
      
      mutate(
        
        # puesto trabajo
        pt_id =
          paste0(
            id_ine_id_trabajador,
            "_",
            id_ine_id_empresa
          ),
        
        # salario diario
        salario_diario =
          if_else(
            n_dias_trabajados > 0,
            monto_remuneracion /
              n_dias_trabajados,
            NA_real_
          ),
        
        # masa ajustada
        masa_salarial_ajustada =
          monto_remuneracion *
          (
            n_dias_trabajados / 30
          )
      ) %>%
      
      summarise(
        
        # empleo formal
        empleo_formal =
          n_distinct(pt_id),
        
        # masa salarial
        masa_salarial =
          sum(
            monto_remuneracion,
            na.rm = TRUE
          ),
        
        # masa salarial ajustada
        masa_salarial_ajustada =
          sum(
            masa_salarial_ajustada,
            na.rm = TRUE
          ),
        
        # salarios
        salario_promedio =
          mean(
            monto_remuneracion,
            na.rm = TRUE
          ),
        
        salario_mediano =
          median(
            monto_remuneracion,
            na.rm = TRUE
          ),
        
        # salario diario
        salario_diario_promedio =
          mean(
            salario_diario,
            na.rm = TRUE
          ),
        
        # intensidad laboral
        dias_promedio =
          mean(
            n_dias_trabajados,
            na.rm = TRUE
          ),
        
        dias_totales =
          sum(
            n_dias_trabajados,
            na.rm = TRUE
          )
      ) %>%
      
      mutate(
        refdate = fecha_archivo
      ) %>%
      
      collect()
  }

# construccion tabla longitudinal ----
tbl_iilf_nacional <-
  
  purrr::map_dfr(
    lista_archivos,
    calcular_indicadores,
    bucket = bucket
  ) %>%
  
  arrange(refdate)

# # validacion ----
# summary(tbl_iilf_nacional$empleo_formal)

# guardar parquet local ----
arrow::write_parquet(
  tbl_iilf_nacional,
  "output/otros/ad-pib/tbl_iilf_nacional.parquet"
)