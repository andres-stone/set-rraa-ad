
# procesamiento 
# obtencion datos sociodemograficos rc

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
ruta_rep <- "s3://activos/infraestructura/rep/pseudonimizado"
ruta_rep_nuevo <- "s3://desarrollo/ooee/trl/rep"
ruta_suseso_sx <- "s3://desarrollo/ooee/trl/5_procesamiento/5_2_clasificacion_codificacion/espejo/ultimo_sexo_suseso.parquet"
ruta_personas  <- "s3://desarrollo/ooee/trl/5_procesamiento/5_2_clasificacion_codificacion/espejo/pqt_personas_nuevo.parquet"

# obtencion de datasets ----
# rep
# demografica
DBI::dbExecute(
  con,
  glue::glue(
    "CREATE OR REPLACE TEMPORARY VIEW DEMOGRAFICA AS
    SELECT *
    FROM parquet_scan('{ruta_rep}/demographic_table_2024.parquet')"
  )
)

# geografica
DBI::dbExecute(
  con,
  glue::glue(
    "CREATE OR REPLACE TEMPORARY VIEW GEOGRAFICA AS
    SELECT *
    FROM parquet_scan('{ruta_rep}/geographic_table_2024.parquet')"
  )
)

# nuevo rep
dt_rep_nuevo <- 
  duckdbfs::open_dataset(
    ruta_rep_nuevo,
    format = "parquet"
  )

# preprocesamiento ----
# rep
dt_demografica <- tbl(con, "demografica")
dt_geografica  <- tbl(con, "geografica")

# dt_demografica 
dt_demografica <-
  dt_demografica %>% 
  mutate(
    sexo_rc = if_else(sexo %in% c(1, 2), sexo, NA_real_),
    nacionalidad_rc = if_else(nacionalidad==3, 1, nacionalidad)
  ) %>% 
  select(
    id_ine, sexo, nacionalidad_rc,
    fecha_nac, fecha_def_cor_rc
  )

# dt geografica
dt_geografica <- 
  dt_geografica %>% 
  select(
    id_ine, codigo_region, codigo_comuna
  )

# REP 2024
dt_personas_rc <- 
  dt_demografica %>% 
  full_join(
    dt_geografica,
    by = "id_ine"
  )

# REP 2025
dt_rep_nuevo <-
  dt_rep_nuevo %>% 
  select(
    id_ine = id_ine_run_dv,
    sexo = sexo_rc,
    nacionalidad_rc,
    fecha_nac = fecha_nac_cor_rc,
    fecha_def_cor_rc,
    codigo_region = codigo_region_rc,
    codigo_comuna = codigo_comuna_rc
  ) %>% 
  mutate(
    nacionalidad_rc = case_when(
      nacionalidad_rc == 2 ~ 1,
      nacionalidad_rc == 3 ~ 2,
      TRUE ~ nacionalidad_rc
    )
  )

# personas que esstan en 2025 y no en 2024
dt_rep_anti <- 
  dt_rep_nuevo %>% 
  anti_join(
    dt_personas_rc %>% 
      select(id_ine),
    by = "id_ine"
  ) 

dt_rep_2024 <- dt_personas_rc
dt_rep_2025 <- dt_rep_anti

# completitud del dato ----

# REP completo
dt_rep <- 
  dt_rep_2024 %>% 
  mutate(
    codigo_region = as.character(codigo_region),
    codigo_comuna = as.character(codigo_comuna)
  ) %>% 
  union_all(
    dt_rep_2025 %>% 
      mutate(
        codigo_region = as.character(codigo_region),
        codigo_comuna = as.character(codigo_comuna)
      )
  )

# sx suseso
dt_personas_suseso <-
  duckdbfs::open_dataset(
    ruta_suseso_sx,
    format  = "parquet",
    recursive = F
  )
 
dt_personas_suseso <-
  dt_personas_suseso %>%
  select(
    id_ine_id_trabajador,
    sexo_final
  )

# completitud de datos
dt_personas <-
  dt_rep %>%
  full_join(
    dt_personas_suseso,
    by = c("id_ine" = "id_ine_id_trabajador")
  )

dt_personas <-
  dt_personas %>%
  mutate(
    sexo_ = if_else(
      is.na(sexo) | !sexo %in% 1:2,
      as.double(sexo_final),
      sexo
    )
  ) %>%
  select(
    id_ine, fecha_nac, fecha_def_cor_rc,
    sexo_, nacionalidad_rc, codigo_region, codigo_comuna
  ) %>%
  rename(
    sexo = sexo_,
    nacionalidad = nacionalidad_rc
  )

tictoc::tic()
duckdbfs::write_dataset(
  dt_personas,
  path = ruta_personas,
  format = "parquet"
)
tictoc::toc()

