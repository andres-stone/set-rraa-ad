
# ultimo sexo y nacionalidad consistente

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
# DBI::dbExecute(con, "SET s3_uploader_thread_limit = 10")  # Limita la concurrencia de subida

# rutas 
# insumos
ruta_suseso <- "s3://activos/rraa_oae/suseso/trabajadores_protegidos/pseudonimizado/"
ruta_sexo_1 <- "s3://desarrollo/ooee/trl/5_procesamiento/5_2_clasificacion_codificacion/espejo/sexos_consistentes_suseso.parquet"
ruta_sexo_2 <- "s3://desarrollo/ooee/trl/5_procesamiento/5_2_clasificacion_codificacion/espejo/ultimo_sexo_suseso.parquet"

# obtencion de datasets ----
# Suseso
dt_suseso <- 
  duckdbfs::open_dataset(
    ruta_suseso,
    format = "parquet",
    unify_schemas = T
  )

# obtencion del ultimo dato

# preprocesamietno
dt_personas <- 
  dt_suseso %>% 
  select(
    id_ine_id_trabajador, 
    id_ine_id_empresa,
    anno,
    mes,
    sexo
  )

# contar sexo consistente por mes
dt_personas <-
  dt_personas %>% 
  select(
    id_ine_id_trabajador,
    anno,
    mes,
    sexo
  ) %>% 
  summarise(
    min_sexo = min(sexo, na.rm = T),
    max_sexo = max(sexo, na.rm = T),
    .by = c(
      anno, mes, id_ine_id_trabajador
    )
  ) %>% 
  filter(
    min_sexo == max_sexo
  ) %>% 
  rename(
    sexo_final = min_sexo
  ) %>% 
  select(
    id_ine_id_trabajador, anno, mes, sexo_final
  )

duckdbfs::write_dataset(
  dt_personas,
  path = ruta_sexo_1,
  format = "parquet"
)

# quedandonos con el ultimo sexo ----

dt_sx_consistente <- 
  duckdbfs::open_dataset(
    ruta_sexo_1,
    format = "parquet",
    recursive = F
  ) %>% 
  mutate(
    periodo = as.integer(anno) * 100 + as.integer(mes)
  )

dt_ultimo_periodo_consistente <-
  dt_sx_consistente %>% 
  select(
    id_ine_id_trabajador, periodo
  ) %>% 
  summarise(
    periodo = max(periodo),
    .by = id_ine_id_trabajador
  )

dt_ultimo_consistente <-
  dt_sx_consistente %>% 
  inner_join(
    dt_ultimo_periodo_consistente,
    by = c(
      "id_ine_id_trabajador",
      "periodo"
    )
  )

tictoc::tic()
duckdbfs::write_dataset(
  dt_ultimo_consistente,
  path = ruta_sexo_2,
  format = "parquet"
)
tictoc::toc()

