
# conteo 
# bases mensuales flujos laborales

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

# ruta salida
ruta_final <- "s3://desarrollo/ooee/trl/5_procesamiento/5_8_finalizacion_datos/es/202512"
ruta_rep <- "s3://activos/infraestructura/rep/pseudonimizado"
ruta_rep_nuevo <- "s3://desarrollo/ooee/trl/rep"

# obtencion datasets ----
dt_suseso <-
  duckdbfs::open_dataset(
    ruta_final,
    format = "parquet",
    unify_schemas = TRUE
  )

# rep demografica 2024
DBI::dbExecute(
  con,
  glue::glue(
    "CREATE OR REPLACE TEMPORARY VIEW DEMOGRAFICA AS
    SELECT *
    FROM parquet_scan('{ruta_rep}/demographic_table_2024.parquet')"
  )
)

# rep geografica 2024
DBI::dbExecute(
  con,
  glue::glue(
    "CREATE OR REPLACE TEMPORARY VIEW GEOGRAFICA AS
    SELECT *
    FROM parquet_scan('{ruta_rep}/geographic_table_2024.parquet')"
  )
)

# rep geografica 2024
DBI::dbExecute(
  con,
  glue::glue(
    "CREATE OR REPLACE TEMPORARY VIEW REP AS
    SELECT *
    FROM parquet_scan('{ruta_rep_nuevo}/rep_base_rc2025_det.parquet')"
  )
)

# SI dic 2025 ----

# depuracion rep 2024 

dt_rep_2024_demografica <- tbl(con, "demografica")
dt_rep_2024_geografica  <- tbl(con, "geografica")
dt_rep_2025 <- tbl(con, "REP")

# dt_demografica 
dt_rep_2024_demografica <-
  dt_rep_2024_demografica %>% 
  mutate(
    nacionalidad_rep_2024 = if_else(nacionalidad==3, 1, nacionalidad)
  ) %>% 
  select(
    id_ine, nacionalidad_rep_2024
  )

# dt geografica
dt_rep_2024_geografica <- 
  dt_rep_2024_geografica %>% 
  select(
    id_ine, 
    region_rep_2024 = codigo_region
  )

dt_rep_2025 <-
  dt_rep_2025 %>% 
  select(
    id_ine_run_dv,
    nacionalidad_rep_2025 = nacionalidad_rc,
    region_rep_2025 = codigo_region_rc
  ) %>% 
  mutate(
    nacionalidad_rep_2025 = case_when(
      nacionalidad_rep_2025 == 2 ~ 1,
      nacionalidad_rep_2025 == 3 ~ 2,
      TRUE ~ nacionalidad_rep_2025
    )
  )

dt_rep_2024_demografica <- dt_rep_2024_demografica %>% collect() %>% as.data.table()
dt_rep_2024_geografica <- dt_rep_2024_geografica %>% collect() %>% as.data.table()
dt_rep_2025 <- dt_rep_2025 %>% collect() %>% as.data.table()

dt_202512 <-
  dt_suseso %>%
  filter(
    anno_devengamiento_remuneracion == 2025,
    mes_devengamiento_remuneracion == 12
  ) %>%
  select(
    id_ine_id_trabajador,
    nacionalidad_final,
    region_final
  ) %>%
  distinct() %>%
  collect() %>% 
  left_join(
    dt_rep_2024_demografica,
    by = c("id_ine_id_trabajador" = "id_ine")
  ) %>%
  left_join(
    dt_rep_2024_geografica,
    by = c("id_ine_id_trabajador" = "id_ine")
  ) %>%
  left_join(
    dt_rep_2025,
    by = c("id_ine_id_trabajador" = "id_ine_run_dv")
  )

rm(
  dt_rep_2024_demografica,
  dt_rep_2024_geografica,
  dt_rep_2025
)
gc()

# %>%
#   left_join(
#     dt_rep_2024_demografica,
#     by = c("id_ine_id_trabajador" = "id_ine")
#   ) %>%
#   left_join(
#     dt_rep_2024_geografica,
#     by = c("id_ine_id_trabajador" = "id_ine")
#   ) %>% 
#   left_join(
#     dt_rep_2025,
#     by = c("id_ine_id_trabajador" = "id_ine_run_dv")
#   ) %>% 
#   mutate(
#     nacionalidad_rep_2025 == as.character(nacionalidad_rep_2025),
#     nacionalidad_rep_2024 == as.character(nacionalidad_rep_2024),
#     region_rep_2025 == as.character(region_rep_2025),
#     region_rep_2024 == as.character(region_rep_2024)
#   )

table(
  dt_202512$region_rep_2024,
  dt_202512$region_rep_2025,
  useNA = "ifany"
)

table(
  dt_202512$region_rep_2024,
  dt_202512$region_final,
  useNA = "ifany"
)

table(
  dt_202512$nacionalidad_rep_2024,
  dt_202512$nacionalidad_rep_2025,
  useNA = "ifany"
)

table(
  dt_202512$nacionalidad_rep_2024,
  dt_202512$nacionalidad_final,
  useNA = "ifany"
)

# caracterizando SI ----

# depuracion rep 2024 
dt_rep_2024_demografica <- tbl(con, "demografica")
dt_rep_2024_geografica  <- tbl(con, "geografica")
dt_rep_2025 <- tbl(con, "REP")

# dt_demografica 
dt_rep_2024_demografica <-
  dt_rep_2024_demografica %>% 
  mutate(
    nacionalidad_rep_2024 = if_else(nacionalidad==3, 1, nacionalidad)
  ) %>% 
  select(
    id_ine, nacionalidad_rep_2024
  )

# dt geografica
dt_rep_2024_geografica <- 
  dt_rep_2024_geografica %>% 
  select(
    id_ine, 
    region_rep_2024 = codigo_region
  )

dt_si <-
  dt_suseso %>%
  select(
    anno_devengamiento_remuneracion,
    mes_devengamiento_remuneracion,
    id_ine_id_trabajador,
    id_ine_id_empresa
  ) %>%
  distinct() %>%
  left_join(
    dt_rep_2024_demografica,
    by = c("id_ine_id_trabajador" = "id_ine")
  ) %>%
  left_join(
    dt_rep_2024_geografica,
    by = c("id_ine_id_trabajador" = "id_ine")
  )

# SI de nacionalidad
dt_si_nc <- 
  dt_si %>% 
  filter(
    is.na(nacionalidad_rep_2024)
  ) 

pt_si_nc_1 <-
  dt_si_nc %>% 
  group_by(
    anno_devengamiento_remuneracion,
    mes_devengamiento_remuneracion
  ) %>% 
  summarise(
    pt = n_distinct(
      paste0(
        id_ine_id_trabajador,
        id_ine_id_empresa
      )
    )
  ) %>% 
  arrange(
    anno_devengamiento_remuneracion,
    mes_devengamiento_remuneracion
  ) %>% 
  collect()

pt_si_nc_2 <-
  dt_si_nc %>% 
  group_by(
    id_ine_id_trabajador,
    id_ine_id_empresa
  ) %>% 
  summarise(
    tt = n_distinct(
      paste0(
        anno_devengamiento_remuneracion,
        mes_devengamiento_remuneracion
      )
    )
  ) %>% 
  collect()

pt_si_nc_3 <-
  dt_si_nc %>%
  filter(
    anno_devengamiento_remuneracion==2025
  ) %>% 
  group_by(
    id_ine_id_trabajador,
    id_ine_id_empresa
  ) %>% 
  summarise(
    tt = n_distinct(
      paste0(
        anno_devengamiento_remuneracion,
        mes_devengamiento_remuneracion
      )
    )
  ) %>% 
  arrange(
    -tt
  ) %>% 
  collect()

write_parquet(pt_si_nc_1, "output/trl/imputacion/pt_si_nc_1.parquet")
write_parquet(pt_si_nc_2, "output/trl/imputacion/pt_si_nc_2.parquet")
write_parquet(pt_si_nc_3, "output/trl/imputacion/pt_si_nc_3.parquet")

dt_nc <-
  dt_suseso %>%
  select(
    anno_devengamiento_remuneracion,
    mes_devengamiento_remuneracion,
    id_ine_id_trabajador,
    id_ine_id_empresa,
    nacionalidad_final
  ) %>%
  distinct()

# SI de nacionalidad
dt_ci_nc <- 
  dt_nc %>% 
  filter(
    is.na(nacionalidad_final)
  ) 

pt_ci_nc_1 <-
  dt_ci_nc %>% 
  group_by(
    anno_devengamiento_remuneracion,
    mes_devengamiento_remuneracion
  ) %>% 
  summarise(
    pt = n_distinct(
      paste0(
        id_ine_id_trabajador,
        id_ine_id_empresa
      )
    )
  ) %>% 
  arrange(
    anno_devengamiento_remuneracion,
    mes_devengamiento_remuneracion
  ) %>% 
  collect()

write_parquet(pt_ci_nc_1, "output/trl/imputacion/pt_ci_nc_1.parquet")
