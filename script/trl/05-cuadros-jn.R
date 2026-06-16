
# procesamiento 
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
library(aws.s3)

# conexion duckdb 
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

# credenciales para aws.s3
Sys.setenv(
  AWS_ACCESS_KEY_ID = Sys.getenv("ACCESS"),
  AWS_SECRET_ACCESS_KEY = Sys.getenv("SECRET")
)

# ruta archivo 
archivo_rds <- "ooee/trl/6_analisis/6_1_preparacion/202604/resultados_trl.rds"

# archivo temporal local
temp_rds <- tempfile(fileext = ".rds")

# descarga desde MinIO
aws.s3::save_object(
  object = archivo_rds,
  bucket = "desarrollo",
  file = temp_rds,
  base_url = "api-minio.ine.gob.cl",
  use_https = TRUE,
  region = "",
  check_region = FALSE
)

# lectura del RDS
dt_resultados_trl <- readRDS(temp_rds)

# indicadores JN ----

# boletin
tbl_general <- dt_resultados_trl$indicadores_general
tbl_seccion <- dt_resultados_trl$indicadores_seccion
tbl_tamano  <- dt_resultados_trl$indicadores_tamano
tbl_sexo    <- dt_resultados_trl$indicadores_sexo
tbl_nacionalidad <- dt_resultados_trl$indicadores_nacionalidad

# nuevos
tbl_region <- dt_resultados_trl$indicadores_region
tbl_tramos <- dt_resultados_trl$indicadores_edad

# empleo publico
tbl_ep <- dt_resultados_trl$indicadores_ep

# excel
# 1. general
tbl_general <-
  tbl_general %>% 
  filter(
    indicador %in% c(
      "t_ent", "var1_t_ent", "var12_t_ent",
      "t_sal", "var1_t_sal", "var12_t_sal",
      "t_rot", "var1_t_rot", "var12_t_rot",
      "t_rot_neta", 
      "t_per", "var1_t_per", "var12_t_per"
    )
  ) %>% 
  arrange(
    anno_dev.cur, mes_dev.cur
  ) %>% 
  filter(
    !is.na(valor)
  )

# 2. seccion
tbl_seccion <-
  tbl_seccion %>% 
  filter(
    indicador %in% c(
      "t_ent", "var12_t_ent",
      "t_sal", "var12_t_sal",
      "t_rot", "var12_t_rot",
      "t_rot_neta", "inc_trln",
      "t_per", "var12_t_per"
    )
  ) %>% 
  arrange(
    anno_dev.cur, mes_dev.cur, seccion
  ) %>% 
  filter(
    !is.na(valor)
  )

# 3. tamano
tbl_tamano <-
  tbl_tamano %>% 
  filter(
    indicador %in% c(
      "t_ent", "var12_t_ent",
      "t_sal", "var12_t_sal",
      "t_rot", "var12_t_rot",
      "t_rot_neta", "inc_trln",
      "t_per", "var12_t_per"
    )
  ) %>% 
  arrange(
    anno_dev.cur, mes_dev.cur, tamano
  ) %>% 
  filter(
    !is.na(valor)
  )

# 4. sexo
tbl_sexo <-
  tbl_sexo %>% 
  filter(
    indicador %in% c(
      "t_ent", "var12_t_ent",
      "t_sal", "var12_t_sal",
      "t_rot", "var12_t_rot",
      "t_rot_neta", "inc_trln",
      "t_per", "var12_t_per"
    )
  ) %>% 
  arrange(
    anno_dev.cur, mes_dev.cur, sexo
  ) %>% 
  filter(
    !is.na(valor)
  )

# 5. nacionalidad
tbl_nacionalidad <-
  tbl_nacionalidad %>% 
  filter(
    indicador %in% c(
      "t_ent", "var12_t_ent",
      "t_sal", "var12_t_sal",
      "t_rot", "var12_t_rot",
      "t_rot_neta", "inc_trln",
      "t_per", "var12_t_per"
    )
  ) %>% 
  arrange(
    anno_dev.cur, mes_dev.cur, nacionalidad
  ) %>% 
  filter(
    !is.na(valor)
  )

# 6. region
tbl_region <-
  tbl_region %>% 
  filter(
    indicador %in% c(
      "t_ent", "var12_t_ent",
      "t_sal", "var12_t_sal",
      "t_rot", "var12_t_rot",
      "t_rot_neta", "inc_trln",
      "t_per", "var12_t_per"
    )
  ) %>% 
  arrange(
    anno_dev.cur, mes_dev.cur, region_trabajador
  ) %>% 
  filter(
    !is.na(valor)
  )

# 7. tramos
tbl_tramos <-
  tbl_tramos %>% 
  filter(
    indicador %in% c(
      "t_ent", "var12_t_ent",
      "t_sal", "var12_t_sal",
      "t_rot", "var12_t_rot",
      "t_rot_neta", "inc_trln",
      "t_per", "var12_t_per"
    )
  ) %>% 
  arrange(
    anno_dev.cur, mes_dev.cur, edad
  ) %>% 
  filter(
    !is.na(valor)
  )

# 8. empleo publico
tbl_ep <- 
  tbl_ep %>% 
  filter(
    indicador %in% c(
      "pt_cur", "var1_pt_cur", "var12_pt_cur"
    )
  ) %>% 
  arrange(
    anno_dev.cur, mes_dev.cur
  ) %>% 
  filter(
    ep == 1,
    !is.na(valor)
  )

# guardar resultados ----
write_xlsx(tbl_general, "output/trl-jn/trl_general.xlsx")
write_xlsx(tbl_seccion, "output/trl-jn/trl_seccion.xlsx")
write_xlsx(tbl_tamano, "output/trl-jn/trl_tamano.xlsx")
write_xlsx(tbl_sexo, "output/trl-jn/trl_sexo.xlsx")
write_xlsx(tbl_nacionalidad, "output/trl-jn/trl_nacionalidad.xlsx")
write_xlsx(tbl_region, "output/trl-jn/trl_region.xlsx")
write_xlsx(tbl_tramos, "output/trl-jn/trl_tramos.xlsx")
write_xlsx(tbl_ep, "output/trl-jn/pt_ep.xlsx")


