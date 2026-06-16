
# cuadratura 

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

# obtencion de datos ----
# archivos RP
# ruta archivo 
archivo_rp <- "ooee/trl/6_analisis/6_1_preparacion/202605/resultados_trl.rds"

# archivo temporal local
temp_rds <- tempfile(fileext = ".rds")

# descarga desde MinIO RP
aws.s3::save_object(
  object = archivo_rp,
  bucket = "desarrollo",
  file = temp_rds,
  base_url = "api-minio.ine.gob.cl",
  use_https = TRUE,
  region = "",
  check_region = FALSE
)

# lectura del RDS
dt_resultados_rp <- readRDS(temp_rds)

# archivos ad
# ruta archivo
archivo_ad <- "ooee/trl/6_analisis/6_1_preparacion/es/202512/tbl_trl_indicadores.rds"

# archivo temporal local
temp_rds <- tempfile(fileext = ".rds")

# descarga desde MinIO AD
aws.s3::save_object(
  object = archivo_ad,
  bucket = "desarrollo",
  file = temp_rds,
  base_url = "api-minio.ine.gob.cl",
  use_https = TRUE,
  region = "",
  check_region = FALSE
)

# lectura del RDS
dt_resultados_ad <- readRDS(temp_rds)

# funciones ----
# renombre estandar a indicadores
recode_indicadores <- 
  function(df) {
    df %>%
      mutate(
        indicador = recode(
          indicador,
          pt        = "pt_cur",
          pt_ent_12 = "pt_ent",
          pt_sal_12 = "pt_sal",
          inc_trln  = "inc_rot_neta"
        )
      )
  }

# funcion generica de comparacion
compare_flujos <- 
  function(
    tbl_ad,
    tbl_rp,
    dim_cols,             # c(nombre_en_ad = "nombre_en_rp")
    rp_rename_time = TRUE,
    filter_na_dims = TRUE,
    extra_mutate_ad = NULL,
    extra_mutate_rp = NULL) {
    
    # preparar _ad
    dt_ad <- 
      tbl_ad %>%
      filter(!is.na(valor)) %>%
      select(-any_of("fecha")) %>%
      recode_indicadores()
    
    if (filter_na_dims) {
      ad_dim_names <- names(dim_cols)
      dt_ad <- 
        dt_ad %>%
        filter(if_all(all_of(ad_dim_names), ~ !is.na(.x)))
    }
    
    if (!is.null(extra_mutate_ad)) dt_ad <- extra_mutate_ad(dt_ad)
    
    # preparar _rp
    dt_rp <- 
      tbl_rp %>%
      filter(!is.na(valor))
    
    # extra_mutate_rp 
    if (!is.null(extra_mutate_rp)) dt_rp <- extra_mutate_rp(dt_rp)
    
    if (rp_rename_time) {
      dt_rp <- dt_rp %>%
        rename(anno = anno_dev.cur, mes = mes_dev.cur)
    }
    
    # Renombrar columnas de dimensión en _rp para que coincidan con _ad
    rp_rename_map <- dim_cols[names(dim_cols) != unname(dim_cols)]
    if (length(rp_rename_map) > 0) {
      rename_vec <- setNames(unname(rp_rename_map), names(rp_rename_map))
      dt_rp <- dt_rp %>% rename(all_of(rename_vec))
    }
    
    # join
    join_cols <- c("anno", "mes", names(dim_cols), "indicador")
    
    dt <- 
      dt_ad %>%
      inner_join(dt_rp, by = join_cols, suffix = c("_ad", "_rp")) %>%
      mutate(d = valor_ad - valor_rp)
    
    dt
  }

# congfiguracion de combinacion
configs <- 
  list(
    # flujos_total (sin dimensiones extra)
    list(
      label       = "general",
      tbl_ad_name = "flujos_total",
      tbl_rp_name = "indicadores_general",
      dim_cols    = character(0)   # sin dimensiones adicionales
    ),
    
    # flujos_sx
    list(
      label       = "sexo",
      tbl_ad_name = "flujos_sx",
      tbl_rp_name = "indicadores_sexo",
      dim_cols    = c(sexo = "sexo")
    ),
    
    # flujos_nc
    # list(
    #   label       = "nacionalidad",
    #   tbl_ad_name = "flujos_nc",
    #   tbl_rp_name = "indicadores_nacionalidad",
    #   dim_cols    = c(nacionalidad = "nacionalidad")
    # ),
    
    # flujos_nc
    list(
      label       = "nacionalidad",
      tbl_ad_name = "flujos_nc",
      tbl_rp_name = "indicadores_nacionalidad",
      dim_cols    = c(nacionalidad_final = "nacionalidad")
    ),
    
    # # flujos_re
    # list(
    #   label       = "region",
    #   tbl_ad_name = "flujos_re",
    #   tbl_rp_name = "indicadores_region",
    #   dim_cols    = c(codigo_region = "region_trabajador"),
    #   extra_mutate_ad = function(df) {
    #     df %>% mutate(codigo_region = as.character(codigo_region))
    #   }
    # ),
    # flujos_re
    list(
      label       = "region",
      tbl_ad_name = "flujos_re",
      tbl_rp_name = "indicadores_region",
      dim_cols    = c(region_final = "region_trabajador"),
      
      extra_mutate_ad = function(df) {
        df %>% 
          mutate(region_final = as.character(region_final))
      },
      
      extra_mutate_rp = function(df) {
        df %>% 
          mutate(region_trabajador = as.character(region_trabajador))
      }
    ),
    
    # flujos_te
    list(
      label       = "edad",
      tbl_ad_name = "flujos_te",
      tbl_rp_name = "indicadores_edad",
      dim_cols    = c(tramo_edad = "edad"),
      extra_mutate_rp = function(df) {
        df %>%
          filter(!is.na(edad)) %>%
          mutate(
            edad = recode(
              edad,
              "0_14"   = 0L,
              "15_24"  = 1L,
              "25_34"  = 2L,
              "35_44"  = 3L,
              "45_54"  = 4L,
              "55_64"  = 5L,
              "65_mas" = 6L
            )
          )
      }
    ),
    
    # flujos_sector
    list(
      label       = "seccion",
      tbl_ad_name = "flujos_sector",
      tbl_rp_name = "indicadores_seccion",
      dim_cols    = c(seccion_ciiu4cl = "seccion")
    ),
    
    # flujos_tamano
    list(
      label       = "tamano",
      tbl_ad_name = "flujos_tamano",
      tbl_rp_name = "indicadores_tamano",
      dim_cols    = c(tamano_empresa_movil = "tamano")
    ),
    
    # flujos_sx_sector (cruce sexo x seccion)
    list(
      label       = "sexo_seccion",
      tbl_ad_name = "flujos_sx_sector",
      tbl_rp_name = "indicadores_seccion_sexo",
      dim_cols    = c(sexo = "sexo", seccion_ciiu4cl = "seccion")
    ),
    
    # flujos_sx_tamano
    list(
      label       = "sexo_tamano",
      tbl_ad_name = "flujos_sx_tamano",
      tbl_rp_name = "indicadores_sexo_tamano",
      dim_cols    = c(sexo = "sexo", tamano_empresa_movil = "tamano")
    ),
    
    # flujos_tamano_sector
    list(
      label       = "tamano_seccion",
      tbl_ad_name = "flujos_tamano_sector",
      tbl_rp_name = "indicadores_seccion_tamano",
      dim_cols    = c(tamano_empresa_movil = "tamano", seccion_ciiu4cl = "seccion")
    ),
    
    # flujos_sx_te
    list(
      label       = "sexo_edad",
      tbl_ad_name = "flujos_sx_te",
      tbl_rp_name = "indicadores_sexo_edad",      # <-- verificar
      dim_cols    = c(sexo = "sexo", tramo_edad = "edad"),
      extra_mutate_rp = function(df) {
        df %>%
          filter(!is.na(edad)) %>%
          mutate(
            edad = recode(
              edad,
              "0_14"   = 0L, "15_24"  = 1L, "25_34"  = 2L,
              "35_44"  = 3L, "45_54"  = 4L, "55_64"  = 5L,
              "65_mas" = 6L
            )
          )
      }
    ),
    
    # flujos_sx_tamano_sector
    list(
      label       = "sexo_tamano_seccion",
      tbl_ad_name = "flujos_sx_tamano_sector",
      tbl_rp_name = "indicadores_seccion_tamano_sexo",
      dim_cols    = c(
        sexo                 = "sexo",
        tamano_empresa_movil = "tamano",
        seccion_ciiu4cl      = "seccion"
      )
    )
  )

# ejecutar codigo ----
tbl_resultados <- 
  configs %>%
  set_names(map_chr(., "label")) %>%
  map(function(cfg) {
    
    tbl_ad <- dt_resultados_ad[[ cfg$tbl_ad_name ]]
    tbl_rp <- dt_resultados_rp[[ cfg$tbl_rp_name ]]
    
    # Saltar si alguna tabla no existe en los datos
    if (is.null(tbl_ad) || is.null(tbl_rp)) {
      message("Saltando '", cfg$label, "': tabla no encontrada.")
      return(NULL)
    }
    
    compare_flujos(
      tbl_ad          = tbl_ad,
      tbl_rp          = tbl_rp,
      dim_cols        = cfg$dim_cols,
      extra_mutate_ad = cfg$extra_mutate_ad,
      extra_mutate_rp = cfg$extra_mutate_rp
    )
  })

# resumen ----
tbl_resumen <- 
  tbl_resultados %>%
  compact() %>%                          # quitar NULLs (tablas no encontradas)
  imap_dfr(function(dt, nm) {
    tibble(
      comparacion  = nm,
      n_filas      = nrow(dt),
      n_dif_nz     = sum(dt$d != 0, na.rm = TRUE),   # diferencias distintas de 0
      max_abs_d    = max(abs(dt$d), na.rm = TRUE),
      mean_abs_d   = mean(abs(dt$d), na.rm = TRUE)
    )
  })

print(tbl_resumen)
