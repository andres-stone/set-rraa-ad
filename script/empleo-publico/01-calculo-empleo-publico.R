
# estimacion empleo publico

# seteo previo ----
rm(list = ls())
options(scipen = 999, digits = 4)
gc()

# librerias 
library(dplyr)
library(duckdb)
library(duckdbfs)
library(purrr)
library(lubridate)
library(glue)
library(tictoc)
library(stringr)
library(openxlsx)

# conexion minio 
con <- cached_connection()
duckdbfs::duckdb_s3_config(
  conn                 = con,
  s3_access_key_id     = Sys.getenv("ACCESS"),
  s3_secret_access_key = Sys.getenv("SECRET"),
  s3_endpoint          = "api-minio.ine.gob.cl",
  s3_region            = "us-east-1",
  s3_url_style         = "path",
  s3_use_ssl           = TRUE
)

# rutas 
# ruta_calculo <- "s3://desarrollo/ooee/trl/5_procesamiento/5_8_finalizacion_datos/espejo"
ruta_calculo <- "s3://desarrollo/ooee/trl/5_procesamiento/5_8_finalizacion_datos/es/202512"

# conexión dataset (lazy)
dt_suseso <-
  duckdbfs::open_dataset(
    ruta_calculo,
    format = "parquet"
  )

# calculo puestos de trabajo ----
tbl_base_maestra <-
  dt_suseso %>%
  filter(
    #is.na(pred_tipo_6),
    ep == 1
  ) %>%
  mutate(
    fecha = make_date(
      as.integer(anno_devengamiento_remuneracion),
      as.integer(mes_devengamiento_remuneracion),
      1L
    )
  ) %>%
  group_by(
    fecha,
    sexo, nacionalidad, tramo_edad,
    sector_institucional, subsector_institucional,
    seccion_ciiu4cl_prin,
    razon_social_unidad_legal
  ) %>%
  summarise(
    pt = n_distinct(
      paste(
        id_ine_id_trabajador, 
        id_ine_id_empresa
      )
    ),
    .groups = "drop"
  ) %>%
  collect()

message(
  "tabla base creada. Generando reportes agregados..."
)

calcular_agregado <-
  function(data, variables_agrupacion) {
    
    vars_group <- c("fecha", variables_agrupacion)
    
    data %>%
      group_by(across(all_of(vars_group))) %>%
      summarise(pt = sum(pt), .groups = "drop")
    
  }

configuracion_reportes <- list(
  "total" = c(),
  "sx"    = c("sexo"),
  "nc"    = c("nacionalidad"),
  "te"    = c("tramo_edad"),
  "si"    = c("sector_institucional"),
  "ssi"   = c("subsector_institucional"),
  "rue"   = c("seccion_ciiu4cl_prin"),
  
  "sx-nc"  = c("sexo", "nacionalidad"),
  "sx-te"  = c("sexo", "tramo_edad"),
  "sx-si"  = c("sexo", "sector_institucional"),
  "sx-ssi" = c("sexo", "subsector_institucional"),
  
  "ssi-rue" = c("subsector_institucional", "seccion_ciiu4cl_prin"),
  
  "razon-social"     = c("razon_social_unidad_legal"),
  "razon-social-rue" = c("razon_social_unidad_legal", "seccion_ciiu4cl_prin"),
  "razon-social-ssi" = c("razon_social_unidad_legal", "subsector_institucional")
)

lista_resultados <-
  map(
    configuracion_reportes,
    ~ calcular_agregado(tbl_base_maestra, .x)
  )

# guardando puestos de trabajo
message("guardando excel...")

wb <- createWorkbook()

iwalk(lista_resultados, function(datos, nombre_hoja) {
  nombre_hoja_safe <- substr(nombre_hoja, 1, 31)
  addWorksheet(wb, nombre_hoja_safe)
  writeData(wb, sheet = nombre_hoja_safe, x = datos)
})

saveWorkbook(
  wb,
  "output/empleo-publico/tbl_suseso_puestos_de_trabajo.xlsx",
  overwrite = TRUE
)

message("¡Proceso completado!")

# guardando en minio
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
    object    = "ooee/trl/6_analisis/6_1_preparacion/empleo-publico/tbl_suseso_puestos_de_trabajo.rds",
    bucket    = "desarrollo",
    region    = "",
    use_https = TRUE,
    base_url  = "api-minio.ine.gob.cl",
    url_style = "path"
  )
})

# calculo personas vinculadas ----

rm(
  tbl_base_maestra,
  lista_resultados
)
gc()

tbl_base_maestra <-
  dt_suseso %>%
  filter(
    ep == 1
  ) %>%
  mutate(
    fecha = make_date(
      as.integer(anno_devengamiento_remuneracion),
      as.integer(mes_devengamiento_remuneracion),
      1L
    ),
    n_dias_trabajados_num = as.numeric(n_dias_trabajados)
  ) %>%
  
  group_by(
    id_ine_id_trabajador, fecha
  ) %>%
  
  mutate(
    rank = dense_rank(desc(monto_remuneracion)) * 1e12 +
      dense_rank(desc(n_dias_trabajados_num)) * 1e6 +
      dense_rank(id_ine_id_empresa)
  ) %>%
  
  filter(
    rank == min(rank)
  ) %>%
  ungroup() %>%
  
  select(
    -id_ine_id_trabajador,
    -id_ine_id_empresa,
    -monto_remuneracion,
    -n_dias_trabajados,
    -n_dias_trabajados_num,
    -rank
  ) %>%
  
  group_by(
    fecha,
    sexo, nacionalidad, tramo_edad,
    sector_institucional, subsector_institucional,
    seccion_ciiu4cl_prin,
    razon_social_unidad_legal
  ) %>%
  summarise(
    pt = n(),
    .groups = "drop"
  ) %>%
  collect()

lista_resultados <-
  map(
    configuracion_reportes,
    ~ calcular_agregado(
      tbl_base_maestra, 
      .x
    )
  )

# guardando puestos de trabajo
message("guardando excel...")

wb <- createWorkbook()

iwalk(lista_resultados, function(datos, nombre_hoja) {
  nombre_hoja_safe <- substr(nombre_hoja, 1, 31)
  addWorksheet(wb, nombre_hoja_safe)
  writeData(wb, sheet = nombre_hoja_safe, x = datos)
})

saveWorkbook(
  wb,
  "output/empleo-publico/tbl_suseso_asalariados_publicos.xlsx",
  overwrite = TRUE
)

message("¡Proceso completado!")

# guardando en minio
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
    object    = "ooee/trl/6_analisis/6_1_preparacion/empleo-publico/tbl_suseso_asalariados_publicos.rds",
    bucket    = "desarrollo",
    region    = "",
    use_https = TRUE,
    base_url  = "api-minio.ine.gob.cl",
    url_style = "path"
  )
})
