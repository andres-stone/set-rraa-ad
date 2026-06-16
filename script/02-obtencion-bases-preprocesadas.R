
# procesamiento 
# bases mensuales flujos laborales
# 5.1 integracion de datos: suseso + rep + predicho tipo 6
# 3124.936 sec

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
# insumos
ruta_suseso <- "s3://activos/rraa_oae/suseso/cotizaciones_trabajadores/pseudonimizado/"
ruta_rue    <- "s3://desarrollo/rue/compartido/seit" # tipo 6
ruta_personas <- "s3://desarrollo/ooee/trl/5_procesamiento/5_2_clasificacion_codificacion/espejo/pqt_personas_nuevo.parquet"
# ruta_personas <- "s3://desarrollo/ooee/trl/5_procesamiento/5_2_clasificacion_codificacion/espejo/pqt_personas.parquet"
# output
ruta_salida <- "s3://desarrollo/ooee/trl/5_procesamiento/5_1_integracion_datos/espejo"

# obtencion de datasets ----
# Suseso
dt_suseso <- 
  duckdbfs::open_dataset(
    ruta_suseso,
    format = "parquet"
  )

# rue
dt_rue <- 
  duckdbfs::open_dataset(
    ruta_rue,
    format = "parquet"
  )

# suseso 
dt_personas <-
  duckdbfs::open_dataset(
    ruta_personas,
    format = "parquet",
    recursive = F
  )

# preprocesamiento ----
# suseso
dt_suseso_preprocesado <-
  dt_suseso %>%
  # dt_suseso_preprocesado %>% 
  select(
    -c(
      anno, mes,
      nacionalidad, 
      tipo_canal_cotizacion, estado_cotizacion,
      fecha_pago
    )
  ) %>% 
  filter(
    # trabajadores dependientes 1 y 6
    # trabajadores asalariados
    tipo_trabajador == 1,
    # filtro anno de devengamiento
    anno_devengamiento_remuneracion > 2016 | 
      (anno_devengamiento_remuneracion == 2016 & 
         mes_devengamiento_remuneracion >= 11)
  ) %>% 
  mutate(
    # normalizar tipo de pago NA a 1
    tipo_pago = if_else(
      is.na(tipo_pago), 1, tipo_pago
    )
  ) %>% 
  
  # paso 1
  # agrupando segun puesto de trabajo, periodo de devengo 
  # y max tipo declaracion
  group_by(
    id_ine_id_trabajador,
    id_ine_id_empresa,
    anno_devengamiento_remuneracion,
    mes_devengamiento_remuneracion,
    tipo_trabajador
  ) %>% 
  mutate(
    # cual es el tipo maximo de declaracion
    max_tipo_declaracion = max(
      tipo_declaracion, na.rm = T
    )
  ) %>% 
  ungroup() %>% 
  
  # paso 2
  # distintos segun pt
  distinct(
    id_ine_id_trabajador,
    id_ine_id_empresa,
    anno_devengamiento_remuneracion,
    mes_devengamiento_remuneracion,
    tipo_trabajador,
    tipo_pago,
    monto_remuneracion,
    n_dias_trabajados,
    codigo_mutual,
    # estado_cotizacion,
    # fecha_pago,
    .keep_all = TRUE
  ) %>% 
  
  # paso 3
  # preferencia para tipo de pago 1 o NA (que le pusimos 1)
  group_by(
    id_ine_id_trabajador,
    id_ine_id_empresa,
    anno_devengamiento_remuneracion,
    mes_devengamiento_remuneracion,
    tipo_trabajador
  ) %>% 
  ungroup() %>% 
  mutate(
    n_dias_trabajados = as.integer(n_dias_trabajados),
    monto_remuneracion = if_else(
      tipo_pago %in% c(2,3), 0, monto_remuneracion
    ),
    n_dias_trabajados = if_else(
      tipo_pago %in% c(2,3), 0, n_dias_trabajados
    )
  ) %>% 
  # suma de montos segun periodo de puestos de trabajo
  group_by(
    id_ine_id_trabajador,
    id_ine_id_empresa,
    anno_devengamiento_remuneracion,
    mes_devengamiento_remuneracion,
    tipo_trabajador
  ) %>% 
  summarise(
    monto_remuneracion = sum(monto_remuneracion, na.rm = T),
    n_dias_trabajados = sum(n_dias_trabajados, na.rm = T),
    .groups = "drop"
  ) %>% 
  
  # paso 4
  # tope de 30 dias trabajados
  mutate(
    n_dias_trabajados = if_else(
      n_dias_trabajados > 30, 30, n_dias_trabajados
    )
  )

# merge con personas
dt_suseso_preprocesado <- 
  dt_suseso_preprocesado %>% 
  left_join(
    dt_personas,
    by = c("id_ine_id_trabajador" = "id_ine")
  ) %>% 
  mutate(
    
    # Limpiar valores inválidos antes del casteo
    fecha_nac = if_else(fecha_nac %in% c("--", "", " "), NA_character_, fecha_nac),
    fecha_def_cor_rc = if_else(fecha_def_cor_rc %in% c("--", "", " "), NA_character_, fecha_def_cor_rc),
    
    # Casteo de fechas
    fecha_nac = as.Date(fecha_nac),
    fecha_def_cor_rc = as.Date(fecha_def_cor_rc),
    
    # Fecha de referencia (inicio del periodo devengado)
    refdate = make_date(
      anno_devengamiento_remuneracion,
      mes_devengamiento_remuneracion,
      1L
    ),
    
    # Componentes de fecha
    anno_nac = year(fecha_nac),
    mes_nac  = month(fecha_nac),
    
    anno_def = year(fecha_def_cor_rc),
    mes_def  = month(fecha_def_cor_rc),
    
    anno_dev = year(refdate),
    mes_dev  = month(refdate),
    
    # Edad en meses
    edad_meses = as.integer((anno_dev - anno_nac) * 12 + (mes_dev - mes_nac)),
    
    # Meses desde fallecimiento (si aplica)
    meses_fallecimiento = as.integer((anno_dev - anno_def) * 12 + (mes_dev - mes_def)),
    
    # Indicador de fallecimiento antes del periodo
    fallecido = if_else(!is.na(meses_fallecimiento) & meses_fallecimiento > 0, 1L, 0L)
    
  ) %>% 
  # filtros de edad en meses
  filter(
    (edad_meses >= 12 * 15 & edad_meses < 12 * 91) | is.na(edad_meses)
  ) %>% 
  # excluir fallecidos antes del periodo
  filter(
    is.na(fecha_def_cor_rc) | meses_fallecimiento <= 0
  ) %>% 
  # limpiar auxiliares
  select(
    -c(
      anno_nac, mes_nac,
      anno_def, mes_def,
      anno_dev, mes_dev,
      meses_fallecimiento
    )
  )

# quitando prediccion de tipo 6
dt_suseso_preprocesado <- 
  dt_suseso_preprocesado %>% 
  anti_join(
    dt_rue %>% 
      select(
        -pred_tipo_6
      ),
    by = c(
      "id_ine_id_trabajador", "id_ine_id_empresa",
      "anno_devengamiento_remuneracion", "mes_devengamiento_remuneracion"
    )
  )

# guardar parquet mensuales ----
tictoc::tic()

periodos <-
  dt_suseso %>%
  filter(
    # tipo_trabajador == "1",
    (anno_devengamiento_remuneracion > 2016) |
      (anno_devengamiento_remuneracion == 2016 & mes_devengamiento_remuneracion >= 11)
  ) %>%
  distinct(
    anno_devengamiento_remuneracion,
    mes_devengamiento_remuneracion
  ) %>%
  collect() %>%
  arrange(
    anno_devengamiento_remuneracion,
    mes_devengamiento_remuneracion
  )

fallidos <- c()

# procesar y escribir un periodo 
procesar_y_escribir <- 
  function(anio, mes, reintentos = 3) {
    
    mes_fmt       <- formatC(mes, width = 2, flag = "0")
    periodo_label <- glue::glue("{anio}-{mes_fmt}")
    ruta_archivo  <- glue::glue("{ruta_salida}/anno={anio}/mes={mes_fmt}/trl_{anio}{mes_fmt}.parquet")
    
    intento  <- 1
    resultado <- FALSE
    
    while (intento <= reintentos) {
      
      resultado <- tryCatch({
        
        DBI::dbExecute(con, glue("
        COPY (
          SELECT *
          FROM ({dbplyr::remote_query(
            dt_suseso_preprocesado %>%
              filter(
                anno_devengamiento_remuneracion == {anio},
                mes_devengamiento_remuneracion  == {mes}
              )
          )})
        ) TO '{ruta_archivo}'
        (FORMAT PARQUET, OVERWRITE_OR_IGNORE TRUE)
      "))
        
        message(glue::glue("[OK]     {periodo_label}"))
        TRUE
        
      }, error = function(e) {
        
        # Espera de 5s ante error 502 u otros errores de red
        if (grepl("502|HTTP", e$message)) {
          message(glue::glue("[502]    {periodo_label} - intento {intento}/{reintentos} - esperando 5s..."))
          Sys.sleep(5)
        } else {
          message(glue::glue("[ERROR]  {periodo_label} - intento {intento}/{reintentos}: {e$message}"))
          Sys.sleep(5)
        }
        FALSE
      })
      
      if (resultado) break
      intento <- intento + 1
    }
    
    # Registrar fallo definitivo
    if (!resultado) {
      warning(glue("[FALLO]  {periodo_label} - agotados {reintentos} reintentos"))
      fallidos <<- c(fallidos, periodo_label)
    }
    
  }

# ejecucion
purrr::walk2(
  periodos$anno_devengamiento_remuneracion,
  periodos$mes_devengamiento_remuneracion,
  procesar_y_escribir,
  reintentos = 5
)
  
tictoc::toc()
