
# seteo previo ----
rm(list = ls())
options(scipen = 999, digits = 4)
gc()

# librerias
library(dplyr)
library(data.table)
library(purrr)
library(lubridate)
library(tidyr)
library(readxl)
library(openxlsx)

# carpeta salida
dir.create("output/empleo-publico/incidencia-condicional", showWarnings = FALSE)

# obtencion tabla
leer_excel <- function(archivo, hoja){
  
  as.data.table(
    read_excel(
      archivo,
      sheet = hoja
    )
  )
}

# tabla de puestos de trabajo
dt <- 
  leer_excel(
    archivo = "output/empleo-publico/tbl_suseso_puestos_de_trabajo.xlsx",
    hoja    = "ssi-rue"
  )

# calculo tasas
calcular_tasas <- 
  function(df) {
    df %>%
      arrange(fecha) %>%
      group_by(subsector_institucional, seccion_ciiu4cl_prin) %>%
      mutate(
        var12_pt = (pt / lag(pt, 12)) - 1,
        var1_pt  = (pt / lag(pt, 1)) - 1
      ) %>%
      ungroup()
  }

dt <- calcular_tasas(dt)

# calculo incidencia
calcular_incidencia_condicional <- 
  function(df) {
    df %>%
      
      # total por seccion (grupo padre)
      group_by(fecha, seccion_ciiu4cl_prin) %>%
      mutate(
        pt_seccion = sum(pt, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      
      # incidencia dentro de seccion
      group_by(subsector_institucional, seccion_ciiu4cl_prin) %>%
      arrange(fecha) %>%
      mutate(
        
        # pesos lag 12
        pt_lag12        = lag(pt, 12),
        pt_seccion_lag12 = lag(pt_seccion, 12),
        wi_12           = pt_lag12 / pt_seccion_lag12,
        inc_var_12      = var12_pt * wi_12,
        
        # pesos lag 1
        pt_lag1        = lag(pt, 1),
        pt_seccion_lag1 = lag(pt_seccion, 1),
        wi_01          = pt_lag1 / pt_seccion_lag1,
        inc_var_01     = var1_pt * wi_01
        
      ) %>%
      ungroup()
  }

dt <- calcular_incidencia_condicional(dt)

# formato largo
transformar_a_largo <- 
  function(df) {
    indicadores <-
      grep("^pt|^var|^inc_", names(df), value = TRUE)
    
    df %>%
      pivot_longer(
        cols = all_of(indicadores),
        names_to = "indicador",
        values_to = "valor"
      )
  }

dt_final <-
  dt %>%
  mutate(
    fecha = ymd(fecha),
    anno  = year(fecha),
    mes   = month(fecha)
  ) %>%
  transformar_a_largo() %>%
  select(
    anno, mes, 
    subsector_institucional, seccion_ciiu4cl_prin,
    indicador, valor
  )

# exportar xlsx
write_xlsx(
  list(
    "incidencia_condicional" = dt_final
  ),
  path = "output/empleo-publico/incidencia-condicional/tbl_ssi_rue_incidencia.xlsx"
)

# guardo n minio
local({
  Sys.setenv(
    AWS_ACCESS_KEY_ID     = Sys.getenv("ACCESS"),
    AWS_SECRET_ACCESS_KEY = Sys.getenv("SECRET")
  )
  
  tmp <- tempfile(fileext = ".rds")
  saveRDS(dt_final, file = tmp)
  on.exit(unlink(tmp))
  
  aws.s3::put_object(
    file      = tmp,
    object    = "ooee/trl/6_analisis/6_1_preparacion/empleo-publico/tbl_ssi_rue_incidencia.rds",
    bucket    = "desarrollo",
    region    = "",
    use_https = TRUE,
    base_url  = "api-minio.ine.gob.cl",
    url_style = "path"
  )
})
