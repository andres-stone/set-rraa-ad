
# Objetivo: desagregar PIB trimestral a frecuencia mensual usando IILF como
#           indicador relacionado (método Chow-Lin)
# Paquete: tempdisagg

# seteo previo ----
rm(list = ls())
options(scipen = 999, digits = 4)
gc()

# librerias
library(dplyr)
library(arrow)
library(readxl)
library(tempdisagg)
library(lubridate)

# cargar datos
tbl_iilf_sa <- 
  arrow::read_parquet("output/otros/ad-pib/tbl_series_sa.parquet")

tbl_pib <- 
  read_excel(
    "insumos/indicadores-macro.xlsx",
    sheet = "Hoja2"
  )

# preparar series trimestrales
# ref_date en Hoja2 corresponde al último mes del trimestre (dic, mar, jun, sep)
pib_ts <- 
  ts(
    tbl_pib$pib_precios_ano_anterior_desestacionalizado,
    start = c(2016, 4),   # Q4 2016
    frequency = 4
  )

consumo_ts <- 
  ts(
    tbl_pib$consumo,
    start = c(2016, 4),
    frequency = 4
  )

# indicador mensual: IILF desestacionalizado
iilf_ts <- 
  ts(
    tbl_iilf_sa$iilf_sa,
    start = c(year(min(tbl_iilf_sa$ref_date)),
              month(min(tbl_iilf_sa$ref_date))),
    frequency = 12
  )

# Chow-Lin: desagregación trimestral → mensual con indicador IILF
modelo_cl_pib <- 
  td(
    pib_ts ~ iilf_ts,
    method = "chow-lin-maxlog",
    conversion = "average"
  )

# extraer serie mensual estimada
pib_mensual <- predict(modelo_cl_pib)

# Chow-Lin para consumo
modelo_cl_consumo <- 
  td(
    consumo_ts ~ iilf_ts,
    method = "chow-lin-maxlog",
    conversion = "average"
  )

consumo_mensual <- predict(modelo_cl_consumo)

# construir tabla resultado
fechas_mensuales <- 
  seq(
    as.Date("2016-11-01"),
    by = "month",
    length.out = length(pib_mensual)
  )

tbl_pib_mensual <- 
  tibble(
    ref_date         = fechas_mensuales,
    pib_mensual      = as.numeric(pib_mensual),
    consumo_mensual  = as.numeric(consumo_mensual)
  )

arrow::write_parquet(
  tbl_pib_mensual,
  "output/otros/ad-pib/tbl_pib_mensual.parquet"
)
