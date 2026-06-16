
# Objetivo: desestacionalizar IILF, IMACEC e IVA
# Método: X-13ARIMA-SEATS via seasonal::seas()

# seteo previo ----
rm(list = ls())
options(scipen = 999, digits = 4)
gc()

# libreriaslibrary(seasonal)
library(dplyr)
library(arrow)
library(lubridate)
library(tsibble)

# cargar iilf con índices
tbl_iilf <- 
  arrow::read_parquet("output/otros/ad-pib/tbl_iilf_indices.parquet")

# cargar indicadores macro
tbl_macro <- 
  readxl::read_excel("insumos/indicadores-macro.xlsx", sheet = "Hoja1")

# merge
tbl <- 
  tbl_iilf %>%
  left_join(
    tbl_macro, 
    by = "ref_date"
  )

# desestacionalizar función auxiliar
desest_x13 <- 
  function(x, fechas) {
    ts_obj <- 
      ts(
        x,
        start = c(year(min(fechas)), month(min(fechas))),
        frequency = 12
      )
    modelo <- seas(ts_obj)
    as.numeric(final(modelo))
  }

# aplicar a variables clave
tbl <- 
  tbl %>%
  arrange(
    ref_date
  ) %>%
  mutate(
    iilf_sa        = desest_x13(iilf, ref_date),
    imacec_sa      = desest_x13(imacec, ref_date),
    iva_sa         = desest_x13(iva, ref_date),
    payroll_sa     = desest_x13(payroll_index, ref_date),
    wage_sa        = desest_x13(wage_index, ref_date)
  )

# variaciones sobre series desestacionalizadas
tbl <- 
  tbl %>%
  mutate(
    var_m_iilf_sa   = (iilf_sa / lag(iilf_sa, 1) - 1),
    var_a_iilf_sa   = (iilf_sa / lag(iilf_sa, 12) - 1),
    var_m_imacec_sa = (imacec_sa / lag(imacec_sa, 1) - 1),
    var_a_imacec_sa = (imacec_sa / lag(imacec_sa, 12) - 1),
    
    var_m_iva_sa    = (iva_sa / lag(iva_sa, 1) - 1),
    var_a_iva_sa    = (iva_sa / lag(iva_sa, 12) - 1)
  )

arrow::write_parquet(
  tbl, 
  "output/otros/ad-pib/tbl_series_sa.parquet"
)