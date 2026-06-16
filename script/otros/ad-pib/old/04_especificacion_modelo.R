
# Objetivo: encontrar el mejor rezago de IILF para predecir IMACEC
# Metodo: correlaciones cruzadas, CCF, AIC/BIC

# seteo previo ----
rm(list = ls())
options(scipen = 999, digits = 4)
gc()

# librerias
library(dplyr)
library(arrow)
library(ggplot2)
library(purrr)

tbl <- 
  arrow::read_parquet(
    "output/otros/ad-pib/tbl_series_sa.parquet"
  )

# 1. Funcion de correlación cruzada (CCF)
# Evaluar hasta 6 rezagos del IILF sobre IMACEC
ccf_result <- 
  ccf(
    tbl$var_a_iilf_sa,
    tbl$var_a_imacec_sa,
    lag.max = 6,
    na.action = na.omit,
    plot = TRUE
  )

# 2. Regresiones bridge con rezagos 0 a 3
resultados_aic <-
  purrr::map_dfr(0:3, function(lag_k) {
    df_lag <- 
      tbl %>%
      mutate(
        iilf_lag = lag(var_a_iilf_sa, lag_k),
        iva_lag  = lag(var_a_iva_sa, lag_k)     # si se desestacionaliza IVA
      ) %>%
      filter(!is.na(iilf_lag), !is.na(var_a_imacec_sa))
    
    modelo <- lm(var_a_imacec_sa ~ iilf_lag + iva_lag, data = df_lag)
    
    tibble(
      lag       = lag_k,
      r2        = summary(modelo)$r.squared,
      r2_adj    = summary(modelo)$adj.r.squared,
      aic       = AIC(modelo),
      bic       = BIC(modelo),
      rmse      = sqrt(mean(residuals(modelo)^2))
    )
  })

# 3. Modelo ganador
lag_optimo <- 
  resultados_aic %>%
  slice_min(aic) %>%
  pull(lag)

# guardar tabla de diagnostico
write_xlsx(
  resultados_aic, 
  "output/otros/ad-pib/diagnostico_rezagos.xlsx"
)
