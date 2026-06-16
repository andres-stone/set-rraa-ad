
# Objetivo: estimar bridge equation y producir nowcast mensual del IMACEC
# Metodo: OLS con variables SA, evaluación out-of-sample

# seteo previo ----
rm(list = ls())
options(scipen = 999, digits = 4)
gc()

# librerias
library(dplyr)
library(arrow)
library(lubridate)
library(broom)

tbl <- 
  arrow::read_parquet("output/otros/ad-pib/tbl_series_sa.parquet")

# corte estimacion: hasta 12 meses antes del ultimo dato
fecha_corte <- max(tbl$ref_date) %m-% months(12)

tbl_train <- tbl %>% filter(ref_date <= fecha_corte)
tbl_test  <- tbl %>% filter(ref_date > fecha_corte)

# modelo bridge (usar lag_optimo del script 04)
modelo_bridge <- 
  lm(
    var_a_imacec_sa ~ var_a_iilf_sa + var_a_iva_sa,
    data = tbl_train
  )

# resumen modelo
summary(modelo_bridge)
tidy(modelo_bridge)      # coeficientes
glance(modelo_bridge)    # métricas globales

# predicción in-sample
tbl_train <- 
  tbl_train %>%
  mutate(
    imacec_fitted = predict(modelo_bridge, newdata = .)
  )

# predicción out-of-sample (evaluación)
tbl_test <- 
  tbl_test %>%
  mutate(
    imacec_pred = predict(modelo_bridge, newdata = .)
  )

# metricas OOS
mae_oos  <- 
  mean(
    abs(tbl_test$imacec_pred - tbl_test$var_a_imacec_sa), 
    na.rm = TRUE
  )
rmse_oos <- 
  sqrt(
    mean((tbl_test$imacec_pred - tbl_test$var_a_imacec_sa)^2, na.rm = TRUE)
  )

cat("MAE OOS:", round(mae_oos, 3), "\n")
cat("RMSE OOS:", round(rmse_oos, 3), "\n")

# tabla histórica (train + test con fitted y pred)
tbl_historico <- 
  bind_rows(
    tbl_train %>% mutate(imacec_pred = NA_real_),
    tbl_test
  ) %>%
  select(ref_date, var_a_imacec_sa, imacec_fitted, imacec_pred)

# nowcast: ultimos meses donde IILF existe pero IMACEC no está publicado
tbl_nowcast <- 
  tbl %>%
  filter(is.na(var_a_imacec_sa)) %>%
  mutate(imacec_nowcast = predict(modelo_bridge, newdata = .)) %>%
  select(ref_date, imacec_nowcast)

# unir con coalesce para tener una sola tabla limpia
tbl_output <- tbl_historico %>%
  left_join(tbl_nowcast, by = "ref_date")

arrow::write_parquet(
  tbl_output,
  "output/otros/ad-pib/tbl_nowcast_imacec.parquet"
)