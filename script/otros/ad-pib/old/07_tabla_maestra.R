# Objetivo: consolidar todas las series en una tabla maestra analítica
# Output: tbl_maestra_iilf.parquet + Excel de difusión

library(dplyr)
library(arrow)
library(readxl)
library(writexl)

tbl_sa      <- arrow::read_parquet("output/otros/ad-pib/tbl_series_sa.parquet")
tbl_nowcast <- arrow::read_parquet("output/otros/ad-pib/tbl_nowcast_imacec.parquet")
tbl_pib_m   <- arrow::read_parquet("output/otros/ad-pib/tbl_pib_mensual.parquet")

tbl_maestra <-
  tbl_sa %>%
  select(
    ref_date,
    iilf, iilf_sa, iilf_ajustado,
    payroll_index, payroll_sa,
    wage_index, wage_sa,
    labor_intensity_index,
    var_mensual_iilf, var_anual_iilf,
    var_m_iilf_sa, var_a_iilf_sa,
    imacec, imacec_sa,
    var_m_imacec_sa, var_a_imacec_sa,
    iva
  ) %>%
  left_join(
    tbl_nowcast %>%
      select(-var_a_imacec_sa),   # <-- evita columna duplicada
    by = "ref_date"
  ) %>%
  left_join(
    tbl_pib_m,
    by = "ref_date"
  )

arrow::write_parquet(
  tbl_maestra,
  "output/otros/ad-pib/tbl_maestra_iilf.parquet"
)

# exportar Excel de difusión
writexl::write_xlsx(
  list(
    "IILF_Indices"   = tbl_maestra %>% select(ref_date, starts_with("iilf"), starts_with("payroll"), starts_with("wage")),
    "Nowcast_IMACEC" = tbl_maestra %>% select(ref_date, contains("imacec"), contains("var_a")),
    "PIB_Mensual"    = tbl_maestra %>% select(ref_date, pib_mensual, consumo_mensual)
  ),
  path = "output/otros/ad-pib/difusion_iilf.xlsx"
)