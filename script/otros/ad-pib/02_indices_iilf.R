
# indices iilf

# seteo previo ----
rm(list = ls())
options(scipen = 999, digits = 4)
gc()

# librerias 
library(dplyr)
library(arrow)
library(lubridate)
library(readxl)

# carga datos ----

# parquet nacional
pqt_iilf_n <-
  open_dataset(
    "output/otros/ad-pib/tbl_iilf_nacional.parquet"
  )

# ipc
tbl_ipc <-
  read_excel(
    "insumos/ipc.xlsx"
  )

# construccion de tablas ----
tbl_iilf_n <-
  pqt_iilf_n %>%
  rename(
    ref_date = refdate
  ) %>%
  collect()

# merge ipc
tbl_iilf_n <-
  tbl_iilf_n %>%
  left_join(
    tbl_ipc,
    by = "ref_date"
  )

# construccion variables reales
tbl_iilf_n <-
  tbl_iilf_n %>%
  mutate(
    masa_salarial_real = masa_salarial / ipc, # masa salarial real
    masa_salarial_ajustada_real = masa_salarial_ajustada / ipc, # masa ajustada real
    salario_promedio_real = salario_promedio / ipc, # salario promedio real
    salario_mediano_real = salario_mediano / ipc, # salario mediano real
    salario_diario_real = salario_diario_promedio / ipc * 100 # salario diario real
  )

# IILF ----
ano_base <- 2018

# parametros base
base <-
  tbl_iilf_n %>%
  filter(
    year(ref_date) == ano_base
  ) %>%
  summarise(
    base_masa = mean(masa_salarial_real, na.rm = TRUE), # wage bill
    base_masa_ajustada = mean(masa_salarial_ajustada_real, na.rm = TRUE), # wage bill ajustado
    base_salario = mean(salario_promedio_real, na.rm = TRUE), # salarios
    base_salario_diario = mean(salario_diario_real, na.rm = TRUE), # salario diario
    base_empleo = mean(empleo_formal, na.rm = TRUE), # empleo
    base_intensidad = mean(dias_totales,na.rm = TRUE) # intensidad
  )

# parametros 
base_masa <- base$base_masa
base_masa_ajustada <- base$base_masa_ajustada
base_salario <- base$base_salario
base_salario_diario <- base$base_salario_diario
base_empleo <-base$base_empleo
base_intensidad <- base$base_intensidad

# construccion indices ----
tbl_iilf_n <-
  tbl_iilf_n %>%
  mutate(
    iilf = masa_salarial_real / base_masa * 100, # indice principal 
    iilf_ajustado = masa_salarial_ajustada_real / base_masa_ajustada * 100, # indice ajustado
    payroll_index = empleo_formal / base_empleo * 100, # payroll employment
    wage_index = salario_promedio_real / base_salario * 100, # wage index
    wage_daily_index = salario_diario_real / base_salario_diario * 100, # wage daily index
    labor_intensity_index = dias_totales / base_intensidad * 100 # labor intensity
  )

# variacione
tbl_iilf_n <-
  tbl_iilf_n %>%
  arrange(ref_date) %>%
  mutate(
    
    # variaciones mensuales
    var_mensual_iilf = (iilf / lag(iilf, 1) - 1),
    var_mensual_payroll = (payroll_index / lag(payroll_index, 1) - 1),
    
    # variaciones anuales
    var_anual_iilf = (iilf / lag(iilf, 12) - 1),
    var_anual_payroll = (payroll_index / lag(payroll_index, 12) - 1)
  )

# comparando el IILF ajustado con respecto al principal (iilf_ajustado - iilf):
# si todos los puestos de trabajo hubieran trabajado el mes completo, 
# el ingreso laboral equivalente habría sido aprox 10-13% mayor, 
# es esperable que sea mayor, pensando en licencias, jornadas parciales,
# entradas-salidas mensuales, empleo parcial, contratos continuos y pandemia.
# evidencia estallido social (mayor brecha)

# hay una recuperación más rapida que intensidad laboral, recuperacion de pandemia
# informalidad parcial, empleo fragmentado y jornadas más cortas.

# iilf: ingreso efectivamente pagado
# iilf_ajustado: capacidad potencial equivalente a full-time

# guardar ----
arrow::write_parquet(
  tbl_iilf_n,
  "output/otros/ad-pib/tbl_iilf_indices.parquet"
)

# visualizaciones ----

# directorio outputs graficos
dir.create(
  "output/otros/ad-pib/graficos",
  recursive = TRUE,
  showWarnings = FALSE
)

# serie principal IILF
g_iilf <-
  ggplot(
    tbl_iilf_n,
    aes(x = ref_date)
  ) +
  geom_line(
    aes(y = iilf),
    linewidth = 1
  ) +
  labs(
    title = "Indice de Ingresos Laborales Formales",
    subtitle = "Base promedio 2019 = 100",
    x = NULL,
    y = "Indice"
  ) +
  theme_minimal()

ggsave(
  filename = "output/otros/ad-pib/graficos/iilf.png",
  plot = g_iilf,
  width = 10,
  height = 6
)

# comparacion IILF vs ajustado
tbl_comp <-
  tbl_iilf_n %>%
  select(
    ref_date,
    iilf,
    iilf_ajustado
  ) %>%
  pivot_longer(
    cols = c(iilf, iilf_ajustado),
    names_to = "serie",
    values_to = "indice"
  )

g_comp <-
  ggplot(
    tbl_comp,
    aes(
      x = ref_date,
      y = indice,
      color = serie
    )
  ) +
  geom_line(
    linewidth = 1
  ) +
  labs(
    title = "Comparacion IILF principal y ajustado",
    subtitle = "Base promedio 2019 = 100",
    x = NULL,
    y = "Indice",
    color = NULL
  ) +
  theme_minimal()

ggsave(
  filename = "output/otros/ad-pib/graficos/iilf_comparado.png",
  plot = g_comp,
  width = 10,
  height = 6
)

# componentes del indice
tbl_componentes <-
  tbl_iilf_n %>%
  select(
    ref_date,
    payroll_index,
    wage_index,
    labor_intensity_index
  ) %>%
  pivot_longer(
    cols = -ref_date,
    names_to = "componente",
    values_to = "indice"
  )

g_componentes <-
  ggplot(
    tbl_componentes,
    aes(
      x = ref_date,
      y = indice,
      color = componente
    )
  ) +
  geom_line(
    linewidth = 1
  ) +
  labs(
    title = "Componentes del IILF",
    subtitle = "Base promedio 2019 = 100",
    x = NULL,
    y = "Indice",
    color = NULL
  ) +
  theme_minimal()

ggsave(
  filename = "output/otros/ad-pib/graficos/componentes_iilf.png",
  plot = g_componentes,
  width = 10,
  height = 6
)

# brecha entre indice principal y ajustado
tbl_iilf_n <-
  tbl_iilf_n %>%
  mutate(
    brecha_ajuste = iilf_ajustado - iilf
  )

g_brecha <-
  ggplot(
    tbl_iilf_n,
    aes(
      x = ref_date,
      y = brecha_ajuste
    )
  ) +
  geom_line(
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Brecha entre IILF principal y ajustado",
    subtitle = "Diferencia en puntos indice",
    x = NULL,
    y = "Brecha"
  ) +
  theme_minimal()

ggsave(
  filename = "output/otros/ad-pib/graficos/brecha_iilf.png",
  plot = g_brecha,
  width = 10,
  height = 6
)
