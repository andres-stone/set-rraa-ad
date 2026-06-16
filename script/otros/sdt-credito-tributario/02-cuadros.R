# evaluacion credito fiscal historico ----

# seteo previo ----
rm(list = ls())
options(scipen = 999, digits = 4)
gc()

# librerias
library(dplyr)
library(arrow)
library(lubridate)
library(openxlsx)

# datos 
pqt_pt <-
  open_dataset(
    "output/credito-tributario/tbl_pt_credito_tributario.parquet"
  ) %>% 
  mutate(
    ano = year(fecha),
    mes = month(fecha)
  )

pqt_cl <-
  open_dataset(
    "output/credito-tributario/tbl_cl_credito_tributario.parquet"
  ) %>% 
  mutate(
    ano = year(fecha),
    mes = month(fecha)
  )

# funciones ----

# participacion simple
fn_participacion <- 
  function(data, grupos, variable, nombre_valor) {
    data %>% 
      group_by(across(all_of(grupos))) %>% 
      summarise(
        valor = {{ variable }},
        .groups = "drop"
      ) %>% 
      group_by(
        ano, mes
      ) %>% 
      mutate(
        participacion = valor / sum(valor, na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      rename(
        !!nombre_valor := valor
      ) %>% 
      arrange(across(all_of(grupos))) %>% 
      collect()
  }

# participacion doble (clp/usd)
fn_participacion_monto <- 
  function(data, grupos, variable_clp, variable_usd) {
    data %>% 
      group_by(across(all_of(grupos))) %>% 
      summarise(
        monto_clp = {{ variable_clp }},
        monto_usd = {{ variable_usd }},
        .groups = "drop"
      ) %>% 
      group_by(
        ano, mes
      ) %>% 
      mutate(
        participacion_clp = monto_clp / sum(monto_clp, na.rm = TRUE),
        participacion_usd = monto_usd / sum(monto_usd, na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      arrange(across(all_of(grupos))) %>% 
      collect()
  }

# obtencion de tablas ----

# puestos de trabajo
tbl_pt_sx_nc <-
  fn_participacion(
    data = pqt_pt,
    grupos = c("ano", "mes", "sexo", "nacionalidad"),
    variable = sum(puestos_de_trabajo, na.rm = TRUE),
    nombre_valor = "pt"
  )

tbl_pt_tamano <-
  fn_participacion(
    data = pqt_pt,
    grupos = c("ano", "mes", "tamano_empresa_movil"),
    variable = sum(puestos_de_trabajo, na.rm = TRUE),
    nombre_valor = "pt"
  )

tbl_pt_sector <-
  fn_participacion(
    data = pqt_pt,
    grupos = c("ano", "mes", "seccion_ciiu4cl"),
    variable = sum(puestos_de_trabajo, na.rm = TRUE),
    nombre_valor = "pt"
  )

# empresas
tbl_ee_tamano <-
  fn_participacion(
    data = pqt_cl,
    grupos = c("ano", "mes", "tamano_empresa_movil"),
    variable = n_distinct(id_ine_id_empresa),
    nombre_valor = "n_e"
  )

tbl_ee_sector <-
  fn_participacion(
    data = pqt_cl,
    grupos = c("ano", "mes", "seccion_ciiu4cl"),
    variable = n_distinct(id_ine_id_empresa),
    nombre_valor = "n_e"
  )

# masa salarial
tbl_cl_tamano <-
  fn_participacion_monto(
    data = pqt_cl,
    grupos = c("ano", "mes", "tamano_empresa_movil"),
    variable_clp = sum(cl_clp, na.rm = TRUE),
    variable_usd = sum(cl_usd, na.rm = TRUE)
  )

# credito tributario
tbl_ct_tamano <-
  fn_participacion_monto(
    data = pqt_cl,
    grupos = c("ano", "mes", "tamano_empresa_movil"),
    variable_clp = sum(ct_clp, na.rm = TRUE),
    variable_usd = sum(ct_usd, na.rm = TRUE)
  )

# lista tablas ----
lista_tablas <- 
  list(
    pt_sx_nc    = tbl_pt_sx_nc,
    pt_tamano   = tbl_pt_tamano,
    pt_sector   = tbl_pt_sector,
    ee_tamano   = tbl_ee_tamano,
    ee_sector   = tbl_ee_sector,
    cl_tamano   = tbl_cl_tamano,
    ct_tamano   = tbl_ct_tamano
  )

# guardar parquet ----
dir.create(
  "output/credito-tributario/tablas",
  recursive = TRUE,
  showWarnings = FALSE
)

invisible(
  lapply(
    names(lista_tablas),
    function(x) {
      
      write_parquet(
        lista_tablas[[x]],
        glue::glue(
          "output/credito-tributario/tablas/{x}.parquet"
        )
      )
      
    }
  )
)

# guardar excel ----
wb <- createWorkbook()

lapply(
  names(lista_tablas),
  function(x) {
    
    addWorksheet(wb, x)
    
    writeData(
      wb,
      sheet = x,
      x = lista_tablas[[x]]
    )
    
  }
)

saveWorkbook(
  wb,
  "output/credito-tributario/tablas/credito_tributario_historico.xlsx",
  overwrite = TRUE
)
