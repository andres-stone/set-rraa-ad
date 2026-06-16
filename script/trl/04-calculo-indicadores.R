
# generando indicadores

# seteo previo ----
rm(list = ls())
options(scipen = 999, digits = 4)
gc()

# librerias
library(dplyr)
library(purrr)
library(lubridate)
library(glue)
library(openxlsx)
library(stringr)
library(tidyr)
library(data.table)
library(readxl)
library(writexl)

# insumos y funciones ----

# nombre de hojas 
n_puestos_de_trabajo <-
  c(
    "flujos_total", 
    "flujos_sx",
    "flujos_nc",
    "flujos_re",
    "flujos_te",
    "flujos_sector",
    "flujos_tamano",
    "flujos_sx_sector",
    "flujos_sx_tamano",
    "flujos_tamano_sector",
    "flujos_sx_re",
    "flujos_sx_te",
    "flujos_sx_tamano_sector"
  )

n_dominio_fijo <-
  c(
    "flujos_total", 
    "flujos_sx",
    "flujos_nc",
    "flujos_re",
    "flujos_sector",
    "flujos_sx_sector",
    "flujos_sx_re"
  )

n_dominio_dinamicos <-
  c(
    "flujos_tamano",
    "flujos_sx_tamano",
    "flujos_tamano_sector",
    "flujos_sx_tamano_sector",
    "flujos_te",
    "flujos_sx_te"
  )

# funcion para leer excel completo
leer_excel <-
  function(hojas, archivo){
    hojas %>% 
      set_names() %>% 
      map(
        ~ as.data.table(
          read_excel(
            archivo,
            sheet = which(hojas == .x)
          )
        )
      )
  }

# lectura de tablas
dt_pt <-
  leer_excel(
    n_puestos_de_trabajo,
    "output/trl/tbl_suseso_pt.xlsx"
  )

dt_flujos_fijos <-
  leer_excel(
    n_dominio_fijo,
    "output/trl/tbl_trl_flujos_laborales.xlsx"
  )

dt_flujos_dinamicos <-
  leer_excel(
    n_dominio_dinamicos,
    "output/trl/tbl_trl_flujos_dinamicos_tamano.xlsx"
  ) %>% 
  map(~ .x[tipo_flujo %chin% c("entrada", "salida")]) %>% 
  map(function(dt) {
    vars_clave <- setdiff(names(dt), c("tipo_flujo", "n"))
    dt_cast <- dcast(
      dt,
      formula = as.formula(
        paste(paste(vars_clave, collapse = " + "), "~ tipo_flujo")
      ),
      value.var = "n"
    )
    setnames(dt_cast, old = "entrada", new = "pt_ent_12")
    setnames(dt_cast, old = "salida",  new = "pt_sal_12")
    dt_cast[]
  })

# reglas segun nombre de tabla
get_join_keys <- 
  function(nombre) {
    base <- c("fecha")
    if (grepl("sx",     nombre)) base <- c(base, "sexo")
    if (grepl("nc",     nombre)) base <- c(base, "nacionalidad_final")
    if (grepl("re",     nombre)) base <- c(base, "region_final")
    if (grepl("sector", nombre)) base <- c(base, "seccion_ciiu4cl")
    if (grepl("tamano", nombre)) base <- c(base, "tamano_empresa_movil")
    if (grepl("te",     nombre)) base <- c(base, "tramo_edad")
    return(base)
  }

dt_joined <-
  map(names(dt_pt), function(nombre) {
    message("Procesando: ", nombre)
    tabla_flujo <- 
      if (grepl("tamano|te", nombre)) dt_flujos_dinamicos[[nombre]]
    else                            dt_flujos_fijos[[nombre]]
    keys <- get_join_keys(nombre)
    out <- dt_pt[[nombre]] %>% 
      full_join(tabla_flujo, by = keys)
    return(out)
  }) %>% 
  set_names(names(dt_pt))

# conversion de fecha 
dt_joined <- 
  dt_joined %>%
  map(~ .x %>% mutate(fecha = as.Date(fecha)))

# calculo de indicadores ----

calcular_tasas <- 
  function(df) {
    vars_grupo <- setdiff(
      names(df),
      c("fecha", "pt", "pt_ent_12", "pt_sal_12")
    )
    
    df <- df %>% arrange(fecha)
    
    if (length(vars_grupo) > 0) {
      df <- df %>% group_by(across(all_of(vars_grupo)))
    }
    
    df %>%
      mutate(
        # tasas
        t_ent       = pt_ent_12 / lag(pt, 12),
        t_sal       = pt_sal_12 / lag(pt, 12),
        t_rot       = (t_ent + t_sal) / 2,
        t_rot_neta  = t_ent - t_sal,
        t_per       = 1 - t_sal,
        
        # var 12 meses
        var12_pt         = (pt / lag(pt, 12)) - 1,
        var12_pt_ent     = (pt_ent_12 / lag(pt_ent_12, 12)) - 1,
        var12_pt_sal     = (pt_sal_12 / lag(pt_sal_12, 12)) - 1,
        var12_t_ent      = t_ent      - lag(t_ent, 12),
        var12_t_rot_neta = t_rot_neta - lag(t_rot_neta, 12),
        var12_t_rot      = t_rot      - lag(t_rot, 12),
        var12_t_sal      = t_sal      - lag(t_sal, 12),
        var12_t_per      = t_per      - lag(t_per, 12),
        
        # var 1 mes
        var1_pt         = (pt / lag(pt, 1)) - 1,
        var1_pt_ent     = (pt_ent_12 / lag(pt_ent_12, 1)) - 1,
        var1_pt_sal     = (pt_sal_12 / lag(pt_sal_12, 1)) - 1,
        var1_t_ent      = t_ent      - lag(t_ent, 1),
        var1_t_rot_neta = t_rot_neta - lag(t_rot_neta, 1),
        var1_t_rot      = t_rot      - lag(t_rot, 1),
        var1_t_sal      = t_sal      - lag(t_sal, 1),
        var1_t_per      = t_per      - lag(t_per, 1)
      ) %>%
      ungroup()
  }

dt_tasas <-
  map(
    dt_joined,
    calcular_tasas
  )

# calculo de incidencias ----

# tabla de referencia: pt total desplazado 12 meses hacia adelante
dt_pt$flujos_total_shifted <- 
  dt_pt$flujos_total %>% 
  mutate(fecha = as.Date(fecha) %m+% months(12)) %>% 
  rename(pt_total_12 = pt)

calcular_incidencia <- 
  function(df) {
    df <- 
      df %>%
      left_join(
        dt_pt$flujos_total_shifted,
        by = "fecha"
      )
    
    vars_grupo <- setdiff(
      names(df),
      c("fecha", "pt", "pt_ent_12", "pt_sal_12", "pt_total_12")
    )
    
    df %>% 
      group_by(across(all_of(vars_grupo))) %>%
      mutate(
        inc_ent      = pt_ent_12 / pt_total_12,
        inc_sal      = pt_sal_12 / pt_total_12,
        inc_rot      = (inc_ent + inc_sal) / 2,
        inc_rot_neta = inc_ent - inc_sal,
        inc_per      = 1 - inc_sal
      ) %>%
      ungroup()
  }

dt_tasas <-
  map(
    dt_tasas,
    calcular_incidencia
  )

# formato de tablas finales ----

transformar_a_largo <- function(df) {
  
  fijas      <- c("fecha", "anno", "mes")
  indicadores <- grep("^pt|^t_|^var|^inc_", names(df), value = TRUE)
  
  df %>%
    pivot_longer(
      cols      = all_of(indicadores),
      names_to  = "indicador",
      values_to = "valor"
    )
}

transformar_general <- function(df) {
  
  df %>%
    mutate(
      fecha = ymd(fecha),
      anno  = year(fecha),
      mes   = month(fecha)
    ) %>%
    transformar_a_largo() %>%
    select(anno, mes, everything()) 
}

dt_largo <- 
  map(
    dt_tasas, 
    transformar_general
  )

# incidencia en la variacion en 12 meses de la trl ----

calcular_inc_var_trl <- function(df) {
  
  cols_fijas  <- c("anno", "mes", "fecha", "indicador", "valor")
  vars_grupo  <- setdiff(names(df), cols_fijas)
  
  df %>%
    filter(indicador == "inc_rot") %>%
    arrange(across(all_of(vars_grupo)), fecha) %>%
    group_by(across(all_of(vars_grupo))) %>%
    mutate(
      inc_rot     = valor,
      inc_rot_12  = dplyr::lag(inc_rot, 12),
      inc_var_trl = inc_rot - inc_rot_12
    ) %>%
    ungroup() %>%
    select(fecha, all_of(vars_grupo), inc_rot, inc_rot_12, inc_var_trl)
}

dt_var_trl <- 
  imap(
    dt_largo, 
    ~ calcular_inc_var_trl(.x)
  )

convertir_resultados_largo <- function(df) {
  df %>%
    pivot_longer(
      cols      = c(inc_rot, inc_rot_12, inc_var_trl),
      names_to  = "indicador",
      values_to = "valor"
    )
}

dt_largo_integrado <- 
  imap(
    dt_largo,
    function(tabla_original, nombre_tabla) {
      res <- dt_var_trl[[nombre_tabla]]
      if (nrow(res) == 0) return(tabla_original)
      res_largo <- convertir_resultados_largo(res)
      bind_rows(tabla_original, res_largo)
    }
  )

dt_largo_integrado <- 
  map(
    dt_largo_integrado,
    ~ .x %>%
      mutate(
        fecha = ymd(fecha),
        anno  = year(fecha),
        mes   = month(fecha)
      )
  )

# guarda excel ----
walk2(
  dt_largo_integrado,
  names(dt_largo_integrado),
  ~ {
    if (nrow(.x) <= 1e6) {
      write_xlsx(
        list(.y = .x),
        path = paste0("output/trl/flujos/", .y, ".xlsx")
      )
      message("Guardado: ", .y)
    } else {
      message("OMITIDO (>1M filas): ", .y)
    }
  }
)

# guardar minio ----
local({
  Sys.setenv(
    AWS_ACCESS_KEY_ID     = Sys.getenv("ACCESS"),
    AWS_SECRET_ACCESS_KEY = Sys.getenv("SECRET")
  )
  
  tmp <- tempfile(fileext = ".rds")
  saveRDS(dt_largo_integrado, file = tmp)
  on.exit(unlink(tmp))
  
  aws.s3::put_object(
    file      = tmp,
    object    = "ooee/trl/6_analisis/6_1_preparacion/es/202512/tbl_trl_indicadores.rds",
    bucket    = "desarrollo",
    region    = "",
    use_https = TRUE,
    base_url  = "api-minio.ine.gob.cl",
    url_style = "path"
  )
})


