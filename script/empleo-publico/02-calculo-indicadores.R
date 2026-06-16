
# sete previo ----
rm(list = ls())
options(scipen = 999, digits = 4)
gc()

# librerias 
library(dplyr)
library(duckdb)
library(duckdbfs)
library(purrr)
library(lubridate)
library(glue)
library(tictoc)
library(stringr)
library(openxlsx)
library(data.table)
library(readxl)
library(writexl)

# carpeta 
dir.create("output/empleo-publico/personas")
dir.create("output/empleo-publico/puestos-de-trabajo")

# insumos y funciones ----

# nombre de hojas
n_puestos_de_trabajo <-
  c(
    "tbl_total",
    "tbl_sx",
    "tbl_nc",
    #"tbl_re",
    "tbl_te",
    "tbl_si",
    "tbl_ssi",
    "tbl_rue",
    "tbl_sx_nc",
    "tbl_sx_te",
    "tbl_sx_si",
    "tbl_sx_ssi",
    "tbl_ssi_rue",
    "tbl_rs",
    "tbl_rs-rue",
    "tbl_rs-ssi"
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
    "output/empleo-publico/tbl_suseso_puestos_de_trabajo.xlsx"
  )

# lectura de tablas
dt_per <-
  leer_excel(
    n_puestos_de_trabajo,
    "output/empleo-publico/tbl_suseso_asalariados_publicos.xlsx"
  )

# calculo de indicadores e incidencia ----

# calculo de tasas y variaciones
calcular_tasas <-
  function(df) {
    
    # detectar la columna de desagregacion
    vars_grupo <-
      setdiff(
        names(df),
        c(
          "fecha","pt"
        )
      )
    
    # si no hay desagregacion: grupo Unico
    if (length(vars_grupo) == 0) {
      df <- df %>%
        arrange(fecha) %>%
        mutate(
          
          # var 12 meses
          var12_pt = (pt / lag(pt, 12)) - 1,
          
          # var 1 mes
          var1_pt = (pt / lag(pt, 1)) - 1
        )
      return(df)
    }
    
    # si hay desagregacion: agrupar
    df %>%
      arrange(fecha) %>%
      group_by(across(all_of(vars_grupo))) %>%
      mutate(
        
        # var 12 meses
        var12_pt = (pt / lag(pt, 12)) - 1,
        
        # var 1 mes
        var1_pt = (pt / lag(pt, 1)) - 1
      ) %>%
      ungroup()
    
  }

dt_pt <-
  map(
    dt_pt,
    calcular_tasas
  )

dt_per <-
  map(
    dt_per,
    calcular_tasas
  )

calcular_incidencias <-
  function(df_desagregado, df_total, var_group) {
    
    df_desagregado %>%
      left_join(
        df_total,
        by = "fecha",
        suffix = c("_des", "_tot")
      ) %>%
      group_by(
        across(all_of(var_group))
      ) %>%
      mutate(
        
        # incidencia var12 (lag 12)
        pt_des_12 = lag(pt_des, 12),
        pt_tot_12 = lag(pt_tot, 12),
        wi_12     = pt_des_12 / pt_tot_12,
        # inc_var_12 = var12_pt_tot * wi_12,
        inc_var_12 = var12_pt_des * wi_12,
        
        # incidencia var01 (lag 1)
        pt_des_01 = lag(pt_des, 1),
        pt_tot_01 = lag(pt_tot, 1),
        wi_01     = pt_des_01 / pt_tot_01,
        # inc_var_01 = var1_pt_tot * wi_01
        inc_var_01 = var1_pt_des * wi_01
        
      ) %>%
      ungroup() %>%
      select(
        -c(
          pt_des_12, pt_tot_12, wi_12,
          pt_des_01, pt_tot_01, wi_01,
          pt_tot, var12_pt_tot, var1_pt_tot
        )
      )
    
  }

# variables de agrupacion
vars_group <-
  list(
    # persona
    tbl_sx = "sexo",
    tbl_nc = "nacionalidad",
    #tbl_re = "region",
    tbl_te = "tramo_edad",
    
    # institucion
    tbl_si  = "sector_institucional",
    tbl_ssi = "subsector_institucional",
    
    # act economica
    tbl_rue   = "seccion_ciiu4cl_prin",
    # tbl_det   = "seccion_sugerida_det",
    # tbl_mme   = "seccion_ciiu4cl",
    
    # combinatorias
    tbl_sx_nc   = c("sexo", "nacionalidad"),
    tbl_sx_te   = c("sexo", "tramo_edad"),
    tbl_sx_si   = c("sexo", "sector_institucional"),
    tbl_sx_ssi  = c("sexo", "subsector_institucional"),
    
    # institucion - sector
    tbl_ssi_rue = c("subsector_institucional", "seccion_ciiu4cl_prin")#,
    # tbl_ssi_det = c("subsector_institucional", "seccion_sugerida_det"),
    # tbl_ssi_mme = c("subsector_institucional", "seccion_ciiu4cl")
    
  )

# generando variaciones e incidencias
dt_per <-
  dt_per[names(dt_per) != "tbl_total"] %>%
  imap(
    
    function(tabla, nombre_tabla) {
      
      calcular_incidencias(
        df_desagregado = tabla,
        df_total       = dt_per$tbl_total,
        var_group      = vars_group[[nombre_tabla]]
      )
      
    }
    
  )

dt_pt <-
  dt_pt[names(dt_pt) != "tbl_total"] %>%
  imap(
    
    function(tabla, nombre_tabla) {
      
      calcular_incidencias(
        df_desagregado = tabla,
        df_total       = dt_pt$tbl_total,
        var_group      = vars_group[[nombre_tabla]]
      )
      
    }
    
  )

# formato de tablas finales ----
transformar_a_largo <-
  function(df) {
    
    # columnas que no son indicadores
    fijas <- c("fecha", "anno", "mes")
    desag_cols <-
      setdiff(
        
        names(df),
        c(
          fijas,
          grep(
            "^pt|^t_|^var|^inc_",
            names(df),
            value = TRUE
          )
        )
        
      )
    
    indicadores <-
      grep(
        "^pt|^t_|^var|^inc_",
        names(df),
        value = TRUE
      )
    
    df %>%
      pivot_longer(
        cols = all_of(indicadores),
        names_to = "indicador",
        values_to = "valor"
      ) %>%
      mutate(
        indicador = if_else(
          indicador == "pt_des", "pt", if_else(
            indicador == "var1_pt_des", "var1_pt", if_else(
              indicador == "var12_pt_des", "var12_pt", if_else(
                indicador == "inc_var_12", "inc12_pt", if_else(
                  indicador == "inc_var_01", "inc1_pt",
                  indicador
                )
              )
            )
          )
        )
      )
    
  }

transformar_general <-
  function(df) {
    
    df %>%
      mutate(
        fecha = ymd(fecha),
        anno  = year(fecha),
        mes   = month(fecha)
      ) %>%
      transformar_a_largo() %>%
      select(
        anno, mes,
        everything()
      )
    
  }

dt_per <-
  map(
    dt_per,
    transformar_general
  )

dt_pt <-
  map(
    dt_pt,
    transformar_general
  )

# guarda excel ----

walk2(
  dt_per,
  names(dt_per),
  ~ {
    if (nrow(.x) <= 1e6) {
      write_xlsx(
        list(.y = .x),
        path = paste0("output/empleo-publico/personas/", .y, ".xlsx")
      )
      message("Guardado: ", .y)
    } else {
      message("OMITIDO (>1M filas): ", .y)
    }
  }
)

walk2(
  dt_pt,
  names(dt_pt),
  ~ {
    if (nrow(.x) <= 1e6) {
      write_xlsx(
        list(.y = .x),
        path = paste0("output/empleo-publico/puestos-de-trabajo/", .y, ".xlsx")
      )
      message("Guardado: ", .y)
    } else {
      message("OMITIDO (>1M filas): ", .y)
    }
  }
)


# guardar minio ----
# guardando personas
local({
  Sys.setenv(
    AWS_ACCESS_KEY_ID     = Sys.getenv("ACCESS"),
    AWS_SECRET_ACCESS_KEY = Sys.getenv("SECRET")
  )
  
  tmp <- tempfile(fileext = ".rds")
  saveRDS(dt_per, file = tmp)
  on.exit(unlink(tmp))
  
  aws.s3::put_object(
    file      = tmp,
    object    = "ooee/trl/6_analisis/6_1_preparacion/empleo-publico/tbl_personas.rds",
    bucket    = "desarrollo",
    region    = "",
    use_https = TRUE,
    base_url  = "api-minio.ine.gob.cl",
    url_style = "path"
  )
})

# guardo pt
local({
  Sys.setenv(
    AWS_ACCESS_KEY_ID     = Sys.getenv("ACCESS"),
    AWS_SECRET_ACCESS_KEY = Sys.getenv("SECRET")
  )
  
  tmp <- tempfile(fileext = ".rds")
  saveRDS(dt_pt, file = tmp)
  on.exit(unlink(tmp))
  
  aws.s3::put_object(
    file      = tmp,
    object    = "ooee/trl/6_analisis/6_1_preparacion/empleo-publico/tbl_puestos-de-trabajo.rds",
    bucket    = "desarrollo",
    region    = "",
    use_https = TRUE,
    base_url  = "api-minio.ine.gob.cl",
    url_style = "path"
  )
})







