
# Script 01 — Obtención de datos sociodemográficos del REP
# Produce: pqt_personas_nuevo.parquet
# Columnas: id_ine, fecha_nac, fecha_def_cor_rc, sexo, nacionalidad,
#            codigo_region, codigo_comuna

rm(list = ls())
options(scipen = 999, digits = 4)
gc()

devtools::load_all(here::here())

# conexion ----
con <- setup_conexion()

# rutas ----
ruta_rep          <- "s3://activos/infraestructura/rep/pseudonimizado"
ruta_rep_nuevo    <- "s3://desarrollo/ooee/trl/rep"
ruta_suseso_sx    <- "s3://desarrollo/ooee/trl/5_procesamiento/5_2_clasificacion_codificacion/espejo/ultimo_sexo_suseso.parquet"
ruta_personas_out <- "s3://desarrollo/ooee/trl/5_procesamiento/5_2_clasificacion_codificacion/espejo/pqt_personas_nuevo.parquet"

# obtencion de datasets ----
DBI::dbExecute(con, glue::glue(
  "CREATE OR REPLACE TEMPORARY VIEW DEMOGRAFICA AS
   SELECT * FROM parquet_scan('{ruta_rep}/demographic_table_2024.parquet')"
))
DBI::dbExecute(con, glue::glue(
  "CREATE OR REPLACE TEMPORARY VIEW GEOGRAFICA AS
   SELECT * FROM parquet_scan('{ruta_rep}/geographic_table_2024.parquet')"
))

dt_rep_nuevo <- open_parquet(ruta_rep_nuevo)

# preprocesamiento ----
dt_demografica <- dplyr::tbl(con, "demografica") %>%
  dplyr::mutate(
    sexo_rc         = dplyr::if_else(sexo %in% c(1, 2), sexo, NA_real_),
    nacionalidad_rc = dplyr::if_else(nacionalidad == 3, 1, nacionalidad)
  ) %>%
  dplyr::select(id_ine, sexo, nacionalidad_rc, fecha_nac, fecha_def_cor_rc)

dt_geografica <- dplyr::tbl(con, "geografica") %>%
  dplyr::select(id_ine, codigo_region, codigo_comuna)

# REP 2024
dt_personas_rc <- dplyr::full_join(dt_demografica, dt_geografica, by = "id_ine")

# REP 2025
dt_rep_nuevo <- dt_rep_nuevo %>%
  dplyr::select(
    id_ine            = id_ine_run_dv,
    sexo              = sexo_rc,
    nacionalidad_rc,
    fecha_nac         = fecha_nac_cor_rc,
    fecha_def_cor_rc,
    codigo_region     = codigo_region_rc,
    codigo_comuna     = codigo_comuna_rc
  ) %>%
  dplyr::mutate(
    nacionalidad_rc = dplyr::case_when(
      nacionalidad_rc == 2 ~ 1,
      nacionalidad_rc == 3 ~ 2,
      TRUE ~ nacionalidad_rc
    )
  )

# personas solo en 2025
dt_rep_anti <- dplyr::anti_join(
  dt_rep_nuevo,
  dplyr::select(dt_personas_rc, id_ine),
  by = "id_ine"
)

# REP completo (2024 base + nuevos 2025)
dt_rep <- dplyr::union_all(
  dplyr::mutate(dt_personas_rc,
    codigo_region = as.character(codigo_region),
    codigo_comuna = as.character(codigo_comuna)
  ),
  dplyr::mutate(dt_rep_anti,
    codigo_region = as.character(codigo_region),
    codigo_comuna = as.character(codigo_comuna)
  )
)

# completitud sexo usando SUSESO como fallback
dt_personas_suseso <- open_parquet(ruta_suseso_sx, recursive = FALSE) %>%
  dplyr::select(id_ine_id_trabajador, sexo_final)

dt_personas <- dt_rep %>%
  dplyr::full_join(dt_personas_suseso, by = c("id_ine" = "id_ine_id_trabajador")) %>%
  dplyr::mutate(
    sexo = dplyr::if_else(
      is.na(sexo) | !sexo %in% 1:2,
      as.double(sexo_final),
      sexo
    )
  ) %>%
  dplyr::select(
    id_ine, fecha_nac, fecha_def_cor_rc,
    sexo, nacionalidad = nacionalidad_rc,
    codigo_region, codigo_comuna
  )

# escritura ----
tictoc::tic()
duckdbfs::write_dataset(dt_personas, path = ruta_personas_out, format = "parquet")
tictoc::toc()
