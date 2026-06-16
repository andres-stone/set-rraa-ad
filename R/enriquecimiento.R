#' Crea vista con el join completo del REP y cálculo de edad_meses
#'
#' Une sexo, nacionalidad, fecha_nac, fecha_def_cor_rc, codigo_region,
#' codigo_comuna desde la tabla de personas. Calcula edad_meses respecto
#' al período de devengamiento. Este es el único punto del pipeline donde
#' se calculan las variables demográficas.
#'
#' @param con Conexión DuckDB (debe tener suseso_view creada)
#' @param ruta_personas Ruta S3 del parquet de personas (output Script 01)
#' @export
crear_vista_rep <- function(con, ruta_personas) {
  DBI::dbExecute(con, "DROP VIEW IF EXISTS rep_view")
  DBI::dbExecute(con, glue::glue(
    "CREATE TEMPORARY VIEW rep_view AS
     SELECT
       id_ine,
       sexo,
       nacionalidad,
       fecha_nac,
       fecha_def_cor_rc,
       codigo_region,
       codigo_comuna
     FROM parquet_scan('{ruta_personas}')"
  ))
}

#' Crea vista con el Marco Maestro de Empresas
#'
#' @param con Conexión DuckDB
#' @param ruta_mme Ruta S3 del parquet MME
#' @export
crear_vista_mme <- function(con, ruta_mme) {
  DBI::dbExecute(con, "DROP VIEW IF EXISTS mme_view")
  DBI::dbExecute(con, glue::glue(
    "CREATE TEMPORARY VIEW mme_view AS
     SELECT
       id_ine_rut,
       seccion_ciiu_4cl,
       division_ciiu_4cl,
       comuna_cut
     FROM parquet_scan('{ruta_mme}')"
  ))
}

#' Crea vista con el flag de empleo público
#'
#' @param con Conexión DuckDB
#' @param ruta_ep Ruta S3 del parquet de empleo público
#' @export
crear_vista_ep <- function(con, ruta_ep) {
  DBI::dbExecute(con, "DROP VIEW IF EXISTS ep_view")
  DBI::dbExecute(con, glue::glue(
    "CREATE TEMPORARY VIEW ep_view AS
     SELECT
       CAST(id_ine AS DOUBLE) AS id_ine,
       razon_social_unidad_legal,
       seccion_ciiu4cl_prin,
       sector_institucional,
       subsector_institucional,
       1 AS ep
     FROM parquet_scan('{ruta_ep}')"
  ))
}

#' Crea vista con tamaño estático por empresa
#'
#' @param con Conexión DuckDB
#' @param ruta_uni Ruta S3 del parquet de tamaño estático
#' @export
crear_vista_tamano_estatico <- function(con, ruta_uni) {
  DBI::dbExecute(con, "DROP VIEW IF EXISTS uni_view")
  DBI::dbExecute(con, glue::glue(
    "CREATE TEMPORARY VIEW uni_view AS
     SELECT id_ine_id_empresa, tamano
     FROM parquet_scan('{ruta_uni}')"
  ))
}

#' Crea vista con moda de nacionalidad por trabajador
#'
#' @param con Conexión DuckDB
#' @param ruta_nacionalidad Ruta S3 del parquet de moda de nacionalidad
#' @export
crear_vista_nacionalidad <- function(con, ruta_nacionalidad) {
  DBI::dbExecute(con, "DROP VIEW IF EXISTS nc_view")
  DBI::dbExecute(con, glue::glue(
    "CREATE TEMPORARY VIEW nc_view AS
     SELECT id_ine_id_trabajador, moda
     FROM parquet_scan('{ruta_nacionalidad}')"
  ))
}

#' Crea vista con región para imputar por empresa
#'
#' @param con Conexión DuckDB
#' @param ruta_region Ruta S3 del parquet de región para imputar
#' @export
crear_vista_region <- function(con, ruta_region) {
  DBI::dbExecute(con, "DROP VIEW IF EXISTS region_view")
  DBI::dbExecute(con, glue::glue(
    "CREATE TEMPORARY VIEW region_view AS
     SELECT id_ine_id_empresa, region_trabajador
     FROM parquet_scan('{ruta_region}')"
  ))
}

#' Crea la vista enriquecida con todas las variables finales
#'
#' Realiza todos los joins (REP, MME, EP, tamaño, nacionalidad, región),
#' calcula edad_meses, depura nacionalidad y región, y genera los tramos
#' etarios. Es el único punto del pipeline donde se calculan estas variables.
#'
#' @param con Conexión DuckDB (todas las vistas auxiliares deben estar creadas)
#' @export
crear_vista_enriquecida <- function(con) {
  DBI::dbExecute(con, "DROP VIEW IF EXISTS enriquecida_view")
  DBI::dbExecute(con, "
    CREATE TEMPORARY VIEW enriquecida_view AS
    SELECT
      s.*,

      -- demografía REP
      r.sexo,
      r.nacionalidad,
      r.fecha_nac,
      r.fecha_def_cor_rc,
      r.codigo_region,
      r.codigo_comuna,

      -- edad en meses (calculada una sola vez aquí)
      (
        (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
         CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
        (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
         CAST(strftime('%m', r.fecha_nac) AS INTEGER))
      ) AS edad_meses,

      -- actividad económica
      m.seccion_ciiu_4cl,
      m.division_ciiu_4cl,
      m.comuna_cut,

      -- empleo público
      e.razon_social_unidad_legal,
      e.seccion_ciiu4cl_prin,
      e.sector_institucional,
      e.subsector_institucional,
      COALESCE(e.ep, 0) AS ep,

      -- tamaño
      u.tamano,

      -- insumos de imputación demográfica
      nc.moda AS nc_moda,
      rg.region_trabajador,

      -- nacionalidad depurada
      CASE
        WHEN TRY_CAST(s.nacionalidad AS INTEGER) IN (88, 99) THEN NULL
        ELSE TRY_CAST(s.nacionalidad AS INTEGER)
      END AS nacionalidad_depurada,

      -- region depurada (FIX: cast a INTEGER en lugar de comparar string flotante)
      CASE
        WHEN TRY_CAST(s.codigo_region AS INTEGER) IN (17, 88, 99) THEN NULL
        ELSE TRY_CAST(s.codigo_region AS INTEGER)
      END AS codigo_region_depurada,

      -- nacionalidad final imputada
      CASE
        WHEN TRY_CAST(s.nacionalidad AS INTEGER) NOT IN (88, 99)
          AND s.nacionalidad IS NOT NULL
          THEN TRY_CAST(s.nacionalidad AS INTEGER)
        WHEN nc.moda IN (88, 99) THEN NULL
        ELSE nc.moda
      END AS nacionalidad_final,

      -- region final imputada
      CASE
        WHEN TRY_CAST(s.codigo_region AS INTEGER) NOT IN (17, 88, 99)
          AND s.codigo_region IS NOT NULL
          THEN TRY_CAST(s.codigo_region AS INTEGER)
        WHEN rg.region_trabajador IN (88, 99) THEN NULL
        ELSE rg.region_trabajador
      END AS region_final,

      -- tramo etario (7 grupos decenales)
      CASE
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 180  THEN 0
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 300  THEN 1
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 420  THEN 2
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 540  THEN 3
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 660  THEN 4
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 780  THEN 5
        ELSE 6
      END AS tramo_edad,

      -- tramo etario 2 (13 grupos quinquenales)
      CASE
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 180  THEN 0
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 240  THEN 1
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 300  THEN 2
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 360  THEN 3
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 420  THEN 4
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 480  THEN 5
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 540  THEN 6
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 600  THEN 7
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 660  THEN 8
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 720  THEN 9
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 780  THEN 10
        WHEN (
          (CAST(s.anno_devengamiento_remuneracion AS INTEGER) * 12 +
           CAST(s.mes_devengamiento_remuneracion  AS INTEGER)) -
          (CAST(strftime('%Y', r.fecha_nac) AS INTEGER) * 12 +
           CAST(strftime('%m', r.fecha_nac) AS INTEGER))
        ) < 840  THEN 11
        ELSE 12
      END AS tramo_edad_2

    FROM suseso_view s

    LEFT JOIN rep_view r
      ON s.id_ine_id_trabajador = r.id_ine

    LEFT JOIN mme_view m
      ON s.id_ine_id_empresa = m.id_ine_rut

    LEFT JOIN ep_view e
      ON s.id_ine_id_empresa = e.id_ine

    LEFT JOIN uni_view u
      ON s.id_ine_id_empresa = u.id_ine_id_empresa

    LEFT JOIN nc_view nc
      ON s.id_ine_id_trabajador = nc.id_ine_id_trabajador

    LEFT JOIN region_view rg
      ON s.id_ine_id_empresa = rg.id_ine_id_empresa
  ")
}
