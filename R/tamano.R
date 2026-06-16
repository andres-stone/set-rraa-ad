#' Calcula la plantilla de trabajadores únicos por empresa y mes
#'
#' @param con Conexión DuckDB (debe tener imputado_view creada)
#' @param ruta_destino Ruta S3 donde guardar pt_empresas.parquet
#' @export
calcular_plantilla_mensual <- function(con, ruta_destino) {
  DBI::dbExecute(con, glue::glue("
    COPY (
      SELECT
        id_ine_id_empresa,
        fecha,
        COUNT(DISTINCT id_ine_id_trabajador) AS n_trabajadores_movil
      FROM imputado_view
      GROUP BY id_ine_id_empresa, fecha
      ORDER BY id_ine_id_empresa, fecha
    ) TO '{ruta_destino}/pt_empresas.parquet'
    (FORMAT PARQUET, OVERWRITE_OR_IGNORE TRUE)
  "))
}

#' Calcula el tamaño estático de empresa (promedio histórico)
#'
#' @param con Conexión DuckDB (debe tener tamano_movil_view creada)
#' @param ruta_destino Ruta S3 donde guardar tamano_estatico.parquet
#' @export
calcular_tamano_estatico <- function(con, ruta_destino) {
  DBI::dbExecute(con, glue::glue("
    COPY (
      WITH base AS (
        SELECT
          id_ine_id_empresa,
          SUM(n_trabajadores_movil) AS pt,
          COUNT(DISTINCT fecha)     AS t
        FROM tamano_movil_view
        GROUP BY id_ine_id_empresa
      )
      SELECT
        id_ine_id_empresa,
        pt,
        t,
        pt * 1.0 / t AS media_pt,
        CASE
          WHEN pt * 1.0 / t = 1   THEN 'unipersonal'
          WHEN pt * 1.0 / t < 5   THEN 'micro'
          WHEN pt * 1.0 / t < 50  THEN 'pequena'
          WHEN pt * 1.0 / t < 200 THEN 'mediana'
          ELSE 'grande'
        END AS tamano
      FROM base
    ) TO '{ruta_destino}/tamano_estatico.parquet'
    (FORMAT PARQUET, OVERWRITE_OR_IGNORE TRUE)
  "))
}

#' Crea la vista de media móvil de 12 meses por empresa
#'
#' La media móvil se reinicia si hay un salto mayor a 12 meses entre
#' observaciones consecutivas de la misma empresa.
#'
#' @param con Conexión DuckDB (debe tener tamano_movil_view creada)
#' @export
crear_media_movil_view <- function(con) {
  DBI::dbExecute(con, "DROP VIEW IF EXISTS media_movil_view")
  DBI::dbExecute(con, "
    CREATE TEMPORARY VIEW media_movil_view AS
    WITH
    con_lag AS (
      SELECT
        id_ine_id_empresa,
        fecha,
        n_trabajadores_movil,
        LAG(fecha) OVER (
          PARTITION BY id_ine_id_empresa
          ORDER BY fecha
        ) AS fecha_anterior
      FROM tamano_movil_view
    ),
    con_grupos AS (
      SELECT
        id_ine_id_empresa,
        fecha,
        n_trabajadores_movil,
        SUM(
          CASE
            WHEN fecha_anterior IS NULL
              OR DATEDIFF('month', fecha_anterior, fecha) > 12
            THEN 1 ELSE 0
          END
        ) OVER (
          PARTITION BY id_ine_id_empresa
          ORDER BY fecha
        ) AS grupo
      FROM con_lag
    ),
    con_media AS (
      SELECT
        a.id_ine_id_empresa,
        a.fecha,
        a.n_trabajadores_movil,
        a.grupo,
        AVG(b.n_trabajadores_movil) AS media_movil_12m,
        COUNT(b.fecha)              AS n_meses_utilizados
      FROM con_grupos a
      INNER JOIN con_grupos b
        ON  a.id_ine_id_empresa = b.id_ine_id_empresa
        AND a.grupo             = b.grupo
        AND b.fecha             > (a.fecha - INTERVAL 12 MONTHS)
        AND b.fecha            <= a.fecha
      GROUP BY
        a.id_ine_id_empresa, a.fecha,
        a.n_trabajadores_movil, a.grupo
    )
    SELECT
      id_ine_id_empresa,
      fecha,
      n_trabajadores_movil AS pt,
      media_movil_12m,
      n_meses_utilizados,
      CASE
        WHEN media_movil_12m < 5   THEN 'micro'
        WHEN media_movil_12m < 50  THEN 'pequena'
        WHEN media_movil_12m < 200 THEN 'mediana'
        ELSE 'grande'
      END AS tamano_empresa_movil
    FROM con_media
  ")
}
