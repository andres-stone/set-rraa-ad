#' Crea la vista base para imputación a partir del dataset truncado
#'
#' La vista agrega periodo_int para permitir lookups eficientes de t-1 y t+1.
#' Usa dt_truncada (con tope imponible ya aplicado) — no dt_suseso cruda.
#'
#' @param con Conexión DuckDB
#' @param dt_truncada Lazy table de preprocesadas con tope aplicado
#' @export
crear_base_view <- function(con, dt_truncada) {
  DBI::dbExecute(con, "DROP VIEW IF EXISTS base_view")
  DBI::dbExecute(con, glue::glue(
    "CREATE TEMPORARY VIEW base_view AS
     SELECT *,
       CAST(anno_devengamiento_remuneracion AS INTEGER) * 100 +
       CAST(mes_devengamiento_remuneracion  AS INTEGER) AS periodo_int
     FROM ({dbplyr::remote_query(dt_truncada)})"
  ))
}

#' Genera el SQL de imputación para un período dado
#'
#' Para períodos borde (primero o último) escribe los registros tal cual.
#' Para períodos intermedios detecta ausentes en t presentes en t-1 y t+1,
#' imputa remuneración y días como promedio (o máximo si uno es cero), y
#' hace UNION ALL con los registros observados.
#'
#' Solo se imputan variables laborales: monto_remuneracion y n_dias_trabajados.
#' Las variables demográficas se agregan en el Script 05.
#'
#' @param anio Año del período a imputar
#' @param mes Mes del período a imputar
#' @param periodo_actual Integer YYYYMM del período actual
#' @param periodo_ant Integer YYYYMM del período anterior (NA si es primero)
#' @param periodo_sig Integer YYYYMM del período siguiente (NA si es último)
#' @return SQL string listo para usar en COPY (...) TO
#' @export
sql_imputacion <- function(anio, mes, periodo_actual, periodo_ant, periodo_sig) {

  es_borde <- is.na(periodo_ant) || is.na(periodo_sig)

  if (es_borde) {
    return(glue::glue(
      "SELECT * EXCLUDE (periodo_int)
       FROM base_view
       WHERE periodo_int = {periodo_actual}"
    ))
  }

  glue::glue("
    WITH
    t AS (
      SELECT * FROM base_view
      WHERE periodo_int = {periodo_actual}
    ),
    t_ant AS (
      SELECT
        id_ine_id_trabajador,
        id_ine_id_empresa,
        monto_remuneracion AS rem_ant,
        n_dias_trabajados  AS dias_ant
      FROM base_view
      WHERE periodo_int = {periodo_ant}
    ),
    t_sig AS (
      SELECT
        id_ine_id_trabajador,
        id_ine_id_empresa,
        monto_remuneracion AS rem_sig,
        n_dias_trabajados  AS dias_sig
      FROM base_view
      WHERE periodo_int = {periodo_sig}
    ),
    ausentes AS (
      SELECT
        t_ant.id_ine_id_trabajador,
        t_ant.id_ine_id_empresa,
        t_ant.rem_ant,
        t_ant.dias_ant,
        t_sig.rem_sig,
        t_sig.dias_sig
      FROM t_ant
      INNER JOIN t_sig
        ON  t_ant.id_ine_id_trabajador = t_sig.id_ine_id_trabajador
        AND t_ant.id_ine_id_empresa     = t_sig.id_ine_id_empresa
      WHERE NOT EXISTS (
        SELECT 1 FROM t
        WHERE t.id_ine_id_trabajador = t_ant.id_ine_id_trabajador
          AND t.id_ine_id_empresa    = t_ant.id_ine_id_empresa
      )
    ),
    imputados AS (
      SELECT
        id_ine_id_trabajador,
        id_ine_id_empresa,
        CASE
          WHEN COALESCE(rem_ant, 0) > 0 AND COALESCE(rem_sig, 0) > 0
            THEN (rem_ant + rem_sig) / 2.0
          ELSE GREATEST(COALESCE(rem_ant, 0), COALESCE(rem_sig, 0))
        END AS monto_remuneracion,
        CASE
          WHEN COALESCE(dias_ant, 0) > 0 AND COALESCE(dias_sig, 0) > 0
            THEN CAST((dias_ant + dias_sig) / 2.0 AS INTEGER)
          ELSE GREATEST(COALESCE(dias_ant, 0), COALESCE(dias_sig, 0))
        END AS n_dias_trabajados,
        {anio} AS anno_devengamiento_remuneracion,
        {mes}  AS mes_devengamiento_remuneracion
      FROM ausentes
    )
    SELECT * EXCLUDE (periodo_int) FROM t
    UNION ALL
    SELECT * FROM imputados
  ")
}

#' Imputa lagunas de un mes y escribe los períodos a S3
#'
#' @param con Conexión DuckDB (debe tener base_view ya creada)
#' @param periodos Tibble con columnas anno, mes, periodo_int
#' @param ruta_salida Ruta S3 base de destino
#' @param reintentos Número máximo de reintentos por período
#' @return Vector de períodos fallidos
#' @export
imputar_lagunas <- function(con, periodos, ruta_salida, reintentos = 3) {
  n <- nrow(periodos)

  fn_sql <- function(anio, mes) {
    idx          <- which(periodos$anno == anio & periodos$mes == mes)
    periodo_actual <- periodos$periodo_int[idx]
    periodo_ant    <- if (idx > 1) periodos$periodo_int[idx - 1] else NA_integer_
    periodo_sig    <- if (idx < n) periodos$periodo_int[idx + 1] else NA_integer_
    sql_imputacion(anio, mes, periodo_actual, periodo_ant, periodo_sig)
  }

  fn_ruta <- function(anio, mes) {
    mes_fmt <- formatC(mes, width = 2, flag = "0")
    glue::glue("{ruta_salida}/anno={anio}/mes={mes_fmt}/trl_{anio}{mes_fmt}.parquet")
  }

  escribir_periodos(periodos, fn_sql, fn_ruta, con, reintentos)
}
