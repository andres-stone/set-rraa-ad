#' Abre un dataset parquet desde S3 como lazy table de DuckDB
#'
#' @param ruta Ruta S3 del dataset
#' @param unify_schemas Si TRUE unifica esquemas entre particiones
#' @param recursive Si TRUE escanea subdirectorios
#' @return Lazy table de duckdbfs
#' @export
open_parquet <- function(ruta, unify_schemas = FALSE, recursive = TRUE) {
  duckdbfs::open_dataset(
    ruta,
    format        = "parquet",
    unify_schemas = unify_schemas,
    recursive     = recursive
  )
}

#' Escribe un período a parquet en S3 con reintentos
#'
#' @param con Conexión DuckDB
#' @param sql_query SQL a ejecutar como string (usado en COPY ... TO)
#' @param ruta_archivo Ruta S3 destino del archivo parquet
#' @param periodo_label Etiqueta del período para logging (ej. "2023-05")
#' @param reintentos Número máximo de reintentos ante error
#' @return TRUE si exitoso, FALSE si agotó reintentos
#' @export
write_periodo <- function(con, sql_query, ruta_archivo, periodo_label, reintentos = 3) {
  intento  <- 1
  resultado <- FALSE

  while (intento <= reintentos) {

    intento_actual <- intento

    resultado <- tryCatch({

      DBI::dbExecute(con, glue::glue(
        "COPY ({sql_query}) TO '{ruta_archivo}'
         (FORMAT PARQUET, OVERWRITE_OR_IGNORE TRUE)"
      ))

      message(glue::glue("[OK]     {periodo_label}"))
      TRUE

    }, error = function(e) {

      if (grepl("502|HTTP", e$message)) {
        message(glue::glue("[502]    {periodo_label} - intento {intento_actual}/{reintentos} - esperando 5s..."))
      } else {
        message(glue::glue("[ERROR]  {periodo_label} - intento {intento_actual}/{reintentos}: {e$message}"))
      }
      Sys.sleep(5)
      FALSE

    })

    if (resultado) break
    intento <- intento + 1
  }

  if (!resultado) {
    warning(glue::glue("[FALLO]  {periodo_label} - agotados {reintentos} reintentos"))
  }

  resultado
}

#' Obtiene lista de períodos disponibles desde un dataset
#'
#' @param dt Lazy table con columnas anno_devengamiento_remuneracion y mes_devengamiento_remuneracion
#' @param filtro_desde_anno Anno mínimo a incluir (default: 2016)
#' @param filtro_desde_mes Mes mínimo del anno inicial (default: 11)
#' @return Tibble con columnas anno, mes, periodo_int, ordenado cronológicamente
#' @export
get_periodos <- function(dt, filtro_desde_anno = 2016, filtro_desde_mes = 11) {
  dt %>%
    dplyr::filter(
      anno_devengamiento_remuneracion > filtro_desde_anno |
        (anno_devengamiento_remuneracion == filtro_desde_anno &
           mes_devengamiento_remuneracion >= filtro_desde_mes)
    ) %>%
    dplyr::distinct(
      anno_devengamiento_remuneracion,
      mes_devengamiento_remuneracion
    ) %>%
    dplyr::collect() %>%
    dplyr::arrange(
      anno_devengamiento_remuneracion,
      mes_devengamiento_remuneracion
    ) %>%
    dplyr::mutate(
      anno       = anno_devengamiento_remuneracion,
      mes        = mes_devengamiento_remuneracion,
      periodo_int = anno_devengamiento_remuneracion * 100 + mes_devengamiento_remuneracion
    ) %>%
    dplyr::select(anno, mes, periodo_int)
}

#' Ejecuta write_periodo para todos los períodos de un tibble
#'
#' @param periodos Tibble con columnas anno y mes
#' @param fn_sql Función que recibe (anio, mes) y devuelve el SQL a ejecutar
#' @param fn_ruta Función que recibe (anio, mes) y devuelve la ruta S3 destino
#' @param con Conexión DuckDB
#' @param reintentos Número máximo de reintentos
#' @return Vector de períodos fallidos (character)
#' @export
escribir_periodos <- function(periodos, fn_sql, fn_ruta, con, reintentos = 3) {
  fallidos <- c()

  purrr::walk2(
    periodos$anno,
    periodos$mes,
    function(anio, mes) {
      mes_fmt       <- formatC(mes, width = 2, flag = "0")
      periodo_label <- glue::glue("{anio}-{mes_fmt}")
      ruta_archivo  <- fn_ruta(anio, mes)
      sql_query     <- fn_sql(anio, mes)

      ok <- write_periodo(con, sql_query, ruta_archivo, periodo_label, reintentos)
      if (!ok) fallidos <<- c(fallidos, periodo_label)
    }
  )

  fallidos
}

#' Imprime reporte final de períodos fallidos
#'
#' @param fallidos Vector de períodos fallidos
#' @param ruta_salida Ruta S3 de destino (para el mensaje de éxito)
#' @export
reporte_fallidos <- function(fallidos, ruta_salida) {
  if (length(fallidos) == 0) {
    message("\nProceso completado sin errores en: ", ruta_salida)
  } else {
    message("\nProceso completado con ", length(fallidos), " período(s) fallido(s):")
    purrr::walk(fallidos, ~ message("  - ", .x))
  }
}
