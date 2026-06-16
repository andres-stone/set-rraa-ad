#' Procesa un archivo mensual y calcula puestos de trabajo por dominio
#'
#' @param archivo Ruta S3 del archivo parquet mensual
#' @param bucket Objeto S3Bucket de Arrow
#' @param cols Vector de columnas a leer (debe incluir llaves + vars clasificación)
#' @param vars_clasificacion Vector de variables de clasificación para agrupar
#' @param col_tamano Nombre columna tamaño (default: "tamano")
#' @param col_sector Nombre columna sector CIIU (default: "seccion_ciiu4cl")
#' @return Tibble con columnas: fecha, vars_clasificacion, pt
#' @export
procesar_archivo_mensual <- function(
    archivo,
    bucket,
    cols,
    vars_clasificacion,
    col_tamano = "tamano",
    col_sector = "seccion_ciiu4cl"
) {
  fecha_archivo <- lubridate::ymd(
    paste0(substr(basename(archivo), 5, 10), "01")
  )

  df <- leer_mes(archivo, bucket, cols, col_tamano, col_sector)

  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(vars_clasificacion))) %>%
    dplyr::summarise(pt = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(fecha = fecha_archivo) %>%
    dplyr::select(fecha, dplyr::everything())
}

#' Calcula un agregado de puestos de trabajo por variables de agrupación
#'
#' @param data Tibble con columna fecha y pt
#' @param variables_agrupacion Vector de variables adicionales de agrupación
#' @return Tibble con fecha, variables_agrupacion y pt sumados
#' @export
calcular_agregado_pt <- function(data, variables_agrupacion) {
  vars_group <- c("fecha", variables_agrupacion)
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(vars_group))) %>%
    dplyr::summarise(pt = sum(pt), .groups = "drop")
}

#' Ejecuta el cálculo de puestos de trabajo para todos los archivos
#'
#' @param lista_archivos Vector nombrado de rutas S3 (names = fechas)
#' @param bucket Objeto S3Bucket de Arrow
#' @param cols Vector de columnas a leer
#' @param vars_clasificacion Variables de clasificación
#' @return Tibble con todos los períodos apilados
#' @export
ejecutar_puestos <- function(lista_archivos, bucket, cols, vars_clasificacion) {
  purrr::map(
    lista_archivos,
    function(archivo) {
      message(glue::glue("Procesando: {basename(archivo)}"))
      tryCatch(
        procesar_archivo_mensual(archivo, bucket, cols, vars_clasificacion),
        error = function(e) {
          message(glue::glue("ERROR en {basename(archivo)}: {e$message}"))
          NULL
        }
      )
    }
  ) %>%
    purrr::compact() %>%
    dplyr::bind_rows()
}
