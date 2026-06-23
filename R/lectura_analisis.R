#' Inicializa un bucket Arrow autenticado para lectura desde MinIO
#'
#' @param bucket_name Nombre del bucket S3 (default: "desarrollo")
#' @param access Variable de entorno con el access key (default: "ACCESS")
#' @param secret Variable de entorno con el secret key (default: "SECRET")
#' @param endpoint Endpoint S3 (default: "api-minio.ine.gob.cl")
#' @return Objeto S3Bucket de Arrow
#' @export
setup_bucket_arrow <- function(
    bucket_name = "desarrollo",
    access      = Sys.getenv("ACCESS"),
    secret      = Sys.getenv("SECRET"),
    endpoint    = "api-minio.ine.gob.cl"
) {
  arrow::s3_bucket(
    bucket_name,
    access_key        = access,
    secret_key        = secret,
    endpoint_override = endpoint,
    region            = "us-east-1",
    scheme            = "https"
  )
}

#' Obtiene lista ordenada de archivos parquet desde una ruta S3
#'
#' @param con Conexión DuckDB
#' @param ruta_calculo Ruta S3 base (ej. "s3://desarrollo/ooee/trl/...")
#' @return Vector de rutas de archivos, nombrado por fecha (YYYY-MM-DD)
#' @export
get_lista_archivos <- function(con, ruta_calculo) {
  archivos <- DBI::dbGetQuery(
    con,
    glue::glue("SELECT file FROM glob('{ruta_calculo}/**/*.parquet') ORDER BY file")
  )$file

  fechas <- lubridate::ymd(
    paste0(substr(basename(archivos), 5, 10), "01")
  )
  stats::setNames(archivos, as.character(fechas))
}

#' Lee un archivo parquet mensual desde S3 con filtros base
#'
#' Aplica los filtros estándar del pipeline (excluye unipersonales y
#' secciones T/U), selecciona las columnas indicadas y deduplica por
#' puesto de trabajo (trabajador × empresa).
#'
#' @param archivo Ruta S3 completa del archivo
#' @param bucket Objeto S3Bucket de Arrow (creado con setup_bucket_arrow)
#' @param cols Vector de nombres de columnas a seleccionar
#' @param col_tamano Nombre de la columna de tamaño (default: "tamano")
#' @param col_sector Nombre de la columna de sector CIIU (default: "seccion_ciiu4cl")
#' @return Data frame deduplicado por (id_ine_id_trabajador, id_ine_id_empresa)
#' @export
leer_mes <- function(
    archivo,
    bucket,
    cols,
    col_tamano = "tamano",
    col_sector = "seccion_ciiu4cl"
) {
  ruta_en_bucket <- sub("^s3://desarrollo/", "", archivo)

  arrow::read_parquet(
    bucket$path(ruta_en_bucket),
    as_data_frame = FALSE
  ) %>%
    dplyr::filter(
      .data[[col_tamano]] != "unipersonal",
      !.data[[col_sector]] %in% c("T", "U") | is.na(.data[[col_sector]])
    ) %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    dplyr::collect() %>%
    dplyr::distinct(id_ine_id_trabajador, id_ine_id_empresa, .keep_all = TRUE)
}
