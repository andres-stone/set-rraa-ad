#' Inicializa conexión DuckDB con configuración S3 para MinIO
#'
#' @param access Variable de entorno con el access key (default: "ACCESS")
#' @param secret Variable de entorno con el secret key (default: "SECRET")
#' @param endpoint Endpoint S3 (default: "api-minio.ine.gob.cl")
#' @param uploader_threads Límite de threads para subida (default: 25)
#' @return Conexión DuckDB configurada
#' @export
setup_conexion <- function(
    access           = Sys.getenv("ACCESS"),
    secret           = Sys.getenv("SECRET"),
    endpoint         = "api-minio.ine.gob.cl",
    uploader_threads = 25
) {
  con <- duckdbfs::cached_connection()

  duckdbfs::duckdb_s3_config(
    conn                    = con,
    s3_access_key_id        = access,
    s3_secret_access_key    = secret,
    s3_endpoint             = endpoint,
    s3_region               = "us-east-1",
    s3_url_style            = "path",
    s3_use_ssl              = TRUE,
    s3_uploader_thread_limit = uploader_threads
  )

  DBI::dbExecute(con, "SET http_retries = 5")
  DBI::dbExecute(con, "SET http_retry_wait_ms = 1000")
  DBI::dbExecute(con, "SET http_keep_alive = false")

  con
}
