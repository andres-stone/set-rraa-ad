#' Guarda una lista de data frames como hojas de un archivo Excel
#'
#' @param lista_resultados Lista nombrada de data frames
#' @param ruta_local Ruta local del archivo Excel de salida
#' @export
guardar_excel <- function(lista_resultados, ruta_local) {
  wb <- openxlsx::createWorkbook()

  purrr::iwalk(lista_resultados, function(datos, nombre_hoja) {
    nombre_safe <- stringr::str_sub(nombre_hoja, 1, 31)
    openxlsx::addWorksheet(wb, nombre_safe)
    openxlsx::writeData(wb, sheet = nombre_safe, x = datos)
  })

  openxlsx::saveWorkbook(wb, ruta_local, overwrite = TRUE)
  message(glue::glue("Excel guardado en: {ruta_local}"))
}

#' Guarda un objeto como RDS en MinIO vía aws.s3
#'
#' @param objeto Objeto R a guardar
#' @param objeto_s3 Ruta del objeto dentro del bucket (sin "s3://bucket/")
#' @param bucket Nombre del bucket (default: "desarrollo")
#' @param endpoint Endpoint MinIO (default: "api-minio.ine.gob.cl")
#' @export
guardar_rds_minio <- function(
    objeto,
    objeto_s3,
    bucket   = "desarrollo",
    endpoint = "api-minio.ine.gob.cl"
) {
  local({
    Sys.setenv(
      AWS_ACCESS_KEY_ID     = Sys.getenv("ACCESS"),
      AWS_SECRET_ACCESS_KEY = Sys.getenv("SECRET")
    )

    tmp <- tempfile(fileext = ".rds")
    saveRDS(objeto, file = tmp)
    on.exit(unlink(tmp))

    aws.s3::put_object(
      file      = tmp,
      object    = objeto_s3,
      bucket    = bucket,
      region    = "",
      use_https = TRUE,
      base_url  = endpoint,
      url_style = "path"
    )
  })

  message(glue::glue("RDS guardado en s3://{bucket}/{objeto_s3}"))
}

#' Lee un archivo RDS desde MinIO vía aws.s3
#'
#' @param objeto_s3 Ruta del objeto dentro del bucket
#' @param bucket Nombre del bucket (default: "desarrollo")
#' @param endpoint Endpoint MinIO (default: "api-minio.ine.gob.cl")
#' @return Objeto R leído desde S3
#' @export
leer_rds_minio <- function(
    objeto_s3,
    bucket   = "desarrollo",
    endpoint = "api-minio.ine.gob.cl"
) {
  local({
    Sys.setenv(
      AWS_ACCESS_KEY_ID     = Sys.getenv("ACCESS"),
      AWS_SECRET_ACCESS_KEY = Sys.getenv("SECRET")
    )

    tmp <- tempfile(fileext = ".rds")
    on.exit(unlink(tmp))

    aws.s3::save_object(
      object        = objeto_s3,
      bucket        = bucket,
      file          = tmp,
      base_url      = endpoint,
      use_https     = TRUE,
      region        = "",
      check_region  = FALSE
    )

    readRDS(tmp)
  })
}

#' Lee un archivo Excel con múltiples hojas como lista de data.tables
#'
#' @param hojas Vector de nombres de hojas
#' @param archivo Ruta local del archivo Excel
#' @return Lista nombrada de data.tables
#' @export
leer_excel_trl <- function(hojas, archivo) {
  hojas %>%
    purrr::set_names() %>%
    purrr::map(
      ~ data.table::as.data.table(
        readxl::read_excel(archivo, sheet = which(hojas == .x))
      )
    )
}

#' Guarda cada tabla de indicadores como Excel individual (si < 1M filas)
#'
#' @param dt_largo Lista nombrada de tibbles en formato largo
#' @param directorio Directorio local de destino
#' @export
guardar_excel_por_tabla <- function(dt_largo, directorio) {
  purrr::walk2(
    dt_largo,
    names(dt_largo),
    function(datos, nombre) {
      if (nrow(datos) <= 1e6) {
        ruta <- file.path(directorio, paste0(nombre, ".xlsx"))
        writexl::write_xlsx(list(datos), path = ruta)
        message(glue::glue("Guardado: {nombre}"))
      } else {
        message(glue::glue("OMITIDO (>1M filas): {nombre}"))
      }
    }
  )
}
