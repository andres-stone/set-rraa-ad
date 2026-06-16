#' Calcula flujos laborales de dominio fijo (t vs t-12)
#'
#' Entradas: puestos en t que no estaban en t-12.
#' Salidas: puestos en t-12 que no están en t.
#'
#' @param idx Índice del período t en lista_archivos (debe ser >= 13)
#' @param lista_archivos Vector nombrado de rutas S3 (names = fechas)
#' @param bucket Objeto S3Bucket de Arrow
#' @param cols Vector de columnas a leer
#' @param vars_clasificacion Variables de clasificación para agrupar
#' @return Tibble con fecha, vars_clasificacion, pt_ent_12, pt_sal_12
#' @export
procesar_flujo_fijo <- function(idx, lista_archivos, bucket, cols, vars_clasificacion) {
  archivo_t   <- lista_archivos[idx]
  archivo_t12 <- lista_archivos[idx - 12]
  fecha_t     <- lubridate::ymd(names(lista_archivos)[idx])

  message(glue::glue(
    "  Flujo fijo: {fecha_t}  (t={basename(archivo_t)} | t-12={basename(archivo_t12)})"
  ))

  df_t   <- leer_mes(archivo_t,   bucket, cols)
  df_t12 <- leer_mes(archivo_t12, bucket, cols)

  llaves <- c("id_ine_id_trabajador", "id_ine_id_empresa")

  entradas <- dplyr::anti_join(df_t, df_t12, by = llaves) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(vars_clasificacion))) %>%
    dplyr::summarise(pt_ent_12 = dplyr::n(), .groups = "drop")

  salidas <- dplyr::anti_join(df_t12, df_t, by = llaves) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(vars_clasificacion))) %>%
    dplyr::summarise(pt_sal_12 = dplyr::n(), .groups = "drop")

  flujo <- dplyr::full_join(entradas, salidas, by = vars_clasificacion) %>%
    dplyr::mutate(
      fecha     = fecha_t,
      pt_ent_12 = tidyr::replace_na(pt_ent_12, 0L),
      pt_sal_12 = tidyr::replace_na(pt_sal_12, 0L)
    ) %>%
    dplyr::select(fecha, dplyr::all_of(vars_clasificacion), pt_ent_12, pt_sal_12)

  rm(df_t, df_t12, entradas, salidas)
  gc()
  flujo
}

#' Calcula flujos laborales de dominio móvil (t vs t-12) con reclasificaciones
#'
#' Entradas puras: en t, no en t-12.
#' Salidas puras: en t-12, no en t.
#' Reclasificaciones: en ambos períodos pero con cambio en tamano_empresa_movil o tramo_edad.
#'
#' @param idx Índice del período t en lista_archivos (debe ser >= 13)
#' @param lista_archivos Vector nombrado de rutas S3 (names = fechas)
#' @param bucket Objeto S3Bucket de Arrow
#' @param cols Vector de columnas a leer (debe incluir tamano_empresa_movil y tramo_edad)
#' @param vars_clasificacion Variables de clasificación para agrupar
#' @param vars_reclasif Variables que al cambiar entre t-12 y t definen reclasificación
#'   (default: c("tamano_empresa_movil", "tramo_edad"))
#' @return Tibble con fecha, tipo_flujo, vars_clasificacion, n
#' @export
procesar_flujo_dinamico <- function(
    idx,
    lista_archivos,
    bucket,
    cols,
    vars_clasificacion,
    vars_reclasif = c("tamano_empresa_movil", "tramo_edad")
) {
  archivo_t   <- lista_archivos[idx]
  archivo_t12 <- lista_archivos[idx - 12]
  fecha_t     <- lubridate::ymd(names(lista_archivos)[idx])

  message(glue::glue(
    "  Flujo dinámico: {fecha_t}  (t={basename(archivo_t)} | t-12={basename(archivo_t12)})"
  ))

  df_t   <- leer_mes(archivo_t,   bucket, cols)
  df_t12 <- leer_mes(archivo_t12, bucket, cols)

  llaves <- c("id_ine_id_trabajador", "id_ine_id_empresa")

  # entradas puras
  entradas_agg <- dplyr::anti_join(df_t, df_t12, by = llaves) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(vars_clasificacion))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(tipo_flujo = "entrada")

  # salidas puras
  salidas_agg <- dplyr::anti_join(df_t12, df_t, by = llaves) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(vars_clasificacion))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(tipo_flujo = "salida")

  # reclasificaciones: presentes en ambos pero con cambio en vars_reclasif
  reclasif_base <- dplyr::inner_join(
    df_t, df_t12,
    by     = llaves,
    suffix = c("_t", "_t12")
  )

  # condicion de cambio en cualquier variable de reclasificacion
  condicion_cambio <- purrr::reduce(
    vars_reclasif,
    function(acc, v) {
      acc | (reclasif_base[[paste0(v, "_t")]] != reclasif_base[[paste0(v, "_t12")]])
    },
    .init = rep(FALSE, nrow(reclasif_base))
  )
  reclasif_base <- reclasif_base[condicion_cambio, ]

  # reclasif salida: atributos de t-12
  vars_t12 <- stats::setNames(
    paste0(vars_clasificacion, "_t12"),
    vars_clasificacion
  )
  reclasif_salidas_agg <- reclasif_base %>%
    dplyr::rename(dplyr::any_of(vars_t12)) %>%
    dplyr::select(dplyr::all_of(vars_clasificacion)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(vars_clasificacion))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(tipo_flujo = "reclasif_salida")

  # reclasif entrada: atributos de t
  vars_t <- stats::setNames(
    paste0(vars_clasificacion, "_t"),
    vars_clasificacion
  )
  reclasif_entradas_agg <- reclasif_base %>%
    dplyr::rename(dplyr::any_of(vars_t)) %>%
    dplyr::select(dplyr::all_of(vars_clasificacion)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(vars_clasificacion))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(tipo_flujo = "reclasif_entrada")

  tbl_final <- dplyr::bind_rows(
    entradas_agg, salidas_agg,
    reclasif_salidas_agg, reclasif_entradas_agg
  ) %>%
    dplyr::mutate(fecha = fecha_t) %>%
    dplyr::select(fecha, tipo_flujo, dplyr::all_of(vars_clasificacion), n)

  rm(df_t, df_t12, reclasif_base, entradas_agg, salidas_agg,
     reclasif_salidas_agg, reclasif_entradas_agg)
  gc()
  tbl_final
}

#' Ejecuta el cálculo de flujos para todos los períodos desde el mes 13
#'
#' @param lista_archivos Vector nombrado de rutas S3
#' @param fn_flujo Función de cálculo (procesar_flujo_fijo o procesar_flujo_dinamico)
#' @param ... Argumentos adicionales pasados a fn_flujo
#' @return Lista con tbl_maestra (tibble apilado) y fallidos (vector de nombres)
#' @export
ejecutar_flujos <- function(lista_archivos, fn_flujo, ...) {
  indices_procesar <- 13:length(lista_archivos)
  fallidos         <- character(0)

  message(glue::glue(
    "Iniciando flujos: {length(indices_procesar)} meses a procesar..."
  ))

  tbl_maestra <- purrr::map(
    indices_procesar,
    function(idx) {
      tryCatch(
        fn_flujo(idx, lista_archivos, ...),
        error = function(e) {
          archivo_fallido <- basename(lista_archivos[idx])
          message(glue::glue("ERROR en {archivo_fallido}: {e$message}"))
          fallidos <<- c(fallidos, archivo_fallido)
          NULL
        }
      )
    }
  ) %>%
    purrr::compact() %>%
    dplyr::bind_rows()

  if (length(fallidos) > 0) {
    message(glue::glue(
      "ADVERTENCIA: {length(fallidos)} mes(es) fallaron: {paste(fallidos, collapse = ', ')}"
    ))
  } else {
    message("Todos los meses procesados correctamente.")
  }

  list(tbl_maestra = tbl_maestra, fallidos = fallidos)
}

#' Calcula agregado de flujos por variables de agrupación
#'
#' @param data Tibble con columnas fecha, pt_ent_12, pt_sal_12
#' @param variables_agrupacion Vector de variables adicionales de agrupación
#' @return Tibble agregado
#' @export
calcular_agregado_flujos <- function(data, variables_agrupacion) {
  vars_group <- c("fecha", variables_agrupacion)
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(vars_group))) %>%
    dplyr::summarise(
      pt_ent_12 = sum(pt_ent_12),
      pt_sal_12 = sum(pt_sal_12),
      .groups = "drop"
    )
}

#' Calcula agregado de flujos dinámicos por variables de agrupación
#'
#' @param data Tibble con columnas fecha, tipo_flujo, n
#' @param vars_agrupacion Vector de variables adicionales de agrupación
#' @return Tibble agregado con columnas fecha, tipo_flujo, vars_agrupacion, n
#' @export
calcular_agregado_flujos_dinamico <- function(data, vars_agrupacion) {
  vars <- c("fecha", "tipo_flujo", vars_agrupacion)
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(vars))) %>%
    dplyr::summarise(n = sum(n), .groups = "drop")
}
