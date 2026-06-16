
# Orquestador del pipeline TRL
# Ejecuta los 5 scripts en secuencia con logging de tiempo y manejo de errores.
# Permite retomar desde una etapa específica usando el argumento `desde`.

# uso:
#   source("orquestador.R")                         # ejecuta todo
#   source("orquestador.R"); pipeline(desde = "03") # retoma desde script 03

library(tictoc)

etapas <- list(
  list(id = "01", nombre = "personas",       ruta = "scripts/01-personas.R"),
  list(id = "02", nombre = "preprocesadas",  ruta = "scripts/02-preprocesadas.R"),
  list(id = "03", nombre = "imputadas",      ruta = "scripts/03-imputadas.R"),
  list(id = "04", nombre = "tamano",         ruta = "scripts/04-tamano.R"),
  list(id = "05", nombre = "completas",      ruta = "scripts/05-completas.R")
)

ejecutar_etapa <- function(etapa) {
  label <- glue::glue("[{etapa$id}] {etapa$nombre}")
  message("\n", strrep("=", 60))
  message(glue::glue("INICIANDO: {label}"))
  message(strrep("=", 60))

  tictoc::tic(label)

  resultado <- tryCatch({
    source(etapa$ruta, local = new.env(parent = globalenv()))
    message(glue::glue("\n[OK] {label} completado"))
    TRUE
  }, error = function(e) {
    message(glue::glue("\n[ERROR] {label} falló: {e$message}"))
    FALSE
  })

  tictoc::toc()
  resultado
}

pipeline <- function(desde = "01", hasta = "05") {
  etapas_a_correr <- Filter(
    function(e) e$id >= desde & e$id <= hasta,
    etapas
  )

  log_resultado <- list()
  tictoc::tic("Pipeline completo")

  for (etapa in etapas_a_correr) {
    ok <- ejecutar_etapa(etapa)
    log_resultado[[etapa$id]] <- list(etapa = etapa$nombre, ok = ok)

    if (!ok) {
      message(glue::glue(
        "\n[DETENIDO] Pipeline interrumpido en etapa {etapa$id} ({etapa$nombre}).",
        "\nPara retomar: pipeline(desde = '{etapa$id}')"
      ))
      break
    }
  }

  tictoc::toc()

  # resumen final
  message("\n", strrep("=", 60))
  message("RESUMEN DEL PIPELINE")
  message(strrep("=", 60))
  purrr::walk(log_resultado, function(r) {
    estado <- if (r$ok) "[OK]  " else "[FAIL]"
    message(glue::glue("  {estado}  {r$etapa}"))
  })

  invisible(log_resultado)
}

# ejecucion directa al hacer source() sin argumentos
if (!exists(".pipeline_sourced")) {
  .pipeline_sourced <- TRUE
  pipeline()
}
