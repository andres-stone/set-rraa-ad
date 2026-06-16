
# ============================================================
# Dashboard: Crédito Tributario Histórico
# ============================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)
library(lubridate)
library(tidyr)

# # ── DATOS SIMULADOS ─────────────────────────────────────────
# set.seed(42)
# 
# periodos <- expand.grid(
#   ano = 2019:2024,
#   mes = 1:12
# ) %>%
#   arrange(ano, mes) %>%
#   filter(!(ano == 2024 & mes > 6))  # hasta jun 2024
# 
# n <- nrow(periodos)
# 
# # Tamaños de empresa
# tamanos <- c("Micro", "Pequeña", "Mediana", "Grande")
# # Secciones CIIU
# secciones <- c("A - Agricultura", "C - Manufactura", "F - Construcción",
#                "G - Comercio", "H - Transporte", "I - Alojamiento",
#                "J - Información", "K - Financiero", "M - Profesional",
#                "N - Servicios admin")

library(readxl)

ruta <- "../output/credito-tributario/tablas/credito_tributario_historico.xlsx"

tbl_pt_sx_nc  <- read_xlsx(ruta, sheet = "pt_sx_nc")
tbl_pt_tamano <- read_xlsx(ruta, sheet = "pt_tamano")
tbl_pt_sector <- read_xlsx(ruta, sheet = "pt_sector")
tbl_ee_tamano <- read_xlsx(ruta, sheet = "ee_tamano")
tbl_ee_sector <- read_xlsx(ruta, sheet = "ee_sector")
tbl_cl_tamano <- read_xlsx(ruta, sheet = "cl_tamano")
tbl_ct_tamano <- read_xlsx(ruta, sheet = "ct_tamano")


# ── tbl_pt_sx_nc: puestos por sexo × nacionalidad ────────────
tbl_pt_sx_nc <- expand.grid(
  ano         = 2019:2024,
  mes         = 1:12,
  sexo        = c("Hombre", "Mujer"),
  nacionalidad = c("Chilena", "Extranjera")
) %>%
  filter(!(ano == 2024 & mes > 6)) %>%
  mutate(
    base = case_when(
      sexo == "Hombre" & nacionalidad == "Chilena"    ~ 0.52,
      sexo == "Mujer"  & nacionalidad == "Chilena"    ~ 0.38,
      sexo == "Hombre" & nacionalidad == "Extranjera" ~ 0.07,
      TRUE ~ 0.03
    ),
    pt = round(base * 1e6 * (1 + (ano - 2019) * 0.03) * runif(n(), 0.97, 1.03)),
    fecha = as.Date(paste(ano, mes, 1, sep = "-"))
  ) %>%
  group_by(ano, mes) %>%
  mutate(participacion = pt / sum(pt)) %>%
  ungroup()

# ── tbl_pt_tamano: puestos por tamaño empresa ────────────────
tbl_pt_tamano <- expand.grid(
  ano            = 2019:2024,
  mes            = 1:12,
  tamano_empresa = tamanos
) %>%
  filter(!(ano == 2024 & mes > 6)) %>%
  mutate(
    base = case_when(
      tamano_empresa == "Grande"  ~ 0.45,
      tamano_empresa == "Mediana" ~ 0.25,
      tamano_empresa == "Pequeña" ~ 0.20,
      TRUE                        ~ 0.10
    ),
    pt = round(base * 2e6 * (1 + (ano - 2019) * 0.025) * runif(n(), 0.97, 1.03))
  ) %>%
  group_by(ano, mes) %>%
  mutate(participacion = pt / sum(pt)) %>%
  ungroup()

# ── tbl_pt_sector: puestos por sección CIIU ──────────────────
bases_sector <- c(0.18, 0.16, 0.07, 0.22, 0.08, 0.04, 0.06, 0.05, 0.08, 0.06)
tbl_pt_sector <- expand.grid(
  ano              = 2019:2024,
  mes              = 1:12,
  seccion_ciiu4cl  = secciones
) %>%
  filter(!(ano == 2024 & mes > 6)) %>%
  left_join(
    data.frame(seccion_ciiu4cl = secciones, base = bases_sector),
    by = "seccion_ciiu4cl"
  ) %>%
  mutate(
    pt = round(base * 2e6 * (1 + (ano - 2019) * 0.02) * runif(n(), 0.95, 1.05))
  ) %>%
  group_by(ano, mes) %>%
  mutate(participacion = pt / sum(pt)) %>%
  ungroup()

# ── tbl_ee_tamano: empresas por tamaño ───────────────────────
tbl_ee_tamano <- expand.grid(
  ano            = 2019:2024,
  mes            = 1:12,
  tamano_empresa = tamanos
) %>%
  filter(!(ano == 2024 & mes > 6)) %>%
  mutate(
    base = case_when(
      tamano_empresa == "Micro"   ~ 0.55,
      tamano_empresa == "Pequeña" ~ 0.25,
      tamano_empresa == "Mediana" ~ 0.12,
      TRUE                        ~ 0.08
    ),
    n_e = round(base * 80000 * (1 + (ano - 2019) * 0.02) * runif(n(), 0.97, 1.03))
  ) %>%
  group_by(ano, mes) %>%
  mutate(participacion = n_e / sum(n_e)) %>%
  ungroup()

# ── tbl_ee_sector: empresas por sección CIIU ─────────────────
tbl_ee_sector <- expand.grid(
  ano              = 2019:2024,
  mes              = 1:12,
  seccion_ciiu4cl  = secciones
) %>%
  filter(!(ano == 2024 & mes > 6)) %>%
  left_join(
    data.frame(seccion_ciiu4cl = secciones, base = bases_sector),
    by = "seccion_ciiu4cl"
  ) %>%
  mutate(
    n_e = round(base * 80000 * (1 + (ano - 2019) * 0.015) * runif(n(), 0.95, 1.05))
  ) %>%
  group_by(ano, mes) %>%
  mutate(participacion = n_e / sum(n_e)) %>%
  ungroup()

# ── tbl_cl_tamano: masa salarial por tamaño ──────────────────
tbl_cl_tamano <- expand.grid(
  ano            = 2019:2024,
  mes            = 1:12,
  tamano_empresa = tamanos
) %>%
  filter(!(ano == 2024 & mes > 6)) %>%
  mutate(
    base = case_when(
      tamano_empresa == "Grande"  ~ 0.50,
      tamano_empresa == "Mediana" ~ 0.24,
      tamano_empresa == "Pequeña" ~ 0.18,
      TRUE                        ~ 0.08
    ),
    monto_clp = round(base * 2e12 * (1 + (ano - 2019) * 0.04) * runif(n(), 0.97, 1.03)),
    monto_usd = round(monto_clp / (880 + runif(n(), -20, 40)))
  ) %>%
  group_by(ano, mes) %>%
  mutate(
    participacion_clp = monto_clp / sum(monto_clp),
    participacion_usd = monto_usd / sum(monto_usd)
  ) %>%
  ungroup()

# ── tbl_ct_tamano: crédito tributario por tamaño ─────────────
tbl_ct_tamano <- expand.grid(
  ano            = 2019:2024,
  mes            = 1:12,
  tamano_empresa = tamanos
) %>%
  filter(!(ano == 2024 & mes > 6)) %>%
  mutate(
    base = case_when(
      tamano_empresa == "Grande"  ~ 0.48,
      tamano_empresa == "Mediana" ~ 0.25,
      tamano_empresa == "Pequeña" ~ 0.19,
      TRUE                        ~ 0.08
    ),
    ct_clp = round(base * 3e11 * (1 + (ano - 2019) * 0.035) * runif(n(), 0.97, 1.03)),
    ct_usd = round(ct_clp / (880 + runif(n(), -20, 40)))
  ) %>%
  group_by(ano, mes) %>%
  mutate(
    participacion_clp = ct_clp / sum(ct_clp),
    participacion_usd = ct_usd / sum(ct_usd)
  ) %>%
  ungroup()

# ── PALETA ───────────────────────────────────────────────────
pal_tamano  <- c("Micro" = "#2d6a9f", "Pequeña" = "#4fb3bf",
                 "Mediana" = "#f0a500", "Grande" = "#c0392b")
pal_sexo    <- c("Hombre" = "#2d6a9f", "Mujer" = "#e05c8a")
pal_nac     <- c("Chilena" = "#2d6a9f", "Extranjera" = "#f0a500")
pal_sector  <- scales::hue_pal()(10)
names(pal_sector) <- secciones
meses_es    <- c("Ene","Feb","Mar","Abr","May","Jun",
                 "Jul","Ago","Sep","Oct","Nov","Dic")

# ── HELPERS ──────────────────────────────────────────────────
fmt_mill <- function(x) {
  dplyr::case_when(
    x >= 1e12 ~ paste0(round(x / 1e12, 1), " B"),
    x >= 1e9  ~ paste0(round(x / 1e9,  1), " MM"),
    x >= 1e6  ~ paste0(round(x / 1e6,  1), " M"),
    TRUE      ~ scales::comma(x)
  )
}

plot_barras_apiladas <- function(data, x_col, fill_col, y_col,
                                 y_label, paleta, pct_col = NULL) {
  # etiqueta eje x
  data <- data %>%
    mutate(periodo = paste0(ano, "-", sprintf("%02d", mes)))
  
  p <- ggplot(data, aes(
    x    = .data[[x_col]],
    y    = .data[[y_col]],
    fill = .data[[fill_col]]
  )) +
    geom_col(position = "stack", width = 0.85, color = "white", linewidth = 0.2) +
    scale_fill_manual(values = paleta, name = NULL) +
    scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, .05))) +
    labs(x = NULL, y = y_label) +
    theme_minimal(base_size = 12) +
    theme(
      plot.background    = element_rect(fill = "#f7f9fc", color = NA),
      panel.background   = element_rect(fill = "#f7f9fc", color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_text(angle = 45, hjust = 1, size = 9, color = "#555"),
      axis.text.y        = element_text(size = 9, color = "#555"),
      legend.position    = "bottom",
      legend.text        = element_text(size = 9),
      plot.margin        = margin(10, 10, 10, 10)
    )
  
  # Si hay columna de participación, añadir barras 100%
  if (!is.null(pct_col)) {
    p <- p + geom_col(
      aes(y = .data[[pct_col]]),
      position = "fill", alpha = 0, color = NA
    )
  }
  p
}

plot_barras_pct <- function(data, x_col, fill_col, y_col, paleta) {
  ggplot(data, aes(
    x    = .data[[x_col]],
    y    = .data[[y_col]],
    fill = .data[[fill_col]]
  )) +
    geom_col(position = "fill", width = 0.85, color = "white", linewidth = 0.2) +
    scale_fill_manual(values = paleta, name = NULL) +
    scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, .02))) +
    labs(x = NULL, y = "Participación (%)") +
    theme_minimal(base_size = 12) +
    theme(
      plot.background    = element_rect(fill = "#f7f9fc", color = NA),
      panel.background   = element_rect(fill = "#f7f9fc", color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_text(angle = 45, hjust = 1, size = 9, color = "#555"),
      axis.text.y        = element_text(size = 9, color = "#555"),
      legend.position    = "bottom",
      legend.text        = element_text(size = 9),
      plot.margin        = margin(10, 10, 10, 10)
    )
}

# ── UI ───────────────────────────────────────────────────────
ui <- page_navbar(
  title = tags$span(
    style = "font-weight:700; letter-spacing:-0.5px; font-size:1.1rem;",
    "📊 Crédito Tributario — Panel Histórico"
  ),
  theme = bs_theme(
    bootswatch  = "flatly",
    primary     = "#2d6a9f",
    base_font   = font_google("IBM Plex Sans"),
    heading_font = font_google("IBM Plex Sans Condensed")
  ),
  bg = "#1a3a5c",
  fillable = FALSE,
  
  # ── TAB 1: Puestos de Trabajo ────────────────────────────
  nav_panel(
    "Puestos de Trabajo",
    icon = bsicons::bs_icon("person-workspace"),
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        bg = "#f0f4f8",
        tags$h6("Filtros", style = "font-weight:700; color:#1a3a5c; margin-bottom:12px;"),
        selectInput("pt_dim", "Dimensión",
                    choices = c("Tamaño empresa" = "tamano",
                                "Sección CIIU"   = "sector",
                                "Sexo × Nac."    = "sx_nc")),
        selectInput("pt_tipo", "Tipo de gráfico",
                    choices = c("Valores absolutos" = "abs",
                                "Participación %"   = "pct")),
        hr(),
        sliderInput("pt_anos", "Período (años)",
                    min = 2019, max = 2024, value = c(2019, 2024),
                    step = 1, sep = ""),
        checkboxGroupInput("pt_meses", "Meses",
                           choices  = setNames(1:12, meses_es),
                           selected = 1:12,
                           inline   = TRUE)
      ),
      layout_columns(
        col_widths = 12,
        card(
          card_header("Evolución histórica — Puestos de Trabajo"),
          plotOutput("plot_pt", height = "400px")
        ),
        card(
          card_header("Tabla de datos"),
          DTOutput("tbl_pt")
        )
      )
    )
  ),
  
  # ── TAB 2: Empresas ──────────────────────────────────────
  nav_panel(
    "Empresas",
    icon = bsicons::bs_icon("building"),
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        bg = "#f0f4f8",
        tags$h6("Filtros", style = "font-weight:700; color:#1a3a5c; margin-bottom:12px;"),
        selectInput("ee_dim", "Dimensión",
                    choices = c("Tamaño empresa" = "tamano",
                                "Sección CIIU"   = "sector")),
        selectInput("ee_tipo", "Tipo de gráfico",
                    choices = c("Valores absolutos" = "abs",
                                "Participación %"   = "pct")),
        hr(),
        sliderInput("ee_anos", "Período (años)",
                    min = 2019, max = 2024, value = c(2019, 2024),
                    step = 1, sep = ""),
        checkboxGroupInput("ee_meses", "Meses",
                           choices  = setNames(1:12, meses_es),
                           selected = 1:12,
                           inline   = TRUE)
      ),
      layout_columns(
        col_widths = 12,
        card(
          card_header("Evolución histórica — Número de Empresas"),
          plotOutput("plot_ee", height = "400px")
        ),
        card(
          card_header("Tabla de datos"),
          DTOutput("tbl_ee")
        )
      )
    )
  ),
  
  # ── TAB 3: Masa Salarial y Crédito ───────────────────────
  nav_panel(
    "Masa Salarial & Crédito",
    icon = bsicons::bs_icon("currency-dollar"),
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        bg = "#f0f4f8",
        tags$h6("Filtros", style = "font-weight:700; color:#1a3a5c; margin-bottom:12px;"),
        selectInput("ms_indicador", "Indicador",
                    choices = c("Masa Salarial (CLP)" = "cl_clp",
                                "Masa Salarial (USD)" = "cl_usd",
                                "Crédito Tributario (CLP)" = "ct_clp",
                                "Crédito Tributario (USD)" = "ct_usd")),
        selectInput("ms_tipo", "Tipo de gráfico",
                    choices = c("Valores absolutos" = "abs",
                                "Participación %"   = "pct")),
        hr(),
        sliderInput("ms_anos", "Período (años)",
                    min = 2019, max = 2024, value = c(2019, 2024),
                    step = 1, sep = ""),
        checkboxGroupInput("ms_meses", "Meses",
                           choices  = setNames(1:12, meses_es),
                           selected = 1:12,
                           inline   = TRUE)
      ),
      layout_columns(
        col_widths = c(6, 6),
        value_box(
          title = "Total acumulado (último período)",
          value = textOutput("vbox_total"),
          showcase = bsicons::bs_icon("cash-stack"),
          theme  = "primary"
        ),
        value_box(
          title = "Categoría dominante",
          value = textOutput("vbox_top"),
          showcase = bsicons::bs_icon("bar-chart-fill"),
          theme  = "info"
        )
      ),
      layout_columns(
        col_widths = 12,
        card(
          card_header("Evolución histórica — Masa Salarial / Crédito Tributario"),
          plotOutput("plot_ms", height = "400px")
        ),
        card(
          card_header("Tabla de datos"),
          DTOutput("tbl_ms")
        )
      )
    )
  )
)

# ── SERVER ───────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── helpers reactivos comunes ────────────────────────────
  
  filtrar <- function(data, anos_input, meses_input) {
    data %>%
      filter(
        ano >= anos_input[1], ano <= anos_input[2],
        mes %in% as.integer(meses_input)
      ) %>%
      mutate(periodo = paste0(ano, "-", sprintf("%02d", mes)))
  }
  
  # ── TAB 1: Puestos de Trabajo ────────────────────────────
  
  pt_data <- reactive({
    switch(
      input$pt_dim,
      tamano = filtrar(tbl_pt_tamano, input$pt_anos, input$pt_meses) %>%
        rename(categoria = tamano_empresa),
      sector = filtrar(tbl_pt_sector, input$pt_anos, input$pt_meses) %>%
        rename(categoria = seccion_ciiu4cl),
      sx_nc  = filtrar(tbl_pt_sx_nc, input$pt_anos, input$pt_meses) %>%
        mutate(categoria = paste0(sexo, " / ", nacionalidad))
    )
  })
  
  pt_paleta <- reactive({
    switch(input$pt_dim,
           tamano = pal_tamano,
           sector = pal_sector,
           sx_nc  = c("Hombre / Chilena"    = "#2d6a9f",
                      "Mujer / Chilena"     = "#e05c8a",
                      "Hombre / Extranjera" = "#4fb3bf",
                      "Mujer / Extranjera"  = "#f0a500")
    )
  })
  
  output$plot_pt <- renderPlot({
    d <- pt_data()
    if (nrow(d) == 0) return(NULL)
    if (input$pt_tipo == "abs") {
      plot_barras_apiladas(d, "periodo", "categoria", "pt",
                           "Puestos de Trabajo", pt_paleta())
    } else {
      plot_barras_pct(d, "periodo", "categoria", "participacion", pt_paleta())
    }
  }, bg = "#f7f9fc")
  
  output$tbl_pt <- renderDT({
    d <- pt_data() %>%
      select(ano, mes, categoria, pt, participacion) %>%
      mutate(
        participacion = scales::percent(participacion, accuracy = 0.1),
        pt            = scales::comma(pt)
      )
    datatable(d, options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE, class = "compact stripe")
  })
  
  # ── TAB 2: Empresas ──────────────────────────────────────
  
  ee_data <- reactive({
    switch(
      input$ee_dim,
      tamano = filtrar(tbl_ee_tamano, input$ee_anos, input$ee_meses) %>%
        rename(categoria = tamano_empresa),
      sector = filtrar(tbl_ee_sector, input$ee_anos, input$ee_meses) %>%
        rename(categoria = seccion_ciiu4cl)
    )
  })
  
  ee_paleta <- reactive({
    switch(input$ee_dim,
           tamano = pal_tamano,
           sector = pal_sector
    )
  })
  
  output$plot_ee <- renderPlot({
    d <- ee_data()
    if (nrow(d) == 0) return(NULL)
    if (input$ee_tipo == "abs") {
      plot_barras_apiladas(d, "periodo", "categoria", "n_e",
                           "N° de Empresas", ee_paleta())
    } else {
      plot_barras_pct(d, "periodo", "categoria", "participacion", ee_paleta())
    }
  }, bg = "#f7f9fc")
  
  output$tbl_ee <- renderDT({
    d <- ee_data() %>%
      select(ano, mes, categoria, n_e, participacion) %>%
      mutate(
        participacion = scales::percent(participacion, accuracy = 0.1),
        n_e           = scales::comma(n_e)
      )
    datatable(d, options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE, class = "compact stripe")
  })
  
  # ── TAB 3: Masa Salarial & Crédito ───────────────────────
  
  ms_data <- reactive({
    ind <- input$ms_indicador
    tbl <- if (grepl("cl_", ind)) tbl_cl_tamano else tbl_ct_tamano
    col <- if (grepl("clp", ind)) {
      if (grepl("cl_", ind)) "monto_clp" else "ct_clp"
    } else {
      if (grepl("cl_", ind)) "monto_usd" else "ct_usd"
    }
    pct_col <- if (grepl("clp", ind)) "participacion_clp" else "participacion_usd"
    
    filtrar(tbl, input$ms_anos, input$ms_meses) %>%
      rename(
        categoria = tamano_empresa,
        valor     = all_of(col),
        pct       = all_of(pct_col)
      )
  })
  
  output$vbox_total <- renderText({
    d <- ms_data()
    if (nrow(d) == 0) return("—")
    ultimo <- d %>% filter(periodo == max(periodo)) %>% summarise(t = sum(valor))
    fmt_mill(ultimo$t)
  })
  
  output$vbox_top <- renderText({
    d <- ms_data()
    if (nrow(d) == 0) return("—")
    d %>%
      filter(periodo == max(periodo)) %>%
      slice_max(valor, n = 1) %>%
      pull(categoria)
  })
  
  output$plot_ms <- renderPlot({
    d <- ms_data()
    if (nrow(d) == 0) return(NULL)
    y_lab <- switch(input$ms_indicador,
                    cl_clp = "Masa Salarial (CLP)",
                    cl_usd = "Masa Salarial (USD)",
                    ct_clp = "Crédito Tributario (CLP)",
                    ct_usd = "Crédito Tributario (USD)"
    )
    if (input$ms_tipo == "abs") {
      plot_barras_apiladas(d, "periodo", "categoria", "valor", y_lab, pal_tamano)
    } else {
      plot_barras_pct(d, "periodo", "categoria", "pct", pal_tamano)
    }
  }, bg = "#f7f9fc")
  
  output$tbl_ms <- renderDT({
    d <- ms_data() %>%
      select(ano, mes, categoria, valor, pct) %>%
      mutate(
        pct   = scales::percent(pct, accuracy = 0.1),
        valor = scales::comma(valor)
      ) %>%
      rename(participacion = pct)
    datatable(d, options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE, class = "compact stripe")
  })
}

shinyApp(ui, server)
