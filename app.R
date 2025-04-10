# Packages
library(shiny)
library(bslib)
library(dplyr)
library(tibble)
library(tidyr)
library(duckdb)
library(DBI)
library(vchartr)

# Database connection
con <- dbConnect(duckdb(), "climindi.duckdb", read_only = TRUE)

# Tables definitions
eto <- tbl(con, "eto")
eto_indi <- tbl(con, "eto_indi")
eto_normal <- tbl(con, "eto_normal")
pr <- tbl(con, "pr")
pr_indi <- tbl(con, "pr_indi")
pr_normal <- tbl(con, "pr_normal")
rh <- tbl(con, "rh")
rh_indi <- tbl(con, "rh_indi")
rh_normal <- tbl(con, "rh_normal")
rs <- tbl(con, "rs")
rs_indi <- tbl(con, "rs_indi")
rs_normal <- tbl(con, "rs_normal")
tmax <- tbl(con, "tmax")
tmax_indi <- tbl(con, "tmax_indi")
tmax_normal <- tbl(con, "tmax_normal")
tmin <- tbl(con, "tmin")
tmin_indi <- tbl(con, "tmin_indi")
tmin_normal <- tbl(con, "tmin_normal")
u2 <- tbl(con, "u2")
u2_indi <- tbl(con, "u2_indi")
u2_normal <- tbl(con, "u2_normal")

# Municipalities names
mun_names <- readRDS("data/mun_names.rds")
mun_list <- mun_names$code_muni
names(mun_list) <- paste(mun_names$name_muni, "-", mun_names$abbrev_state)
rm(mun_names)

# Skelethon tibble
tibble_sk <- tibble(
  date = as.Date(NA),
  value = numeric(),
  name = character()
)

# Interface
ui <- page_navbar(
  title = "Indicadores climatológicos municipais",
  theme = bs_theme(bootswatch = "shiny"),
  fillable = TRUE,

  # Logo
  tags$head(
    tags$script(
      HTML(
        '$(document).ready(function() {
             $(".navbar .container-fluid")
               .append("<img id = \'myImage\' src=\'selo_obs_h.png\' align=\'right\' height = \'57.5px\'>"  );
            });'
      )
    ),
    tags$style(
      HTML(
        '@media (max-width:992px) { #myImage { position: fixed; right: 10%; top: 0.5%; }}'
      )
    )
  ),

  # Translation
  tags$script(
    HTML(
      "
      $(document).ready(function() {
        // Change the text 'Expand' in all tooltips
        $('.card.bslib-card bslib-tooltip > div').each(function() {
          if ($(this).text().includes('Expand')) {
            $(this).text('Expandir');
          }
        });
  
        // Use MutationObserver to change the text 'Close'
        var observer = new MutationObserver(function(mutations) {
          $('.bslib-full-screen-exit').each(function() {
            if ($(this).html().includes('Close')) {
              $(this).html($(this).html().replace('Close', 'Fechar'));
            }
          });
        });
  
        // Observe all elements with the class 'card bslib-card'
        $('.card.bslib-card').each(function() {
          observer.observe(this, { 
            attributes: true, 
            attributeFilter: ['data-full-screen'] 
          });
        });
      });
    "
    )
  ),

  # Map page
  nav_panel(
    title = "Visualização",

    # Sidebar
    layout_sidebar(
      sidebar = sidebar(
        open = "always",
        selectizeInput(
          inputId = "mun_sel",
          label = "Município",
          choices = NULL
        ),
        accordion(
          multiple = FALSE,
          accordion_panel(
            "Temperatura máxima",
            checkboxInput(
              inputId = "tmax_obs_sel",
              label = "Temperatura máxima diária",
              value = FALSE
            ),
            selectizeInput(
              inputId = "tmax_indi_sel_uni",
              label = "Indicadores mensais",
              choices = c(
                "Média" = "mean",
                "Mediana" = "median",
                "Desvio padrão" = "sd",
                "Percentil 10" = "p10",
                "Percentil 25" = "p25",
                "Percentil 75" = "p75",
                "Percentil 90" = "p90"
              ),
              multiple = TRUE,
              selected = "p90"
            ),
            selectizeInput(
              inputId = "tmax_normal_sel",
              label = "Normal 1961-1990",
              choices = c(
                "Média" = "normal_mean",
                "Percentil 10" = "normal_p10",
                "Percentil 90" = "normal_p90"
              ),
              multiple = TRUE
            ),
            selectizeInput(
              inputId = "tmax_indi_sel_count",
              label = "Indicadores mensais (contagem)",
              choices = c(
                "Onda de calor 3 dias" = "heat_waves_3d",
                "Onda de calor 5 dias" = "heat_waves_5d",
                "Dias quentes" = "hot_days",
                "Dias acima de 25 graus" = "t_25",
                "Dias acima de 30 graus" = "t_30",
                "Dias acima de 35 graus" = "t_35",
                "Dias acima de 40 graus" = "t_40"
              ),
              multiple = TRUE,
              selected = "hot_days"
            )
          ),
          accordion_panel(
            "Temperatura mínima",
            checkboxInput(
              inputId = "tmin_obs_sel",
              label = "Temperatura mínima diária",
              value = FALSE
            ),
            selectizeInput(
              inputId = "tmin_indi_sel_uni",
              label = "Indicadores mensais",
              choices = c(
                "Média" = "mean",
                "Mediana" = "median",
                "Desvio padrão" = "sd",
                "Percentil 10" = "p10",
                "Percentil 25" = "p25",
                "Percentil 75" = "p75",
                "Percentil 90" = "p90"
              ),
              multiple = TRUE,
              selected = "p10"
            ),
            selectizeInput(
              inputId = "tmin_normal_sel",
              label = "Normal 1961-1990",
              choices = c(
                "Média" = "normal_mean",
                "Percentil 10" = "normal_p10",
                "Percentil 90" = "normal_p90"
              ),
              multiple = TRUE
            ),
            selectizeInput(
              inputId = "tmin_indi_sel_count",
              label = "Indicadores mensais (contagem)",
              choices = c(
                "Onda de frio 3 dias" = "cold_spells_3d",
                "Onda de frio 5 dias" = "cold_spells_5d",
                "Dias frios" = "cold_days",
                "Dias abaixo de 0 graus" = "t_0",
                "Dias abaixo de 5 graus" = "t_5",
                "Dias abaixo de 10 graus" = "t_10",
                "Dias abaixo de 15 graus" = "t_15",
                "Dias abaixo de 20 graus" = "t_20"
              ),
              multiple = TRUE,
              selected = "cold_days"
            )
          ),
          accordion_panel(
            "Precipitação"
          ),
          accordion_panel(
            "Umidade relativa"
          ),
          accordion_panel(
            "Radiação solar"
          ),
          accordion_panel(
            "Velocidade do vento"
          ),
          accordion_panel(
            "Evapotranspiração"
          )
        )
      ),

      # Visualization
      card(
        card_body(
          class = "p-0", # Fill card, used for maps,
          vchartOutput(outputId = "graph", height = "auto")
        )
      )
    )
  ),

  # About page
  nav_panel(
    title = "Métodos e indicadores",
    card(
      card_header("Card title"),
      p("Bla bla bla.")
    ),
    accordion(
      multiple = FALSE,
      accordion_panel(
        "Título A",
        p("Bla bla bla.")
      ),
      accordion_panel(
        "Título B",
        p("Bla bla bla.")
      ),
      accordion_panel(
        "Título C",
        p("Bla bla bla.")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Fill municipality selector
  updateSelectizeInput(
    session = session,
    inputId = "mun_sel",
    server = TRUE,
    choices = mun_list
  )

  # Observe indicator selection
  graph_data_uni <- reactive({
    req(input$mun_sel)

    # tmax obs
    if (input$tmax_obs_sel == TRUE) {
      tmp1 <- tmax |>
        filter(name == "Tmax_3.2.3_mean") |>
        filter(code_muni == input$mun_sel) |>
        select(-code_muni, name) |>
        collect() |>
        mutate(
          name = "Temperatura máxima"
        )
    } else if (input$tmax_obs_sel == FALSE) {
      tmp1 <- tibble_sk
    }

    # tmax indi
    if (length(input$tmax_indi_sel_uni) > 0) {
      tmp2 <- tmax_indi |>
        filter(code_muni == input$mun_sel) |>
        select(year, month, all_of(input$tmax_indi_sel_uni)) |>
        rename_with(~ paste0("tmax_", .), all_of(input$tmax_indi_sel_uni)) |>
        collect() |>
        mutate(date = as.Date(paste0(year, "-", month, "-1"))) |>
        select(-year, -month) |>
        relocate(date) |>
        pivot_longer(cols = starts_with("tmax_"))
    } else {
      tmp2 <- tibble_sk
    }

    # tmax normal
    if (length(input$tmax_normal_sel) > 0) {
      tmp3 <- tmax_indi |>
        filter(code_muni == input$mun_sel) |>
        select(year, month, all_of(input$tmax_normal_sel)) |>
        rename_with(~ paste0("tmax_", .), all_of(input$tmax_normal_sel)) |>
        collect() |>
        mutate(date = as.Date(paste0(year, "-", month, "-1"))) |>
        select(-year, -month) |>
        relocate(date) |>
        pivot_longer(cols = starts_with("tmax_"))
    } else {
      tmp3 <- tibble_sk
    }

    # tmin obs
    if (input$tmin_obs_sel == TRUE) {
      tmp4 <- tmin |>
        filter(name == "Tmin_3.2.3_mean") |>
        filter(code_muni == input$mun_sel) |>
        select(-code_muni, name) |>
        collect() |>
        mutate(
          name = "Temperatura mínima"
        )
    } else if (input$tmin_obs_sel == FALSE) {
      tmp4 <- tibble_sk
    }

    # tmin indi
    if (length(input$tmin_indi_sel_uni) > 0) {
      tmp5 <- tmin_indi |>
        filter(code_muni == input$mun_sel) |>
        select(year, month, all_of(input$tmin_indi_sel_uni)) |>
        rename_with(~ paste0("tmin_", .), all_of(input$tmin_indi_sel_uni)) |>
        collect() |>
        mutate(date = as.Date(paste0(year, "-", month, "-1"))) |>
        select(-year, -month) |>
        relocate(date) |>
        pivot_longer(cols = starts_with("tmin_"))
    } else {
      tmp5 <- tibble_sk
    }

    # tmin normal
    if (length(input$tmin_normal_sel) > 0) {
      tmp6 <- tmax_indi |>
        filter(code_muni == input$mun_sel) |>
        select(year, month, all_of(input$tmin_normal_sel)) |>
        rename_with(~ paste0("tmin_", .), all_of(input$tmin_normal_sel)) |>
        collect() |>
        mutate(date = as.Date(paste0(year, "-", month, "-1"))) |>
        select(-year, -month) |>
        relocate(date) |>
        pivot_longer(cols = starts_with("tmin_"))
    } else {
      tmp6 <- tibble_sk
    }

    bind_rows(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)
  })

  graph_data_count <- reactive({
    req(input$mun_sel)

    # tmax indi
    if (length(input$tmax_indi_sel_count) > 0) {
      tmp1 <- tmax_indi |>
        filter(code_muni == input$mun_sel) |>
        select(year, month, all_of(input$tmax_indi_sel_count)) |>
        rename_with(~ paste0("tmax_", .), all_of(input$tmax_indi_sel_count)) |>
        collect() |>
        mutate(date = as.Date(paste0(year, "-", month, "-1"))) |>
        select(-year, -month) |>
        relocate(date) |>
        pivot_longer(cols = starts_with("tmax_"))
    } else {
      tmp1 <- tibble_sk
    }

    # tmin indi
    if (length(input$tmin_indi_sel_count) > 0) {
      tmp2 <- tmin_indi |>
        filter(code_muni == input$mun_sel) |>
        select(year, month, all_of(input$tmin_indi_sel_count)) |>
        rename_with(~ paste0("tmin_", .), all_of(input$tmin_indi_sel_count)) |>
        collect() |>
        mutate(date = as.Date(paste0(year, "-", month, "-1"))) |>
        select(-year, -month) |>
        relocate(date) |>
        pivot_longer(cols = starts_with("tmin_"))
    } else {
      tmp2 <- tibble_sk
    }

    bind_rows(tmp1, tmp2)
  })

  # Graph
  output$graph <- renderVchart({
    # Fetch data
    res_uni <- graph_data_uni()
    res_count <- graph_data_count()

    # Check size
    if (nrow(res_uni) > 0 | nrow(res_count) > 0) {
      # Plot
      vchart() |>
        v_line(
          data = res_uni,
          aes(x = date, y = value, color = name),
          serie_id = "uni",
          name = "uni",
          line = list(style = list(opacity = 0.5))
        ) |>
        v_line(
          data = res_count,
          aes(x = date, y = value, color = name),
          serie_id = "count",
          name = "count",
          line = list(style = list(opacity = 0.5))
        ) |>
        v_scale_y_continuous(
          seriesId = "uni",
          name = "",
          position = "left"
        ) |>
        v_scale_y_continuous(
          seriesId = "count",
          name = "",
          position = "right"
        ) |>
        v_specs_datazoom()
    }
  })
}

shinyApp(ui, server)
