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

# Interface
ui <- page_navbar(
  title = "Indicadores climatológicos para saúde", 
  theme = bs_theme(bootswatch = "shiny"),

  # Logo
  tags$head(
    tags$script(
      HTML('$(document).ready(function() {
             $(".navbar .container-fluid")
               .append("<img id = \'myImage\' src=\'selo_obs_h.png\' align=\'right\' height = \'57.5px\'>"  );
            });')),
    tags$style(
      HTML('@media (max-width:992px) { #myImage { position: fixed; right: 10%; top: 0.5%; }}')
    )),

  # Translation
  tags$script(
    HTML("
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
    ")
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
              value = TRUE
            ),
            selectizeInput(
              inputId = "tmax_indi_sel",
              label = "Indicadores mensais",
              choices = c("Média" = "mean", "Mediana" = "median", "Desvio padrão" = "sd", "Percentil 10" = "p10", "Percentil 25" = "p25", "Percentil 75" = "p75", "Percentil 90" = "p90", "Onda de calor 3 dias" = "heat_waves_3d", "Onda de calor 5 dias" = "heat_waves_5d", "Dias quentes" = "hot_days", "Dias acima de 25 graus" = "t_25", "Dias acima de 30 graus" = "t_30", "Dias acima de 35 graus" = "t_35", "Dias acima de 40 graus" = "t_40"), 
              multiple = TRUE
            ),
            selectizeInput(
              inputId = "tmax_normal_sel",
              label = "Normal 1961-1990",
              choices = c("Média", "Percentil 10", "Percentil 90"), 
              multiple = TRUE
            )
          ),
          accordion_panel(
            "Temperatura mínima"
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
          vchartOutput(outputId = "main_graph", height = "auto")
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
    server = FALSE,
    choices = c("3304557", "3303401", "1502103")
  )

  # Observe indicator selection
  graph_data <- reactive({
    req(input$mun_sel)
    #req(input$tmax_obs_sel)

    print(input$tmax_obs_sel)

    # tmax obs
    if(input$tmax_obs_sel == TRUE){
      tmp1 <- tmax |>
        filter(name == "Tmax_3.2.3_mean") |>
        filter(code_muni == input$mun_sel) |>
        select(-code_muni, name) |>
        collect() |>
        mutate(
          name = "Temperatura máxima"
        )
    } else if(input$tmax_obs_sel == FALSE) {
      tmp1 <- tibble()
    }

    # tmax indi
    if(length(input$tmax_indi_sel) > 0){
      tmp2 <- tmax_indi |>
        filter(code_muni == input$mun_sel) |>
        select(year, month, all_of(input$tmax_indi_sel)) |>
        collect() |>
        mutate(date = as.Date(paste0(year,"-",month,"-1"))) |>
        select(-year, -month) |>
        relocate(date) |>
        pivot_longer(cols = input$tmax_indi_sel)
    } else {
      tmp2 <- tibble()
    }

    bind_rows(tmp1, tmp2)
  })

  # Main graph (test)
  output$main_graph <- renderVchart({
    # Fetch data
    res <- graph_data()

    # Check size
    if(nrow(res) > 0){
      # Plot
      vchart(data = res) |>
        v_line(
          aes(x = date, y = value, color = name), 
          line = list(style = list(opacity = 0.5))
        ) |>
          v_specs_datazoom()
    }
    
  })
  

}

shinyApp(ui, server)