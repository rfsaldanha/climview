# Packages
library(shiny)
library(bslib)
library(dplyr)
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
        accordion(
          multiple = FALSE,
          accordion_panel(
            "Temperatura máxima",
            checkboxInput(
              inputId = "tmax_obs_sel",
              label = "Temperatura máxima observada",
              value = TRUE
            ),
            selectizeInput(
              inputId = "tmax_indi_sel",
              label = "Indicadores mensais",
              choices = c("Média", "Mediana", "Desvio padrão", "Percentil 10", "Percentil 25", "Percentil 75", "Percentil 90", "Onda de calor 3 dias", "Onda de calor 5 dias", "Dias quentes", "Dias acima de 25 graus", "Dias acima de 30 graus", "Dias acima de 35 graus", "Dias acima de 40 graus"), 
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
  # Main graph
  output$main_graph <- renderVchart({
    co2_emissions %>% 
  filter(country %in% c("China", "United States", "India")) %>% 
  vchart() %>% 
  v_line(
    aes(year, co2, color = country)
  ) %>% 
  v_specs_datazoom(
    start = "{label:.0f}",
    startValue = 2000, 
    end = "{label:.0f}"
  ) %>% 
  v_specs_legend(
    orient = "top",
    position = "start",
    layout = "vertical",
    layoutType = "absolute",
    left = 30,
    top = 20,
    item = list(
      shape = list(
        style = list(
          symbolType = "roundLine"
        )
      )
    )
  )
  })
  

}

shinyApp(ui, server)