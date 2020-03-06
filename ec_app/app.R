# Paquetes 
library(shiny)
library(gEcon)
library(gEcon.estimation)
library(shinythemes)

# Función para manejar objetos gecon
source("plot_gecon_simulation.R")

# Interface de usuario
ui <- fluidPage(
    theme = "slate",

    # Titulo
    titlePanel("Modelo semiestructural"),

    # iputs
    sidebarLayout(
        sidebarPanel(
            h2("Simulación"),
            wellPanel(
            radioButtons(
                "tipo_economia",
                "Tipo de economía",
                choices = c("Cerrada" = "ec", "Abierta" = "ea")),
            uiOutput("variable"),
            
            radioButtons("facets", "¿Facets?", choices = c("Sí" = TRUE, "No" = FALSE))
            ),
            h2("Forecast"),
            wellPanel(
                selectInput(
                    "variable_fc",
                    label = "Variable",
                    choices = c("Inflación" = "pic", "Demanda agregada" = "ygap", "TPM" = "tpm"),
                    selected = "ygap"
                )  
            )
        ),

        # Gráfico de la impulso respuesta
        mainPanel(
            h2("Impulso respuesta"),
           plotOutput("impulso_respuesta"),
           h2("Forecast"),
           plotOutput("forecast_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$variable <- renderUI({
        selectInput(
            "variable",
            label = "Variable de choque",
            choices = choices_shocks_widget[[input$tipo_economia]],
            selected = "eta_tpm"
        )
    })    
    
   fir <- reactive({
       
       if(input$tipo_economia == "ea"){
           model <- ea
       }else {
           model <- ec
       }
       
        compute_irf(model, 
                    variables = c("ygap", "pic", "tpm"),
                    sim_length = 12,
                    shock = input$variable)
    })

    output$impulso_respuesta <- renderPlot({
       ggplot_gecon_simulation(fir(), facets = input$facets)
        })
    
    output$forecast_plot <- renderPlot({
        tidy_forecast_ec %>%
            filter(variable %in% input$variable_fc) %>%
            ggplot(aes(x = h, y = mean, linetype = key)) +
            geom_ribbon(aes(ymin = low95, ymax = up95), 
                        fill = "#D5DBFF", color = NA, size = 0) +
            geom_ribbon(aes(ymin = low80, ymax = up80), 
                        fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
            geom_line(size = 1) +
            theme_minimal() +
            theme(legend.position = "bottom") +
            labs(
                x = "",
                y = input$variable_fc,
                color = ""
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
