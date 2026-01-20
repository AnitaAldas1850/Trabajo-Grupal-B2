library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(title = "Simulación de Primas de Seguro"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Parámetros", tabName = "parametros", icon = icon("sliders-h")),
      menuItem("Resultados", tabName = "resultados", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # -------------------------------
      # TAB 1: PARÁMETROS
      # -------------------------------
      tabItem(
        tabName = "parametros",
        
        fluidRow(
          
          box(
            title = "Parámetros financieros",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            
            numericInput(
              inputId = "margen",
              label = "Margen de ganancia (%)",
              value = 20,
              min = 0,
              max = 100,
              step = 1
            ),
            
            numericInput(
              inputId = "impuestos",
              label = "Tasa de impuestos (%)",
              value = 12,
              min = 0,
              max = 100,
              step = 1
            ),
            
            numericInput(
              inputId = "nsim",
              label = "Número de simulaciones",
              value = 10000,
              min = 1000,
              step = 1000
            )
          ),

          # Columna central: Modelo de Frecuencia
          box(
            title = "Modelo de Frecuencia (Número de Siniestros)",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            
            selectInput("dist_frec", "Distribución:",
                        choices = c("Poisson", "Binomial", "Binomial Negativa")),
            
            conditionalPanel(
              condition = "input.dist_frec == 'Poisson'",
              numericInput("lambda", "Parámetro λ:", 
                          value = 5, min = 0.1, step = 0.1)
            ),
            
            conditionalPanel(
              condition = "input.dist_frec == 'Binomial'",
              numericInput("n_trials", "Número de ensayos (n):", 
                          value = 100, min = 1),
              numericInput("p_success", "Probabilidad éxito (p):", 
                          value = 0.05, min = 0, max = 1, step = 0.01)
            ),
            
            conditionalPanel(
              condition = "input.dist_frec == 'Binomial Negativa'",
              numericInput("r_param", "Parámetro r (éxitos):", 
                          value = 5, min = 1),
              numericInput("p_nbinom", "Probabilidad éxito (p):", 
                          value = 0.3, min = 0, max = 1, step = 0.01)
            )
          ),
          # Columna derecha: Modelo de Severidad
          box(
            title = "Modelo de Severidad (Costo por Siniestro)",
            status = "success",
            solidHeader = TRUE,
            width = 4,
            
            selectInput("dist_sev", "Distribución:",
                        choices = c("Exponencial", "Gamma", "Lognormal", "Weibull")),
            
            conditionalPanel(
              condition = "input.dist_sev == 'Exponencial'",
              numericInput("rate_exp", "Tasa (λ):", 
                          value = 0.01, min = 0.001, step = 0.001)
            ),
            
            conditionalPanel(
              condition = "input.dist_sev == 'Gamma'",
              numericInput("shape_gamma", "Forma (α):", 
                          value = 2, min = 0.1, step = 0.1),
              numericInput("rate_gamma", "Tasa (β):", 
                          value = 0.01, min = 0.001, step = 0.001)
            ),
            
            conditionalPanel(
              condition = "input.dist_sev == 'Lognormal'",
              numericInput("meanlog", "Media logarítmica (μ):", 
                          value = 6, step = 0.1),
              numericInput("sdlog", "Desviación logarítmica (σ):", 
                          value = 1, min = 0.1, step = 0.1)
            ),
            
            conditionalPanel(
              condition = "input.dist_sev == 'Weibull'",
              numericInput("shape_weibull", "Forma (α):", 
                          value = 1.5, min = 0.1, step = 0.1),
              numericInput("scale_weibull", "Escala (λ):", 
                          value = 1000, min = 1, step = 10)
            )
          )
          
        )
      ),
      
      # -------------------------------
      # TAB 2: RESULTADOS
      # -------------------------------
      tabItem(
        tabName = "resultados",
        
        fluidRow(
          
          valueBox(
            value = textOutput("EN"),
            subtitle = "Esperanza del número de siniestros",
            icon = icon("exclamation-circle"),
            color = "aqua",
            width = 4
          ),
          
          valueBox(
            value = textOutput("EX"),
            subtitle = "Esperanza del costo individual",
            icon = icon("dollar-sign"),
            color = "green",
            width = 4
          ),
          
          valueBox(
            value = textOutput("ES"),
            subtitle = "Costo total esperado",
            icon = icon("calculator"),
            color = "yellow",
            width = 4
          )
          
        )
      )
      
    )
  )
)


