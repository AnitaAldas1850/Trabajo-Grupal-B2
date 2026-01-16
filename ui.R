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

