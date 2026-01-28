library(shiny)
library(scales)
library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(shinyWidgets)

# Definir UI
ui <- fluidPage(
  
  # --- Encabezado de Títulos ---
  div(class = "main-title", "Sistema de Cálculo de Prima de Seguros"),
  div(class = "authors-subtitle", "Elaborado por: Aldás A., Iniquinga M., Torres E."),
  
  dashboardHeader(),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Configuración", tabName = "config", icon = icon("sliders-h")),
      menuItem("Resultados", tabName = "resultados", icon = icon("chart-bar")),
      menuItem("Análisis Sensibilidad", tabName = "sensibilidad", icon = icon("search-dollar")),
      menuItem("Detalles Técnicos", tabName = "teoria", icon = icon("book"))
    ),
    hr(),
    div(style = "padding: 20px;",
        actionButton("simular", "EJECUTAR SIMULACIÓN", 
                     icon = icon("play"), 
                     style = "width: 100%; background-color: #27ae60; color: white; font-weight: bold; border: none;"),
        br(), br(),
        downloadButton("reporte", "Descargar Reporte", style = "width: 100%;")
    )
  ),
  
  dashboardBody(
    # --- Estilo CSS Personalizado ---
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;800&display=swap');
        
        body, .main-sidebar, .left-side { font-family: 'Inter', sans-serif; background-color: #f4f7f9; }
        
        /* Título Principal Estilizado */
        .main-title { 
          text-align: center; 
          color: #1e2d40; 
          font-weight: 800; 
          font-size: 38px; 
          margin-top: 10px; 
          margin-bottom: 5px;
          letter-spacing: -1px;
        }
        
        /* Subtítulo / Autores */
        .authors-subtitle { 
          text-align: left; 
          color: #7f8c8d; 
          font-weight: 400; 
          font-size: 14px; 
          margin-bottom: 25px; 
          padding-left: 15px;
          border-left: 3px solid #3498db;
        }

        .main-header .logo { background-color: #1e2d40 !important; font-weight: 600; }
        .main-header .navbar { background-color: #2c3e50 !important; }
        .box { border-radius: 8px; border-top: 4px solid #3498db; box-shadow: 0 4px 6px rgba(0,0,0,0.05); }
        .box-header { color: #2c3e50 !important; font-weight: 600; }
      "))
    ),
    
  
    tabItems(
      # TAB 1: CONFIGURACIÓN
      tabItem(tabName = "config",
              fluidRow(
                box(title = "Simulación y Tiempo", width = 4, status = "primary",
                    numericInput("n_sim", "Número de simulaciones:", 10000, 1000, 100000),
                    numericInput("periodo", "Período (años):", 1, 0.1, 10, 0.1)
                ),
                box(title = "Modelo de Frecuencia", width = 4, status = "primary",
                    selectInput("dist_frec", "Distribución:", 
                                choices = c("Poisson", "Binomial", "Binomial Negativa")),
                    uiOutput("ui_frec")
                ),
                box(title = "Modelo de Severidad", width = 4, status = "primary",
                    selectInput("dist_sev", "Distribución:", 
                                choices = c("Exponencial", "Gamma", "Lognormal", "Weibull")),
                    uiOutput("ui_sev")
                )
              ),
              fluidRow(
                box(title = "Modificadores de Riesgo", width = 6,
                    numericInput("deducible", "Deducible por siniestro ($):", 0, 0),
                    numericInput("limite_max", "Límite de indemnización ($):", 50000, 1000)
                ),
                box(title = "Parámetros Comerciales", width = 6,
                    numericInput("margen", "Margen de ganancia (%):", 20, 0, 100),
                    numericInput("impuestos", "Tasa de impuestos (%):", 15, 0, 100)
                )
              )
      ),
      
      # TAB 2: RESULTADOS
      tabItem(tabName = "resultados",
              fluidRow(
                valueBoxOutput("box_pura", width = 4),
                valueBoxOutput("box_comercial", width = 4),
                valueBoxOutput("box_siniestros", width = 4)
              ),
              fluidRow(
                box(title = "Distribución de Pérdida Agregada (S)", width = 8, 
                    plotOutput("hist_perdida", height = "400px")),
                box(title = "Estadísticas Descriptivas", width = 4, 
                    tableOutput("estadisticas"))
              ),
              fluidRow(
                box(title = "Frecuencia de Siniestros", width = 6, plotOutput("hist_frecuencia", height = "250px")),
                box(title = "Severidad de Siniestros", width = 6, plotOutput("hist_severidad", height = "250px"))
              )
      ),
      
      # TAB 3: SENSIBILIDAD
      tabItem(tabName = "sensibilidad",
              fluidRow(
                box(title = "Impacto del Margen en Prima Comercial", width = 12, 
                    plotOutput("sensibilidad_margen", height = "400px"))
              )
      ),
      
      # TAB 4: TEORÍA
      tabItem(tabName = "teoria",
              box(title = "Modelo Colectivo de Riesgo", width = 12,
                  withMathJax(),
                  p("La pérdida agregada se modela mediante la suma aleatoria:"),
                  helpText("$$S = \\sum_{i=1}^{N} X_i$$"),
                  p("Donde cada siniestro individual $X_i$ se ajusta por el deducible $(d)$ y el límite $(L)$:"),
                  helpText("$$X_{adj} = \\min(\\max(X - d, 0), L)$$"),
                  hr(),
                  h4("Cálculo de Primas"),
                  helpText("$$\\Pi_{comercial} = E[S] \\times (1 + m) \\times (1 + t)$$")
              )
      )
    )
  )
)
