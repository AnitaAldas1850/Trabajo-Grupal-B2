library(shiny)

library(tidyverse)
library(ggplot2)

# Definir Server
server <- function(input, output) {
  
  # UI Dinámica para Frecuencia
  output$ui_frec <- renderUI({
    if(input$dist_frec == "Poisson") numericInput("lambda", "λ (Media):", 5, 0.1)
    else if(input$dist_frec == "Binomial") tagList(numericInput("n_trials", "n:", 100), numericInput("p_success", "p:", 0.05))
    else tagList(numericInput("r_param", "r:", 5), numericInput("p_nbinom", "p:", 0.3))
  })
  
  # UI Dinámica para Severidad
  output$ui_sev <- renderUI({
    switch(input$dist_sev,
           "Exponencial" = numericInput("rate_exp", "Tasa (λ):", 0.01),
           "Gamma" = tagList(numericInput("shape_gamma", "α (Forma):", 2), numericInput("rate_gamma", "β (Tasa):", 0.01)),
           "Lognormal" = tagList(numericInput("meanlog", "μ (Media log):", 6), numericInput("sdlog", "σ (Desv log):", 1)),
           "Weibull" = tagList(numericInput("shape_weibull", "α (Forma):", 1.5), numericInput("scale_weibull", "λ (Escala):", 1000)))
  })
  
  # SIMULACIÓN UNIFICADA
  resultados <- eventReactive(input$simular, {
    req(input$n_sim)
    n <- input$n_sim
    
    # 1. Simular Frecuencia (N)
    frecuencia <- switch(input$dist_frec,
                         "Poisson" = rpois(n, input$lambda * input$periodo),
                         "Binomial" = rbinom(n, input$n_trials, input$p_success),
                         "Binomial Negativa" = rnbinom(n, input$r_param, input$p_nbinom))
    
    # 2. Simular Severidad (X)
    total_siniestros <- sum(frecuencia)
    if (total_siniestros == 0) return(NULL)
    
    costos_raw <- switch(input$dist_sev,
                         "Exponencial" = rexp(total_siniestros, input$rate_exp),
                         "Gamma" = rgamma(total_siniestros, input$shape_gamma, input$rate_gamma),
                         "Lognormal" = rlnorm(total_siniestros, input$meanlog, input$sdlog),
                         "Weibull" = rweibull(total_siniestros, input$shape_weibull, input$scale_weibull))
    
    # Aplicar Deducible y Límite
    costos_netos <- pmin(pmax(costos_raw - input$deducible, 0), input$limite_max)
    
    # 3. Agregación (S = Sum X)
    perdida_agregada <- numeric(n)
    idx_fin <- cumsum(frecuencia)
    idx_ini <- c(1, idx_fin[-n] + 1)  # Ajuste para evitar problema con índices
    
    for (i in 1:n) {
      if (frecuencia[i] > 0) {
        perdida_agregada[i] <- sum(costos_netos[idx_ini[i]:idx_fin[i]])
      }
    }
    
    # 4. Cálculos Estadísticos
    var_95 <- quantile(perdida_agregada, 0.95)
    tvar_95 <- mean(perdida_agregada[perdida_agregada > var_95])
    
    list(
      frecuencia = frecuencia,
      S = perdida_agregada,
      prima_pura = mean(perdida_agregada),
      var_95 = var_95,
      tvar_95 = tvar_95
    )
  })
   
# OUTPUTS DE RESULTADOS
output$box_pura <- renderValueBox({
  req(resultados())
  valueBox(dollar(resultados()$prima_pura), "Prima Pura Simulada", icon = icon("calculator"), color = "blue")
})

output$box_comercial <- renderValueBox({
  req(resultados())
  p_com <- resultados()$prima_pura * (1 + input$margen/100) * (1 + input$impuestos/100)
  valueBox(dollar(p_com), "Prima Comercial", icon = icon("money-bill-wave"), color = "green")
})

output$box_var <- renderValueBox({
  req(resultados())
  valueBox(dollar(resultados()$var_95), "VaR (95%)", icon = icon("shield-alt"), color = "red")
})

output$tabla_primas <- renderTable({
  req(resultados())
  res <- resultados()
  p_com <- res$prima_pura * (1 + input$margen/100) * (1 + input$impuestos/100)
  data.frame(
    Concepto = c("Media de Siniestros (E[N])", "Prima Pura (E[S])", "Prima Comercial"),
    Valor = c(round(mean(res$frecuencia), 2), dollar(res$prima_pura), dollar(p_com))
  )
})

output$estadisticas <- renderTable({
  req(resultados())
  res <- resultados()
  data.frame(
    Métrica = c("Máximo", "SD", "TVaR (95%)"),
    Valor = c(dollar(max(res$S)), dollar(sd(res$S)), dollar(res$tvar_95))
  )
})
# Histograma de pérdida agregada
output$hist_perdida <- renderPlot({
  req(resultados())
  ggplot(data.frame(S = resultados()$S), aes(x = S)) +
    geom_histogram(fill = "#2c3e50", color = "white", bins = 40) +
    geom_vline(xintercept = resultados()$var_95, color = "red", linetype = "dashed", size = 1) +
    theme_minimal() + labs(x = "Pérdida Agregada ($)", y = "Frecuencia") +
    scale_x_continuous(labels = dollar)
})
# Análisis de Sensibilidad para Margen
output$sensibilidad_margen <- renderPlot({
  req(resultados())
  margenes <- seq(0, 100, 5)
  primas <- resultados()$prima_pura * (1 + margenes/100) * (1 + input$impuestos/100)
  ggplot(data.frame(m = margenes, p = primas), aes(m, p)) + 
    geom_line(color = "#3498db", size = 1.2) + geom_point() +
    theme_minimal() + labs(x = "Margen de Ganancia (%)", y = "Prima Comercial ($)") +
    scale_y_continuous(labels = dollar)
})

# Análisis de Sensibilidad para Impuestos
output$sensibilidad_impuestos <- renderPlot({
  req(resultados())
  impuestos_seq <- seq(0, 0.3, by = 0.03)
  primas <- resultados()$prima_pura * (1 + input$margen/100) * (1 + impuestos_seq)
  ggplot(data.frame(Impuestos = impuestos_seq*100, Prima = primas), 
         aes(x = Impuestos, y = Prima)) +
    geom_line(color = "green", linewidth = 1.5) +
    geom_point(color = "darkgreen", size = 3) +
    labs(title = "Sensibilidad: Prima vs Tasa de Impuestos",
         x = "Tasa de Impuestos (%)", y = "Prima Comercial ($)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
})
# Gráfico Riesgo vs Prima
output$riesgo_prima <- renderPlot({
  req(resultados())
  res <- resultados()
  ggplot(data.frame(S = sort(res$S), id = 1:length(res$S)), aes(id, S)) +
    geom_area(fill = "purple", alpha = 0.3) +
    geom_line(color = "purple") +
    theme_minimal() + labs(x = "Escenario (Ordenado)", y = "Costo ($)") +
    scale_y_continuous(labels = dollar)
})
#------------------
  
  # Histograma de severidad (muestra de costos individuales)
  output$hist_severidad <- renderPlot({
    req(resultados())
    
    # Simular una muestra de costos individuales para visualización
    n_muestra <- 10000
    costos_individuales <- switch(input$dist_sev,
                                  "Exponencial" = rexp(n_muestra, rate = input$rate_exp),
                                  "Gamma" = rgamma(n_muestra, shape = input$shape_gamma, rate = input$rate_gamma),
                                  "Lognormal" = rlnorm(n_muestra, meanlog = input$meanlog, sdlog = input$sdlog),
                                  "Weibull" = rweibull(n_muestra, shape = input$shape_weibull, scale = input$scale_weibull)
    )
    
    ggplot(data.frame(Severidad = costos_individuales), aes(x = Severidad)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "salmon", alpha = 0.7) +
      geom_density(color = "darkred", linewidth = 1) +
      labs(title = "Distribución de Severidad",
           x = "Costo por siniestro ($)", y = "Densidad") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14))
  })
  
 
  # Tabla de parámetros de distribuciones
  #output$parametros_dist <- renderTable({
  #  req(resultados())
  #  
  #  params_frec <- switch(input$dist_frec,
  #                        "Poisson" = c("λ" = input$lambda),
  #                        "Binomial" = c("n" = input$n_trials, "p" = input$p_success),
  #                        "Binomial Negativa" = c("r" = input$r_param, "p" = input$p_nbinom)
  #  )
  #  
  #  params_sev <- switch(input$dist_sev,
  #                       "Exponencial" = c("λ" = input$rate_exp),
  #                       "Gamma" = c("α" = input$shape_gamma, "β" = input$rate_gamma),
  #                       "Lognormal" = c("μ" = input$meanlog, "σ" = input$sdlog),
  #                       "Weibull" = c("α" = input$shape_weibull, "λ" = input$scale_weibull)
  #  )
  #  
  #  data.frame(
  #    Modelo = c("Frecuencia", "Severidad"),
  #    Distribución = c(input$dist_frec, input$dist_sev),
  #    Parámetros = c(paste(names(params_frec), params_frec, collapse = ", "),
  #                   paste(names(params_sev), params_sev, collapse = ", ")),
  #    E[·] = c(sprintf("%.2f", resultados()$E_N), 
  #             sprintf("%.2f", resultados()$E_X))
  #  )
  #}, striped = TRUE)
 #
  # Descargar reporte
  output$reporte <- downloadHandler(
    filename = function() {
      paste("reporte_prima_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
  # Aquí iría el código para generar un PDF con los resultados
   #Por ahora, solo creamos un archivo temporal
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
  
  # Parámetros para pasar al reporte
      params <- list(resultados = resultados(),
                     input_params = reactiveValuesToList(input))
  
  # Renderizar el reporte
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}

