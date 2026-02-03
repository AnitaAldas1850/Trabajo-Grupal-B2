library(shiny)
library(tidyverse)
library(ggplot2)
library(scales)
library(rmarkdown) # ¡Añade esta!
library(shinyWidgets) #

# Definir Server

server <- function(input, output, session) {
  
  # 1. UI Dinámica para Frecuencia
  output$ui_frec <- renderUI({
    if(input$dist_frec == "Poisson") {
      numericInput("lambda", "λ (Media):", 5, 0.1)
    } else if(input$dist_frec == "Binomial") {
      tagList(numericInput("n_trials", "n:", 100), numericInput("p_success", "p:", 0.05))
    } else {
      tagList(numericInput("r_param", "r:", 5), numericInput("p_nbinom", "p:", 0.3))
    }
  })
  
  
  
  # 2. UI Dinámica para Severidad
  output$ui_sev <- renderUI({
    switch(input$dist_sev,
           "Exponencial" = numericInput("rate_exp", "Tasa (λ):", 0.01),
           "Gamma" = tagList(numericInput("shape_gamma", "α (Forma):", 2), numericInput("rate_gamma", "β (Tasa):", 0.01)),
           "Lognormal" = tagList(numericInput("meanlog", "μ (Media log):", 6), numericInput("sdlog", "σ (Desv log):", 1)),
           "Weibull" = tagList(numericInput("shape_weibull", "α (Forma):", 1.5), numericInput("scale_weibull", "λ (Escala):", 1000)))
  })
  
  # 3. SIMULACIÓN UNIFICADA
  resultados <- eventReactive(input$simular, {
    req(input$n_sim, input$periodo)
    n <- input$n_sim
    
    # Simular Frecuencia
    frecuencia <- switch(input$dist_frec,
                         "Poisson" = rpois(n, input$lambda * input$periodo),
                         "Binomial" = rbinom(n, input$n_trials, input$p_success),
                         "Binomial Negativa" = rnbinom(n, input$r_param, input$p_nbinom))
    
    total_siniestros <- sum(frecuencia)
    
    if (is.na(total_siniestros) || total_siniestros == 0) {
      return(list(frecuencia = rep(0, n), costos_raw = 0, S = rep(0, n), prima_pura = 0, var_95 = 0))
    }
    
    # Simular Severidad
    costos_raw <- switch(input$dist_sev,
                         "Exponencial" = rexp(total_siniestros, input$rate_exp),
                         "Gamma" = rgamma(total_siniestros, input$shape_gamma, input$rate_gamma),
                         "Lognormal" = rlnorm(total_siniestros, input$meanlog, input$sdlog),
                         "Weibull" = rweibull(total_siniestros, input$shape_weibull, input$scale_weibull))
    
    costos_netos <- pmin(pmax(costos_raw - input$deducible, 0), input$limite_max)
    
    # Agregación
    perdida_agregada <- numeric(n)
    if(total_siniestros > 0){
      idx_fin <- cumsum(frecuencia)
      idx_ini <- c(1, idx_fin[-n] + 1)
      for (i in 1:n) {
        if (frecuencia[i] > 0) perdida_agregada[i] <- sum(costos_netos[idx_ini[i]:idx_fin[i]])
      }
    }
    
    v_95 <- as.numeric(quantile(perdida_agregada, 0.95))
    
    list(
      frecuencia = frecuencia,
      costos_raw = costos_raw,
      S = perdida_agregada,
      E_N = mean(frecuencia),
      E_X = mean(costos_netos),
      prima_pura = mean(perdida_agregada),
      var_95 = v_95,
      tvar_95 = mean(perdida_agregada[perdida_agregada >= v_95]), # Agregado para evitar error en tabla
      ci_low = as.numeric(quantile(perdida_agregada, 0.025)),
      ci_high = as.numeric(quantile(perdida_agregada, 0.975))
    )
  })
  
  observeEvent(input$simular, {
    # Forzamos el cálculo de resultados y mostramos el mensaje
   resultados() 
    sendSweetAlert(
      session = session,
      title = "¡Simulación Completada!",
      text = "Los datos se han generado correctamente. Ya puedes ver los gráficos en Resultados.",
      type = "success"
    )
  })
  
  
  # 4. OUTPUTS DE RESULTADOS
  output$box_en <- renderValueBox({ req(resultados()); valueBox(round(resultados()$E_N, 2), "E[N]", icon = icon("list-ol"), color = "purple") })
  output$box_ex <- renderValueBox({ req(resultados()); valueBox(dollar(resultados()$E_X), "E[X] Neto", icon = icon("user-tie"), color = "orange") })
  output$box_es <- renderValueBox({ req(resultados()); valueBox(dollar(resultados()$prima_pura), "E[S]", icon = icon("file-invoice-dollar"), color = "teal") })
  output$box_pura <- renderValueBox({ req(resultados()); valueBox(dollar(resultados()$prima_pura), "Prima Pura", icon = icon("calculator"), color = "blue") })
  
  output$box_comercial <- renderValueBox({ 
    req(resultados())
    p_com <- resultados()$prima_pura * (1 + input$margen/100) * (1 + input$impuestos/100)
    valueBox(dollar(p_com), "Prima Comercial", icon = icon("money-bill-wave"), color = "green") 
  })
  
  output$box_var <- renderValueBox({ req(resultados()); valueBox(dollar(resultados()$var_95), "VaR (95%)", icon = icon("shield-alt"), color = "red") })
  
  output$tabla_primas <- renderTable({
    req(resultados())
    res <- resultados()
    p_com <- res$prima_pura * (1 + input$margen/100) * (1 + input$impuestos/100)
    data.frame(
      Concepto = c("Media de Siniestros (E[N])", "Prima Pura (E[S])", "Prima Comercial"),
      Valor = c(as.character(round(res$E_N, 2)), 
                as.character(dollar(res$prima_pura)), 
                as.character(dollar(p_com)))
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
  
  # Histograma de frecuencia de siniestros (N)
  output$hist_frecuencia <- renderPlot({
    req(resultados())
    ggplot(data.frame(N = resultados()$frecuencia), aes(x = N)) +
      geom_histogram(fill = "#3498db", color = "white", binwidth = 1) +
      theme_minimal() + 
      labs(title = "Distribución de la Frecuencia (N)", x = "Número de Siniestros", y = "Frecuencia")
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
  
  output$sensibilidad_deducible <- renderPlot({
    req(resultados())
    res <- resultados()
    # Optimización: Cálculo vectorial de la prima para diferentes deducibles
    ded_seq <- seq(0, max(res$costos_raw, 1000), length.out = 15)
    primas <- sapply(ded_seq, function(d) {
      mean(res$frecuencia) * mean(pmin(pmax(res$costos_raw - d, 0), input$limite_max))
    })
    ggplot(data.frame(d = ded_seq, p = primas), aes(d, p)) + 
      geom_line(color = "#e67e22", size = 1) + labs(x="Deducible ($)", y="Prima Pura ($)") + theme_minimal() + scale_y_continuous(labels = dollar)
  })
  
  output$sensibilidad_limite <- renderPlot({
    req(resultados())
    res <- resultados()
    lim_seq <- seq(100, input$limite_max * 2, length.out = 15)
    primas <- sapply(lim_seq, function(l) {
      mean(res$frecuencia) * mean(pmin(pmax(res$costos_raw - input$deducible, 0), l))
    })
    ggplot(data.frame(l = lim_seq, p = primas), aes(l, p)) + 
      geom_line(color = "#27ae60", size = 1) + labs(x="Límite ($)", y="Prima Pura ($)") + theme_minimal() + scale_y_continuous(labels = dollar)
  })
  
  output$plot_ci <- renderPlot({
    req(resultados()); ggplot(data.frame(S = resultados()$S), aes(x = S)) +
      geom_density(fill = "#3498db", alpha = 0.4) +
      geom_vline(xintercept = c(resultados()$ci_low, resultados()$ci_high), color = "red", linetype = "dashed") +
      theme_minimal() + scale_x_continuous(labels = dollar)
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
      paste0("Informe_Simulacion_", Sys.Date(), ".html")
    },
    content = function(file) {
      # 1. Mostrar notificación al usuario
      showNotification("Generando informe... esto puede tardar unos segundos.", type = "message")
      
      # 2. Crear una ruta temporal segura
      temp_dir <- tempdir()
      temp_report <- file.path(temp_dir, "reporte.Rmd")
      
      # 3. Copiar el archivo Rmd a la carpeta temporal
      # Importante: Asegúrate que el archivo se llame reporte.Rmd (todo en minúsculas)
      file.copy("reporte.Rmd", temp_report, overwrite = TRUE)
      
      # 4. Renderizar usando un entorno aislado
      tryCatch({
        rmarkdown::render(
          input = temp_report,
          output_file = file, # Shiny maneja el destino final aquí
          params = list(
            resultados = resultados(),
            input = input
          ),
          # Esto es vital en la nube para evitar conflictos de variables
          envir = new.env(parent = globalenv())
        )
      }, error = function(e) {
        showNotification(paste("Error al generar el reporte:", e$message), type = "error")
      })
    }
  )
  
}


