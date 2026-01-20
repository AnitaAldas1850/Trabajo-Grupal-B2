server <- function(input, output) {
  
  # Función para simular frecuencia
  simular_frecuencia <- reactive({
    n <- input$n_sim
    
    switch(input$dist_frec,
           "Poisson" = rpois(n, input$lambda * input$periodo),
           "Binomial" = rbinom(n, size = input$n_trials, prob = input$p_success),
           "Binomial Negativa" = rnbinom(n, size = input$r_param, prob = input$p_nbinom)
    )
  })

  # Función para simular severidad
  simular_severidad <- function(n_siniestros) {
    total_siniestros <- sum(n_siniestros)
    
    if(total_siniestros == 0) return(rep(0, length(n_siniestros)))
    
    switch(input$dist_sev,
           "Exponencial" = {
             # Simular todos los costos
             costos <- rexp(total_siniestros, rate = input$rate_exp)
           },
           "Gamma" = {
             costos <- rgamma(total_siniestros, shape = input$shape_gamma, rate = input$rate_gamma)
           },
           "Lognormal" = {
             costos <- rlnorm(total_siniestros, meanlog = input$meanlog, sdlog = input$sdlog)
           },
           "Weibull" = {
             costos <- rweibull(total_siniestros, shape = input$shape_weibull, scale = input$scale_weibull)
           }
    )
        # Distribuir costos según número de siniestros
    perdida_agregada <- numeric(length(n_siniestros))
    idx <- 1
    for(i in 1:length(n_siniestros)) {
      if(n_siniestros[i] > 0) {
        perdida_agregada[i] <- sum(costos[idx:(idx + n_siniestros[i] - 1)])
        idx <- idx + n_siniestros[i]
      }
    }
    
    return(perdida_agregada)
  }
  output$EN <- renderText({
    "—"
  })
  
  output$EX <- renderText({
    "—"
  })
  
  output$ES <- renderText({
    "—"
  })
  
}


