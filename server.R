library("ggplot2")

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    #### data ####
    # get input from ui
    m_data <- input$m_data
    sd_data <- input$sd_data

    # create data
    set.seed(20230807)
    x <-  rnorm(n = 10, mean = m_data, sd = sd_data)
    
    # sequence for x-axis
    mu_hat <- seq(70, 130, length.out = 200)
    
    ##### calculate Likelihood #####
    # calculates for every created x value (mu_hat) the likelihood for given data
    Likelihood_function <- sapply(mu_hat, FUN = function(i_mu){
      prod(dnorm(x, mean = i_mu, sd = sd_data))
    },
    simplify = TRUE)
    
    # Normierung der Likelihood
    den_like <- Bolstad::sintegral(mu_hat, Likelihood_function) # Normierungskonstante
    Likelihood_function_norm <- Likelihood_function / den_like$value # normierte Likelihood
    
    
    #### Prior ####
    mu_prior <- input$mu_prior
    tau_prior <- input$tau_prior
    prior_dens <- dnorm(mu_hat, mean = mu_prior, sd = tau_prior)
    
    
    
    #### Posteriori ####
    # calculate posteriori
    posterior0 <- prior_dens * Likelihood_function_norm 
    
    # Posteriori normieren
    den_post <- Bolstad::sintegral(mu_hat, posterior0)
    posterior <- posterior0 / den_post$value
    
    
    
    #### Plot ####
    # setting colours
    cols <- viridis::viridis(n = 3)
    
    
    ggplot(NULL, aes(x = mu_hat)) +
      
      # Likelihood of data
      geom_line(aes(y = Likelihood_function_norm, col  = "Likelihood Data")) +
      geom_area(aes(y = Likelihood_function_norm, fill = "Likelihood Data"), alpha = .4) +
      
      # Prior
      geom_line(aes(y = prior_dens, col  = "Prior")) +
      geom_area(aes(y = prior_dens, fill = "Prior"), alpha = .4) +
      
      # Posterior
      geom_line(aes(y = posterior, col  = "Posterior")) +
      geom_area(aes(y = posterior, fill = "Posterior"), alpha = .4) +
      
      # Labs, Theme etc.
      scale_colour_manual("Verteilung", 
                          breaks = c("Likelihood Data", "Prior", "Posterior"),
                          values = cols) +
      scale_fill_manual("Verteilung", 
                        breaks = c("Likelihood Data", "Prior", "Posterior"),
                        values = cols) +
      labs(title = "Beispiel: Intelligenz",
           y = "Likelihood",
           x = "Intelligenzwerte",
           caption = "Sehr schön zu sehen ist hier, dass in diesem simplen Fall die Likelihood der Daten einen sehr großen Einfluss hat.\n Das ändert sich mit zunehmender Komplexität des Modells.") +
      theme_bw() +
      theme(legend.justification = c("left", "top"), legend.position = c(.01, .99))
    
  })
}
