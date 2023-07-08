### Global Zeugs ###
library(ggplot2)
library(shiny)
library(shinythemes)
library(viridis)




ui <- fluidPage(
            
            # Application title
            titlePanel("Verhältnis von Prior, Data Likelihood & Posteoriori"),
            
            # define sidebars
            
            sidebarLayout(
              sidebarPanel(
                
                # Sidebar with a slider input for Prior Mean
                sliderInput(inputId = "mu_prior",
                            label = "Prior Mean",
                            min = 85,
                            max = 115,
                            value = 100),
                
                
                # Sidebar with a slider input for Prior Tau
                sliderInput(inputId = "tau_prior",
                            label = "Prior Tau",
                            min = 5,
                            max = 25,
                            value = 15),
              
                
                
                # Sidebar with a slider input for Data Mean
                sliderInput(inputId = "m_data",
                            "Data Mean",
                            min = 85,
                            max = 115,
                            value = 110),
                
                
                
                # Sidebar with a slider input for Data sd
                sliderInput(inputId = "sd_data",
                            "Data SD",
                            min = 5,
                            max = 25,
                            value = 20),
                
                
                
                # Sidebar with a slider input for n Data
                sliderInput(inputId = "n_data",
                            "Sample Size",
                            min = 10,
                            max = 1000,
                            value = 10),
                
                
                

                
              ),
              
              # Show a plot of the generated distribution
              mainPanel(
                plotOutput("distPlot")
              )
            )
  )




server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    x <- seq(70, 130, length = 100)
       
    mu_prior <- input$mu_prior
    tau_prior <- input$tau_prior
    m_data <- input$m_data
    sd_data <- input$sd_data
    n_data <- input$n_data
    
    sd_post <- (1 / ((1 / tau_prior) + (n_data / sd_data)))
    m_post <- sd_post * (((1 / tau_prior) * mu_prior) + ((n_data / sd_data) * m_data))
    
    cols <- viridis(n = 3)
    
    
    ggplot(data.frame(x), aes(x = x)) +
      stat_function(fun = dnorm, args = list(mean = mu_prior, sd = tau_prior), aes(colour = "Prior")) +
      stat_function(fun = dnorm, args = list(mean = m_data, sd = sd_data), aes(colour = "Data")) +
      stat_function(fun = dnorm, args = list(mean = m_post, sd = sd_post), aes(colour = "Posteriori")) +
      geom_area(aes(x = x, y = dnorm(x, mean = mu_prior, sd = tau_prior), fill = "Prior"), alpha = .4) +
      geom_area(aes(x = x, y = dnorm(x, mean = m_data, sd = sd_data), fill = "Data"), alpha = .4) +
      geom_area(aes(x = x, y = dnorm(x, mean = m_post, sd = sd_post), fill = "Posteriori"), alpha = .4) +
      labs(y = "Likelihood") +
      scale_colour_manual("Verteilung", 
                          breaks = c("Prior", "Data", "Posteriori"),
                          values = cols) +
      scale_fill_manual("Verteilung", 
                        breaks = c("Prior", "Data", "Posteriori"),
                        values = cols) +
      labs(title = "Beispiel: Intelligenz",
           caption = "Sehr schön zu sehen ist hier, dass in diesem simplen Fall die Likelihood der Daten einen sehr großen Einfluss hat.\n Das ändert sich mit zunehmender Komplexität des Modells.") +
      theme_bw() +
      theme(legend.justification = c(.9, .9), legend.position = c(.9, .9)) +
      coord_cartesian(ylim = c(0, .05))
      
    
    
    
    
    
    
    
  })
}




shinyApp(ui, server)
