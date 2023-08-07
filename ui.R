cols <- viridis::viridis(n = 3)


ui <- fluidPage(
  
  shinyWidgets::setSliderColor(c(rep(cols[1], 2), rep(cols[2], 2)), 1:5),
  
  
  # Application title
  titlePanel("Verhältnis von Likelihood der Daten, Priori & Posteoriori"),
  
  # define sidebars
  
  sidebarLayout(
    sidebarPanel(
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
                  value = 15)
      
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)