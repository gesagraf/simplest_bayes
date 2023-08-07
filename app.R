library("shiny")
library("rsconnect")

ui <- source("ui.R")
server <- source("server.R")

shinyApp(ui, server)
