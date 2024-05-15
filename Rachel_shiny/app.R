#============== Top =================================
# Hazelrigg Rainfall Data Shiny App Run Script
# 03/05/2024
# Rachel Baxter
# https://github.com/Rach-BAX
# App for Hazelrigg weather CEEDS coding club project
# https://github.com/ceeds-coding-club/hazelrigg
# Script for running the app

library(shiny)

# run clean up script, set up figure function

source("setup.R") # refers to set up script that produces the graph

yrs <- unique(previous$year)


# ui --------------------------------------------

ui <- fluidPage(
  #App title
    h2("Daily Rainfall Measurements"),
    
  # Select Years
   plotOutput(outputId = "rain_data"),
   fluidRow(column(width = 3,
                  selectInput(inputId = "year_sel", 
                              label = "Pick year to compare", 
                              selected = year(today()), 
                              choices = yrs, 
                              multiple = F)))
  
)



# server ----------------------------------------

server <- function(input, output, session){
  
  output$rain_data <- renderPlot({
    rainfall_graph(yr = c(input$year_sel)) # for selecting different years
  })
    
    }


# run app --------------------------------

shinyApp(ui = ui, server = server)