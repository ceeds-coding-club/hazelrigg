library(shiny)

ui <- fluidPage(
  #App title
  h2("Moving average of rainfall (mm)"),
  h3("Hazelrigg weather station"),
  #selecting years
  plotOutput(outputId = "monthly_rain"),
  fluidRow(column(width = 4,
                  numericInput(inputId = "roll_sel",
                               label = "Pick a rolling window size...",
                               min=1,
                               max=365,
                               value=50),
                  checkboxInput(inputId = "plotbars",
                                label = "Plot daily bars?",
                                value=TRUE)),
           column(width = 8,
                  sliderInput(inputId = "year_slid",
                              label = "Date range", 
                              min = 1966, 
                              max = 2024, 
                              value = c(2020, 2024), 
                              sep = "", 
                              dragRange = T,
                              width = '90%'))
           )
)

server <- function(input, output, session){
  output$monthly_rain <- renderPlot({
    plot_rma(input$roll_sel, input$year_slid, input$plotbars)
    # highlight_month(yr = input$year_sel)
  })
}

shinyApp(ui = ui, server = server)
