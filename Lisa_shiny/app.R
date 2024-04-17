library(shiny)

#run the cleaning scripts, and generate the function for the figure
source("Lisa_shiny/setup.R")

yrs <- unique(hr_annual$year)

#Define UI for temperature highlighting different years

ui <- fluidPage(
    #App title
    h2("Mean monthly rainfall (mm)"),
      h3("Hazelrigg weather station"),
    #selecting years
    plotOutput(outputId = "monthly_rain"),
    fluidRow(column(width = 6,
                    selectInput(inputId = "year_sel", 
                                label = "Pick year(s) to highlight", 
                                selected = year(today()), 
                                choices = yrs, 
                                multiple = T)),
             column(width = 6,
                    sliderInput(inputId = "year_slid",
                                label = "...or set a range", 
                                min = 1966, 
                                max = 2024, 
                                value = c(2024, 2024), 
                                sep = "", 
                                dragRange = T,
                                width = '100%')))
)

server <- function(input, output, session){
    output$monthly_rain <- renderPlot({
        highlight_month(yr = c(input$year_slid[1]:input$year_slid[2], input$year_sel))
        # highlight_month(yr = input$year_sel)
    })
}

shinyApp(ui = ui, server = server)
