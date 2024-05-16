#=============== Hazelrigg Weather Shiny App====================


# Info ------------------------------------------------------

 03/05/2024
 Rachel Baxter
 https://github.com/Rach-BAX
 App for Hazelrigg weather CEEDS coding club project
 https://github.com/ceeds-coding-club/hazelrigg

# Resources used -------------------------------------------
 *graph*
 https://johndjohnson.info/post/how-to-build-a-tufte-style-weather-graph-in-r-using-ggplot2/
 https://rpubs.com/bradleyboehmke/weather_graphic
 *shiny*
https://mastering-shiny.org/action-graphics.html

# Description --------------------------------------------

Rainfall graph for Shiny app. 
Recreation of the Tuft graph from: https://rpubs.com/bradleyboehmke/weather_graphic

Uses data collected daily from Hazelrigg weather station:
Hazelrigg Rainfall 1966-.xls
Data stored in 'data' folder

Graph charts rainfall in mm for 366 days of a year with ggplot's linearrange to show historical maximum and mean rainfall with current year's data plotted as a red line. 

Graph allows user to choose a year to compare to current year. 
Currently only one at a time because colours look wonky. 

Setup script includes:
* req'd libraries
* data cleaning function (Sam's code)
* Removal of 1966 data
* subsetted data from years excluding current year: 'previous' dataframe
* subsetted data from current year: 'current' dataframe
* function to build the plot (linear range + a line graph)

App script runs the app
