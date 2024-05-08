
### Ingrid's shiny to compare hazelrigg data with other locations

### WARNING - currently very messy!!!!!!!!!!! 


library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(rvest)


### cities that don't work
# york - scraping is getting the table above the climate table 
# san francisco - scraping is getting something other than the table (maybe the correct table but lots of extra information by mistake??)

###############################################################################
# attempt to put sunshine into shiny script - combining 2 data frames in function
### OUTISDE UI or SERVER
# get the hazelrigg data
sun <- read_xls("C:/Users/rober115/OneDrive - Lancaster University/Data for others/R group/Hazelrigg/Hazelrigg Sunshine 1966-.xls",
                range = "year!A1:BI367",
                col_types = c("numeric", "numeric", rep("text", 59))) |>
  pivot_longer("1966":"2024", names_to = "year", values_to = "sunshine_h") |>
  mutate(sunshine_h = sunshine_h |>
           # correct NAs
           str_replace_all("NaN", "NA")) |>
  arrange(year, Month, Date) |>
  clean_names() |>
  # make year numeric
  mutate_at(c('year','sunshine_h'), as.numeric) |> 
  # add a yyyy-mm-dd column
  mutate(yyyymmdd = make_date(year,month,date), .after=year)

# summarise data
# get total per month
tpm<-sun |>
  group_by(year, month) |>
  summarise(total=sum(sunshine_h, na.rm=T))

# get average across years
avg<-tpm |>
  group_by(month) |>
  summarise(mean=mean(total, na.rm=T))

# add date
avg$date<-paste0("2024-",ifelse(avg$month<10,"0",""),avg$month,"-01")
# change months to date format
avg$date<-as.POSIXct(avg$date)

# function for getting and processing web data
scrape <- function(location){
  link <- paste0("https://en.wikipedia.org/wiki/",location,"#Climate")
  page <- read_html(link)
  climate <- html_element(page, "table.wikitable.mw-collapsible") |>
    html_table()
  # tidy data
  climate <- climate |> 
    row_to_names(row_number = 1) |> # get rid of table titles
    pivot_longer(-Month) |> # pivot table
    rename(parameter=Month,month=name) |> # change column names
    filter(!month=='Year') # remove yearly average
  # remove farenheit in brackets
  climate$value<-gsub("\\s*\\([^\\)]+\\)","", climate$value)
  climate$parameter<-gsub("\\s*\\([^\\)]+\\)","", climate$parameter)
  # change value to numeric
  climate$value<-as.numeric(climate$value)
  # change abbreviated month to numeric month
  climate$month<-match(climate$month,month.abb)
  # add full date
  climate$date<-paste0("2024-", ifelse(climate$month<10,"0",""),climate$month,"-01")
  # change months to date format
  climate$date<-as.POSIXct(climate$date)
  # subset sunshine data
  othersun<-subset(climate, parameter=="Mean monthly sunshine hours")
  # combine two data sets
  final<-cbind(othersun, avg[,2])
  # return???
  return(final)
}

# get world map data
world <- map_data("world")

# UI
ui<- fluidPage(
  titlePanel("Hazelrigg sunshine hours"),
  sidebarLayout(
    sidebarPanel(
      textInput("txt", label = "Compare Hazelrigg with city:", value = ""),
      actionButton("update" ,"Go"),
    ),
    mainPanel(fluidRow(
                verticalLayout(plotOutput("plot"), plotOutput("plot2"))
              )
    )
  ),
)

# SERVER
server<- function(input, output) {
  
  react <- reactive({ # does 'input$update,' need to be between two brackets to only update when button pressed? but this causes error 
    
    scrape(input$txt) 
    
  })
  
  
  # plot Hazelrigg only
  output$plot <- renderPlot({
      ggplot()+
      geom_bar(data=avg, aes(date, mean), stat="identity", fill="#3b65b8")+
      #geom_point(data=react, aes(date,mean))+
      #if(!is.null(react))geom_point(data=react, aes(date,mean))+
      {if(!is.null(input$txt))geom_point(data= react(),aes(date,value),colour="red")}+
      #if(!is.null(input$txt))geom_point(data=react, aes(date,value))+
      #{if(!is.null(input$txt))geom_point(aes(date, value), colour="red")}+
      #annotate(geom="text", x=as.POSIXct("2024-12-01"), y=ifelse(max(avg$mean)<max(othersun$value),max(othersun$value),max(avg$mean)), label=input$txt, colour="red")+
      labs(title="Hazelrigg", subtitle=paste0("+ ",input$txt), x="Month",
           y="Average sunshine per month (h)", caption = paste0(input$txt," data source: Wikipedia"))+
      scale_x_datetime(date_label="%b",
                       breaks=seq(as.POSIXct("2024-01-01"),as.POSIXct("2024-12-01"), by="months"))+
      theme_bw()+
      theme(panel.grid=element_blank(),plot.subtitle=element_text(colour = "red"),
            plot.caption=element_text(size=10), plot.title = element_text(colour = "#3b65b8"))
  })
  
  output$plot2 <- renderPlot({
    world %>%
      ggplot(aes(x = long, y = lat, group = group))+
      geom_polygon(color = "white", linewidth = 0.2)+
      geom_point(x=-2.775644, y=54.014271, colour="#3b65b8", shape=18, size=6)+
      theme_minimal()+
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank())
  })
  
  
}

shinyApp(ui,server)





###############################################################################
# attempt to put sunshine into shiny script
# initial graph doesn't work, graph + scraped data does work, go button doesn't work
### OUTISDE UI or SERVER
# get the hazelrigg data
sun <- read_xls("C:/Users/rober115/OneDrive - Lancaster University/Data for others/R group/Hazelrigg/Hazelrigg Sunshine 1966-.xls",
                range = "year!A1:BI367",
                col_types = c("numeric", "numeric", rep("text", 59))) |>
  pivot_longer("1966":"2024", names_to = "year", values_to = "sunshine_h") |>
  mutate(sunshine_h = sunshine_h |>
           # correct NAs
           str_replace_all("NaN", "NA")) |>
  arrange(year, Month, Date) |>
  clean_names() |>
  # make year numeric
  mutate_at(c('year','sunshine_h'), as.numeric) |> 
  # add a yyyy-mm-dd column
  mutate(yyyymmdd = make_date(year,month,date), .after=year)

# summarise data
# get total per month
tpm<-sun |>
  group_by(year, month) |>
  summarise(total=sum(sunshine_h, na.rm=T))

# get average across years
avg<-tpm |>
  group_by(month) |>
  summarise(mean=mean(total, na.rm=T))

# add date
avg$date<-paste0("2024-",ifelse(avg$month<10,"0",""),avg$month,"-01")
# change months to date format
avg$date<-as.POSIXct(avg$date)

# function for getting and processing web data
scrape <- function(location){
  link <- paste0("https://en.wikipedia.org/wiki/",location,"#Climate")
  page <- read_html(link)
  climate <- html_element(page, "table.wikitable.mw-collapsible") |>
    html_table()
  # tidy data
  climate <- climate |> 
    row_to_names(row_number = 1) |> # get rid of table titles
    pivot_longer(-Month) |> # pivot table
    rename(parameter=Month,month=name) |> # change column names
    filter(!month=='Year') # remove yearly average
  # remove farenheit in brackets
  climate$value<-gsub("\\s*\\([^\\)]+\\)","", climate$value)
  climate$parameter<-gsub("\\s*\\([^\\)]+\\)","", climate$parameter)
  # change value to numeric
  climate$value<-as.numeric(climate$value)
  # change abbreviated month to numeric month
  climate$month<-match(climate$month,month.abb)
  # add full date
  climate$date<-paste0("2024-", ifelse(climate$month<10,"0",""),climate$month,"-01")
  # change months to date format
  climate$date<-as.POSIXct(climate$date)
  # subset sunshine data
  othersun<-subset(climate, parameter=="Mean monthly sunshine hours")
  # return???
  return(othersun)
}


# UI
ui<- fluidPage(
  titlePanel("Hazelrigg sunshine hours"),
  sidebarLayout(
    sidebarPanel(
      textInput("txt", label = "Compare Hazelrigg with city:", value = ""),
      actionButton("update" ,"Go"),
    ),
    mainPanel(
      plotOutput("plot")
    )
  ),
)

# SERVER
server<- function(input, output) {
  
  idk_react <- reactive({ # does 'input$update,' need to be between two brackets to only update when button pressed? but this causes error 
    
    scrape(input$txt) # this returns a list (1:3 in list are avg & 4:7 are othersun)
    
  })
  
  # plot Hazelrigg + other location
  output$plot <- renderPlot({
    idk_react() |>
      ggplot()+
      geom_bar(data=avg, aes(date, mean), stat="identity")+
      {if(!is.null(input$txt))geom_point(aes(date, value), colour="red")}+
      #annotate(geom="text", x=as.POSIXct("2024-12-01"), y=ifelse(max(avg$mean)<max(othersun$value),max(othersun$value),max(avg$mean)), label=input$txt, colour="red")+
      labs(title="Hazelrigg", subtitle=paste0("+ ",input$txt), x="Month",
           y="Average sunshine per month (h)", caption = "Non Hazelrigg data source: Wikipedia")+
      scale_x_datetime(date_label="%b",
                       breaks=seq(as.POSIXct("2024-01-01"),as.POSIXct("2024-12-01"), by="months"))+
      theme_bw()+
      theme(panel.grid=element_blank(),plot.subtitle=element_text(colour = "red"),
            plot.caption=element_text(size=10))
  })
  
}


shinyApp(ui,server)


###############################################################################
### above but for rainfall
hr <- read_xls("C:/Users/rober115/OneDrive - Lancaster University/Data for others/R group/Hazelrigg/Hazelrigg Rainfall 1966-.xls",
               range = "year!A1:BI367",
               col_types = c("numeric", "numeric", rep("text", 59))
) |>
  pivot_longer("1966":"2024", names_to = "year", values_to = "rainfall_mm") |>
  mutate(rainfall_mm = rainfall_mm |>
           #replace Trace|trace|tr|Tr with 0.01
           str_replace_all("[Tt]{1}.*\\b", "0.01") |>
           #replace anything starting with "a point"." with "0."
           # some ambiguity because could be that ".0" was typed instead of "0."
           str_replace_all("^\\.(.*)", "0.\\1") |>
           #remove trailing point symbol
           str_replace_all("(.*)\\.$", "\\1") |>
           #convert to numeric and round to 2 dp
           as.numeric() #|> round(digits = 2)
  ) |>
  arrange(year, Month, Date) |>
  clean_names() |>
  # make year numeric
  mutate_at('year', as.numeric) |>
  # add a yyyy-mm-dd column
  mutate(yyyymmdd = make_date(year,month,date), .after=year)

# summarise data
# get total per month
tpmhr<-hr |>
  group_by(year, month) |>
  summarise(total=sum(rainfall_mm, na.rm=T))

# get average across years
avghr<-tpmhr |>
  group_by(month) |>
  summarise(mean=mean(total, na.rm=T))

# add date
avghr$date<-paste0("2024-",ifelse(avghr$month<10,"0",""),avghr$month,"-01")
# change months to date format
avghr$date<-as.POSIXct(avghr$date)

# scrape function already exists above
# but here it is altered to work with all data (not just e.g. sunshine)
# function for getting and processing web data
scraperain <- function(location){
  link <- paste0("https://en.wikipedia.org/wiki/",location,"#Climate")
  page <- read_html(link)
  climate <- html_element(page, "table.wikitable.mw-collapsible") |>
    html_table()
  # tidy data
  climate <- climate |> 
    row_to_names(row_number = 1) |> # get rid of table titles
    pivot_longer(-Month) |> # pivot table
    rename(parameter=Month,month=name) |> # change column names
    filter(!month=='Year') # remove yearly average
  # remove farenheit in brackets
  climate$value<-gsub("\\s*\\([^\\)]+\\)","", climate$value)
  climate$parameter<-gsub("\\s*\\([^\\)]+\\)","", climate$parameter)
  # change value to numeric
  climate$value<-as.numeric(climate$value)
  # change abbreviated month to numeric month
  climate$month<-match(climate$month,month.abb)
  # add full date
  climate$date<-paste0("2024-", ifelse(climate$month<10,"0",""),climate$month,"-01")
  # change months to date format
  climate$date<-as.POSIXct(climate$date)
  # subset sunshine data
  othersun<-subset(climate, parameter=="Average precipitation mm" | parameter=="Average rainfall mm")
  # return???
  return(othersun)
}

# UI
ui<- fluidPage(
  titlePanel("Hazelrigg rainfall"),
  sidebarLayout(
    sidebarPanel(
      textInput("txt", label = "Compare Hazelrigg with city:", value = ""),
      actionButton("update" ,"Go"),
    ),
    mainPanel(
      plotOutput("plot")
    )
  ),
)

# SERVER
server<- function(input, output) {
  
  idk_react <- reactive({ # does 'input$update,' need to be between two brackets to only update when button pressed? but this causes error 
    
    scraperain(input$txt) # this returns a list (1:3 in list are avg & 4:7 are othersun)
    
  })
  
  # plot Hazelrigg + other location
  output$plot <- renderPlot({
    idk_react() |>
      ggplot()+
      geom_bar(data=avghr, aes(date, mean), stat="identity")+
      {if(!is.null(input$txt))geom_point(aes(date, value), colour="red")}+
      #annotate(geom="text", x=as.POSIXct("2024-12-01"), y=ifelse(max(avg$mean)<max(othersun$value),max(othersun$value),max(avg$mean)), label=input$txt, colour="red")+
      labs(title="Hazelrigg", subtitle=paste0("+ ",input$txt), x="Month",
           y="Average rainfall per month (mm)", caption = paste0(input$txt," data source: Wikipedia"))+
      scale_x_datetime(date_label="%b",
                       breaks=seq(as.POSIXct("2024-01-01"),as.POSIXct("2024-12-01"), by="months"))+
      theme_bw()+
      theme(panel.grid=element_blank(),plot.subtitle=element_text(colour = "red"),
            plot.caption=element_text(size=10))
  })
  
}


shinyApp(ui,server)

# map of the location
# get world map
world <- map_data("world")
# plot
world %>%
  ggplot(aes(x = long, y = lat, group = group))+
  geom_polygon(color = "white", linewidth = 0.2)+
  geom_point(x=0, y=0, colour="red", shape=18)+
  #labs(title="Cowpea production 2021")+
  theme_minimal()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())


###############################################################################
# shiny app - text input with go button (WORKING!), outputs text (ui & server separate)
ui<- fluidPage(
  titlePanel("Hazelrigg sunshine hours"),
  sidebarLayout(
    sidebarPanel(
      textInput("txt", label = "Compare Hazelrigg with city:", value = ""),
      actionButton("update" ,"Go"),
    ),
    mainPanel(
      verbatimTextOutput("value")
    )
  ),
)

server<- function(input, output) {
  output$value <- renderText({ 
    input$update
    isolate(input$txt)
  })
}

shinyApp(ui,server)

