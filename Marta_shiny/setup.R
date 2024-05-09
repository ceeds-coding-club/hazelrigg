library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)
library(zoo)

## Sam's data wrangling & cleaning -----------------

hr <- read_xls("data/Hazelrigg Rainfall 1966-.xls",
               range = "year!A1:BI367",
               col_types = c("numeric", "numeric", rep("text", 59))
               ) |>
  pivot_longer(cols = "1966":"2024", names_to = "year", values_to = "rainfall_mm") |>
  mutate(rainfall_mm = rainfall_mm |>
           #replace Trace|trace|tr|Tr with 0.01
           str_replace_all("[Tt]{1}.*\\b", "0.01") |>
           #replace anything starting with "a point"." with "0."
           # some ambiguity because could be that ".0" was typed instead of "0."
           str_replace_all("^\\.(.*)", "0.\\1") |>
           #remove trailing point symbol
           str_replace_all("(.*)\\.$", "\\1") |>
           #convert to numeric and round to 2 dp
           as.numeric() |> round(digits = 2)
  ) |>
  arrange(year, Month, Date) |>
  clean_names()

## Marta's plotting rolling means of rainfall data -----------------

# Rename date to day
hr$day <- hr$date

# Force date into a date type and filter leap days (because...yeah)
hr$date <- dmy(paste0(hr$day,"/",hr$month,"/",hr$year))
hr <- hr %>% filter(!is.na(date))

# Trim start and end to remove NAs
# This is currently just hardcoded but could be automagic
hr_trim <- hr[274:21244,]
is.na(hr_trim$rainfall_mm) <- 0

plot_rma <- function(windowsize, daterange, plotbars=TRUE){
  if (windowsize < 1){
    windowsize <- 1
  }
  # generate a rolling average of hr_trim
  hr_subset <- hr_trim
  hr_subset$rollrain <- rollmean(hr_trim$rainfall_mm, k=windowsize, fill=c(0,0,0))
  
  # Trim off dummy-filled values
  trimsize <- round((windowsize+10)/2)
  hr_subset <- hr_subset[trimsize:(nrow(hr_subset)-trimsize),]
  
  # Trim to year range
  hr_subset <- hr_subset |> filter(date %within% interval(ymd(paste(daterange[1], "1", "1", sep="/")), ymd(paste(daterange[2], "12", "31", sep="/"))))
  
  # Plotit!
  p <- ggplot(hr_subset, aes(x=date, y=rollrain))
  if(plotbars){
    p <- p + geom_col(aes(y=rainfall_mm), alpha=1)
  }
  p <- p + geom_line(size=1, colour="blue") + theme_minimal() + labs(x="Date", y="Rainfall (mm)")
  return(p)
  
}

# print(plot_rma(30, c(2019, 2023), T))


