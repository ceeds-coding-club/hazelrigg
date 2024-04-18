library(tidyverse)
library(readxl)
library(janitor)
library (lubridate)
library(wesanderson)


## Sam's data wrangling -----------------

hr <- read_xls("data/Hazelrigg Rainfall 1966-.xls",
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
    clean_names()


## Lisa's data wrangling ---------------

hr_annual <- hr %>% 
    mutate(t = str_c(month, date, sep = "_")) %>% 
    filter(!is.na(rainfall_mm)) %>% #remove 1966 Na's
    mutate(month = month(month, label = TRUE))

hr_monthly <- hr_annual %>% 
    group_by(year) %>% 
    mutate(ann_ave = mean(rainfall_mm)) %>% 
    group_by(year, month, ann_ave) %>% 
    summarise(monthly_rain = mean(rainfall_mm))

hr_overall = hr_monthly %>% 
    group_by(month) %>% 
    summarise(rain = mean(monthly_rain))

# function for plotting (Lisa) -----------------------

highlight_month <- function(yr = year(today())){
    pal <- wes_palette("Zissou1", n = length(yr), type = "continuous") #generate palette
    hr_monthly_sub <- hr_monthly %>%                                #subset dataset for highlights
        filter(year%in%yr)
    ggplot() +
        geom_line(data = hr_monthly, aes(x = month, y = monthly_rain, group = year),
                  show.legend = F, 
                  colour = "grey", 
                  alpha = 0.2,
                  linewidth = 1.5) +
        geom_line(data = hr_overall, aes(x=month, y = rain, group = 1), 
                  colour = "grey40",
                  linewidth = 1.2, show.legend = T)+
        geom_line(data = hr_monthly_sub, 
                  aes(x = month, y = monthly_rain, colour = year, group = year),
                  linewidth = 1.5,
                  linejoin = "round", 
                  lineend = "round")+
        labs(y = "Average rain (mm)", x = "Month")+
        scale_colour_manual(values = pal) + 
        scale_x_discrete(expand = c(0.01, 0)) +
        scale_y_continuous(expand = c(0, 0)) + 
        theme_classic()+
        theme(axis.title = element_text(size = 16, face = "bold"),
             axis.text = element_text(size = 14),
             text = element_text(family = "arial"),
             legend.text = element_text(size = 12),
             legend.title = element_text(size = 12, face = "bold"))

}
