#============== Top =================================
# Hazelrigg Rainfall Data Shiny App Set Up Script
# 03/05/2024
# Rachel Baxter
# https://github.com/Rach-BAX
# App for Hazelrigg weather CEEDS coding club project
# https://github.com/ceeds-coding-club/hazelrigg
# Script for setting up the app




# Libraries -------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)

# Sam's Clean up code -------------------------------------------------

rainfall <- read_xls("data/Hazelrigg Rainfall 1966-.xls",
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


# Extra cleaning ----------------------------------------

#remove 1966

rainfall <- rainfall %>%
  filter(year != 1966)

# set today's date to use as cutoff for current data
current_year <- year(today())



# Build dataframes --------------------------------------

# Make dataframe for previous year's data for linearrange

previous <- rainfall %>%
  group_by(year, month) %>%
  arrange(year, month, date) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(newdate = seq(1, length(date))) %>%   # label days as 1:365 (will represent x-axis)         
  ungroup() %>%
  filter(year != current_year) %>% # filter out current year
  group_by(newdate) %>%
  mutate(upper = max(rainfall_mm), # identify max value for each day
         lower = min(rainfall_mm), # identify min value for each day
         avg = mean(rainfall_mm),  # calculate mean value for each day
         se = sd(rainfall_mm)/sqrt(length(rainfall_mm))) %>%  # calculate standard error of mean
  mutate(avg_upper = avg+(2.101*se),  # calculate 95% CI for mean
         avg_lower = avg-(2.101*se)) %>%  # calculate 95% CI for mean
  ungroup()


# Make dataframe of current year's data (for the line)

current <- rainfall %>%
  group_by(year, month) %>%
  arrange(year, month, date) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(newdate = seq(1, length(date))) %>%  # create matching x-axis as historical data
  ungroup() %>%
  filter(year == current_year)  # select current year data


# Function to build the plot -----------------------------------------------------

rainfall_graph <- function(yr = year(today())) {
# subset previous year's data to create selection option
  subset <- previous %>%
    filter(year%in%yr)
# crate some data for the line in the legend 
legend_data <- data.frame(x=c(45,46,47,48,49,50,51),y=c(68,68,70,72,70,71,69))
# Set up plot
 ggplot(previous, aes(newdate, rainfall_mm))+
# prev year min and max
  geom_linerange(previous, mapping=aes(x=newdate, ymin=lower, ymax=upper), color="snow3", linewidth = 0.98)+
# prev year normal range
  geom_linerange(previous, mapping=aes(x=newdate, ymin=avg_lower, ymax=avg_upper), color="deepskyblue3", linewidth = 0.95)+
# current year's data
  geom_line(current, mapping=aes(x=newdate, y=rainfall_mm, group=1), color='#45425A', size=1.05) +
# data for prev year comparison
  geom_line(subset, mapping=aes(x=newdate, y=rainfall_mm, group=2),color = "tomato2" , size=1.03, alpha = 0.85) +
# lines for each month
  geom_vline(xintercept = 31, colour = "darkslategray", linetype=3, size=.5) +
  geom_vline(xintercept = 59, colour = "darkslategray", linetype=3, size=.5) +
  geom_vline(xintercept = 90, colour = "darkslategray", linetype=3, size=.5) +
  geom_vline(xintercept = 120, colour = "darkslategray", linetype=3, size=.5) +
  geom_vline(xintercept = 151, colour = "darkslategray", linetype=3, size=.5) +
  geom_vline(xintercept = 181, colour = "darkslategray", linetype=3, size=.5) +
  geom_vline(xintercept = 212, colour = "darkslategray", linetype=3, size=.5) +
  geom_vline(xintercept = 243, colour = "darkslategray", linetype=3, size=.5) +
  geom_vline(xintercept = 273, colour = "darkslategray", linetype=3, size=.5) +
  geom_vline(xintercept = 304, colour = "darkslategray", linetype=3, size=.5) +
  geom_vline(xintercept = 334, colour = "darkslategray", linetype=3, size=.5) +
  geom_vline(xintercept = 365, colour = "darkslategray", linetype=3, size=.5) +
 # labels for each month
  coord_cartesian(ylim = c(0,80)) +
  scale_y_continuous(breaks = seq(-20,80, by=10)) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("January", "February", "March", "April",
                                "May", "June", "July", "August", "September",
                                "October", "November", "December"))+
  ylab("Rainfall (mm)")+
#remove background
  theme(panel.background = element_blank(), axis.title.x = element_blank(), axis.text.x  = element_text(size=16), axis.text.y  = element_text(size=16))+
#Legend annotation
  annotate("segment", x = 50, xend = 50, y = 65, yend = 75, colour = "snow3", size=7)+
  annotate("segment", x = 50, xend = 50, y = 68, yend = 72, colour = "deepskyblue3", size=7)+
  geom_line(data=legend_data, aes(x=x,y=y), color='#45425A', size=0.95)+
  annotate("text", x = 65, y = 70, label = "NORMAL RANGE", size=3.5, colour="gray30", fontface = "bold")+
  annotate("text", x = 67, y = 75, label = "RECORD HIGH", size=3.5, colour="gray30", fontface = "bold")+
  annotate("text", x = 65, y = 65, label = "RECORD LOW", size=3.5, colour="gray30", fontface = "bold")+
  annotate("text", x = 40, y = 68, label = current_year, size=4, colour="#45425A", fontface = "bold")



}

