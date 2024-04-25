library(tidyverse)
library(janitor)

log<-read.csv('data/loggers/CCSL006413_A_Ten_Min.dat', skip=1)
units<-log[1:2,]

# drop units and logger metadata
log<-log[-c(1:2),] %>% 
    clean_names()

# create hourly values
hour<-log %>% mutate(date = as.Date(str_split_fixed(timestamp, n=2, pattern = ' ')[,1]),
                     month = month(date),
                     year = year(date),
                     time = str_split_fixed(timestamp, n=2, pattern = ' ')[,2],
                     hour = str_split_fixed(time, n=3, pattern = '\\:')[,1],
                     .after = timestamp) %>%
    mutate(across(c(hour, p_temp_c_avg:bp_mbar),as.numeric)) %>% 
    group_by(date, month, year, hour) %>% 
    summarise(air_temp_c = mean(air_tc_avg, na.rm=TRUE),
              rel_humidity = mean(rh),
              solar_irradiation = sum(slr_k_j_tot, na.rm=TRUE),
              sunshine_mins = sum(sun_mins) + sum(sun_secs)/60,
              windspeed = mean(a100lk_ws_ms_s_wvt),
              wind_direction = mean(w200p_wd_d1_wvt),
              rainfall = sum(rain_mm_tot),
              air_pressure = mean(bp_mbar)
    )
