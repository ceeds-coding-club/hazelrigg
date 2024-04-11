library(tidyverse)

# HAZELRIGG: Campbell Scientific Automatic Weather Stations
# 
# (A)	Main Hazelrigg meteorological enclosure, GR 493 578, c. 95m asl.
# (B)	New enclosure, GR 490 579, c.85m asl (just across the motorway from the N end of Campus).
# 
# Wind turbine is between the two sites at GR 492 578.
# 
# The data are all 10 minute averages (or totals) and the columns are:
#     
#     Station A:
#     
# E - Temperature (°C) (measured by temp/RH sensor in Stevenson Screen)
# F - Relative Humidity (%) in Stevenson Screen
# G - Solar irradiation: 10 min average (kW/m2)
# H - Solar irradiation: 10 min total (kJ/m2)
# I, J - Sunshine duration (minutes, seconds)
# K - Air temperature in Stevenson Screen (°C)
# L - Concrete temperature (°C)
# M - Grass temperature (°C)
# N, O, P, Q, R, S - Soil temperatures at 5, 10, 20, 30, 50, 100cm (°C)
# T, U, V, W - Sonic Anemometer (wind speed in 3 planes)
# Y - Windspeed at 10m on mast (m/s)
# Z - Wind direction at 10m on mast (degrees)
# AA - Rainfall 10 min total (mm)
# AB - Air Pressure (mbar)


log<-read.csv('data/loggers/CCSL006413_A_Ten_Min.dat', skip=1)
units<-log[1:2,]

# drop units and logger metadata
log<-log[-c(1:2),-c(2:4)] %>% 
    clean_names()

# create hourly values
hour<-log %>% mutate(date = str_split_fixed(timestamp, n=2, pattern = ' ')[,1],
                     time = str_split_fixed(timestamp, n=2, pattern = ' ')[,2],
                     hour = str_split_fixed(time, n=3, pattern = '\\:')[,1],
                     .after = timestamp) %>%
    mutate(across(c(hour, p_temp_c_avg:bp_mbar),as.numeric)) %>% 
    group_by(date, hour) %>% 
    summarise(air_temp_c = mean(air_tc_avg, na.rm=TRUE),
              rel_humidity = mean(rh),
              solar_irradiation = sum(slr_k_j_tot, na.rm=TRUE),
              sunshine_mins = sum(sun_mins) + sum(sun_secs)/60,
              windspeed = mean(a100lk_ws_ms_s_wvt),
              wind_direction = mean(w200p_wd_d1_wvt),
              rainfall = sum(rain_mm_tot),
              air_pressure = mean(bp_mbar)
              )
