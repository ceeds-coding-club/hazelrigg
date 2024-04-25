# Script creates commuting shiny panel [weather at 7-9 and 4-6]
source('James_shiny/setup.R')

commute<-hour %>% 
    filter(hour %in% c(7,8,16,17)) %>%
    mutate(time = ifelse(hour %in% c(7,8), 'am', 'pm')) %>% 
    group_by(month, time) %>% 
    summarise(across(air_temp_c:air_pressure, mean)) %>% 
    pivot_longer(-c(month, time), names_to = 'var', values_to = 'val')

vars<-c('air_temp_c', 'rainfall', 'sunshine_mins', 'windspeed')
ggplot(commute %>% filter(var %in% vars), aes(month, val, col=time)) +
    geom_line() +
    facet_wrap(~var, scales='free_y') + 
    labs(x = '') +
    scale_x_continuous(breaks=c(1:12), labels=month.abb) +
    theme_classic()

# number of raindays
raindays<-hour %>% 
    filter(hour %in% c(7,8,16,17)) %>%
    mutate(time = ifelse(hour %in% c(7,8), 'am', 'pm')) %>% 
    group_by(date, month, time) %>% 
    summarise(rainfall = sum(rainfall)) %>% 
    mutate(rainday = ifelse(rainfall > 0.1, 'rain', 'dry')) %>% 
    group_by(month, time) %>% 
    summarise(n = length(rainday), 
              raindays = length(rainday[which(rainday=='rain')]),
              p_rain = raindays / n)

ggplot(raindays, aes(month, p_rain, col=time)) + 
    geom_line() +
    labs(x = '', y = 'Chance of rain') +
    scale_x_continuous(breaks=c(1:12), labels=month.abb) +
    scale_y_continuous(labels=scales::percent) +
    theme_classic()
