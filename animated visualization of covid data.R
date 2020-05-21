#ANIMATED TIME SERIES PLOT

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(gganimate)library(gifski)
library(av)
library(gapminder)

data= read.csv(('latestdata.csv'),header = T) 

# Handling dates
datanew <- data %>% 
  mutate(date_confirmation = dmy(date_confirmation))

head(datanew$date_confirmation)

# animated time series plot

datanew %>% group_by(date_confirmation) %>%
  summarise(count=n())