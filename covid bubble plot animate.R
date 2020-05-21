
library(ggplot2) #for bar and line graphs
library(dplyr)
library(tidyr)
library(lubridate) # for date time and month
library(gganimate) # for animation
library(gifski)
library(av)
library(gapminder) #for bubble plot

data= read.csv(('latestdata.csv'),header = T) 

# Handling dates
datanew <- data %>% 
  mutate(date_confirmation = dmy(date_confirmation))

head(gapminder)
p2<- ggplot(gapminder,aes(x=gdpPercap,y=lifeExp,
                          size=pop,color=country))+
  geom_point(show.legend = F,alpha=0.7)+
  scale_x_log10()+
  labs(x='GDP per Capital',
       y='Life Expectancy')+
  scale_size(range = c(2,15))

# add animation to bubble plot

p2 + transition_time(year)+
  labs(title = 'Gdp per capitavl / Life expectancy')+
  shadow_wake(0.5)

# add animation to bubble plot - facet wrap 
#and for differnent continets

p2 + transition_time(year)+
  labs(title = 'Gdp per capitavl / Life expectancy')+
  shadow_wake(0.5)+
  facet_wrap(~continent)