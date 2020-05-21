#ANIMATED TIME SERIES PLOT

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

head(datanew$date_confirmation)

# animated time series plot

datanew %>% group_by(date_confirmation) %>%
  summarise(count=n()) %>%
  mutate(cuml=cumsum(count)) %>%
  ggplot(aes(x=date_confirmation,y=cuml)) +
  geom_line(color='red')+
  geom_point(size=1.5)+
  geom_area(fill='red')+
  theme_bw()+
  ggtitle('Daily Cumulative values')+
  transition_reveal(cuml)


# save animation
anim_save('cumlplot')

#data completion for 4 countries

#extract day and month information
datanew$day<-day(datanew$date_confirmation)
datanew$month<-month(datanew$date_confirmation)

new <-datanew %>%
  filter(month == 3) %>%
  group_by(day,country) %>%
  summarise(count=n())

new <- data.frame(complete(new, day, country,
                           fill = list(count = 0)))
new %>% filter(country == 'United States' |
                 country == 'France' |
                 country == 'United kingdom' |
                 country =='China') %>%
  ggplot(aes(x= day, y=count,group=country,color=country)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ggtitle('Animated Daily Line Plot')+
  transition_reveal(day)



 
 #bubble plot
 
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
 
 
 
 
   
 
 
 
 
 
 
 

  


                 
  




