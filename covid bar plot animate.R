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
new <- datanew %>%
  filter(country == 'United States' |
           country == 'France' |
           country == 'United Kingdom' |
           country =='China') %>%
  filter(month==2|month==3) %>%
  group_by(country,month) %>%
  summarise(count=n())


p<-new %>% ggplot(aes(x=country,y=count,fill=country))+
  geom_bar(stat='identity')+
  geom_point(size=1.5)+
  scale_y_log10()+
  theme_bw()+
  guides(Fill=F)

#animated bar plot by month
p + transition_time(as.integer(month))+
  labs(title='Animated bar plot for covd 19 by month',
       subtitle = 'Month time')




#animated bar plot by country
p + transition_states(count)+
  labs(title='Animated animated bar plot by country ')+
  shadow_mark()+
  enter_grow()

# animated bar plot by day

new1 <- datanew %>%
  filter(country == 'United States' |
           country == 'France' |
           country == 'United Kingdom' |
           country =='China') %>%
  filter(day==1|day==30) %>%
  group_by(country,day) %>%
  summarise(count=n())


p1<-new1 %>% ggplot(aes(x=country,y=count,fill=country))+
  geom_bar(stat='identity')+
  geom_point(size=1.5)+
  scale_y_log10()+
  theme_bw()+
  guides(Fill=F)

#animated bar plot by day and country
p1 + transition_time(as.integer(day))+
  labs(title='Animated bar plot for covd 19 by month',
       subtitle = 'Month time')

