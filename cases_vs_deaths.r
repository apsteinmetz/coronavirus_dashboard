# correlate deaths and cases by state
library(tidyverse)

# source https://github.com/nytimes/covid-19-data.git
us_states <- read_csv("~/R Projects/covid-19-data/us-states.csv")
#us_states_live <- read_csv("~/R Projects/covid-19-data/live/us-states.csv")

# create rolling average changes and lags
us_states <- us_states %>% 
  group_by(state) %>% 
  mutate(cum_cases = cases,cum_deaths = deaths) %>% 
  mutate(cases_7day = (cases - lag(cases,7))/7) %>% 
  mutate(deaths_7day = (deaths - lag(deaths,7))/7) %>% 
  mutate(d0 = deaths_7day) %>% 
  mutate(d7 = lead(d0,7),d17 = lead(d0,17),d21=lead(d0,21))

us_states %>% 
  filter(state %in% state.name[1:10]) %>% 
  ggplot(aes(date,cases)) + geom_line() +
  facet_wrap(~state,scales = "free") +
#  scale_y_log10() + 
  theme(legend.position = "none") +
  geom_line(aes(y=deaths*20),color="red")


state1 = "Florida"

coeff = 45
us_states %>%  
  #filter(state %in% c("Florida","Texas","California","New York")) %>% 
  filter(state == state1) %>%
  ggplot(aes(date,cases_7day)) + geom_point(color="orange") +
  geom_point(aes(y=d0*coeff),color="red") +
  scale_y_continuous(
    name = "Cases",
    sec.axis = sec_axis(d0~./coeff,name="Deaths")) +
  theme(
    axis.title.y = element_text(color = "orange", size=13),
    axis.title.y.right = element_text(color = "red", size=13)
  ) + 
  labs(title = paste(state1, "7-Day Moving Average"),
#       subtitle = "Deaths lagged by 17 Days",
       x = "2020") +
  geom_vline(xintercept = as.Date("2020-07-14")) + 
  geom_vline(xintercept = as.Date("2020-07-14")+17) + 
  annotate(geom = "text",x=Sys.Date()-35,y=11000,label = "Prediction Date") +
  NULL

us_states %>%  
  #filter(state %in% c("Florida","Texas","California","New York")) %>% 
  filter(state == state1) %>%
  ggplot(aes(cases_7day,deaths_7day)) + geom_point(color="orange") + geom_smooth(method="lm")
  NULL

  
m <- lm(d17 ~ cases_7day+date+poly(cases_7day,2,raw = TRUE),data=filter(us_states,state==state1))
m <- lm(d17 ~ cases_7day+date,data=filter(us_states,state==state1))
#predict(m,data.frame(cases=3000))
summary(m)
