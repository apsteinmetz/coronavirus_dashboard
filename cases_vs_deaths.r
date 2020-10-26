# correlate deaths and cases by state
library(tidyverse)
library(covid19nytimes)



# source https://github.com/nytimes/covid-19-data.git

us_states_long <- covid19nytimes::refresh_covid19nytimes_states()
#us_states <- read_csv("~/R Projects/covid-19-data/us-states.csv")
#us_states_live <- read_csv("~/R Projects/covid-19-data/live/us-states.csv")

# create rolling average changes and lags
us_states <- us_states_long %>%
  pivot_wider(names_from="data_type",values_from="value") %>% 
  rename(state=location) %>%
  select(date,state,cases_total,deaths_total) %>% 
  group_by(state) %>%
  mutate(cases_7day = (lag(cases_total,7) - cases_total)/7) %>%
  mutate(deaths_7day = (lag(deaths_total,7) - deaths_total)/7) %>%
  mutate(d0 = deaths_7day) %>%
  mutate(d7 = lag(d0,7),d17 = lag(d0,17),d21=lag(d0,21)) %>%
  {.}


us_states %>% 
  filter(state %in% state.name[1:10]) %>% 
  ggplot(aes(date,cases_total)) + geom_line() +
  facet_wrap(~state,scales = "free") +
#  scale_y_log10() + 
  theme(legend.position = "none") +
  geom_line(aes(y=deaths_total*20),color="red")


state1 = "Florida"

coeff = 45
us_states %>%  
  #filter(state %in% c("Florida","Texas","California","New York")) %>% 
  filter(state == state1) %>%
  ggplot(aes(date,cases_7day)) + geom_point(color="orange") +
  geom_point(aes(y=d17*coeff),color="red") +
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
#  geom_vline(xintercept = as.Date("2020-07-14")) + 
#  geom_vline(xintercept = as.Date("2020-07-14")+21) + 
#  annotate(geom = "text",x=Sys.Date()-35,y=11000,label = "Prediction Date") +
  NULL

gg <- us_states %>%  
  #filter(state %in% c("Florida","Texas","California","New York")) %>% 
  filter(state == state1) %>%
  ggplot(aes(cases_7day,d21)) + 
  scale_x_continuous(labels =scales::comma) + 
  geom_line(color="orange") + 
  geom_smooth(method="lm") +
  labs(y="Deaths Lagged 21 Days",
       x="New Cases - 7-day Average",
       title = "Florida Cases vs. Deaths") + 
  NULL
gg 

  
m <- lm(d17 ~ cases_7day+date+poly(cases_7day,2,raw = TRUE),data=filter(us_states,state==state1))
m <- lm(d17 ~ cases_7day+date,dl)
summary(m)

#add regression stats to a ggplot object
add_regr_stats <- function(gg,show=c("rsq","slope")){
  x <- gg$data %>% pull(str_remove_all(as.character(gg$mapping)[1],"~"))
  y <- gg$data %>% pull(str_remove_all(as.character(gg$mapping)[2],"~"))
  regr <- lm(y~x)
  xpos= max(x/2,na.rm = TRUE)
  ypos= max(y/2,na.rm = TRUE)
  if("rsq" %in% show) {
    rsq = round(cor(x,y,use="na"),3)
    gg <- gg + annotate(geom = "text",label=paste0("R-Sq = ",rsq),x=xpos,y=ypos)
  }
  if("slope" %in% show){
    slope = round(regr$coefficients[2],3)
    gg <- gg + annotate(geom = "text",label=paste0("Slope = ",slope),x=xpos,y=ypos+10)
  }
  return(gg)
}

devtools::source_gist("524eade46135f6348140")


