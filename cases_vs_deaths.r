# correlate deaths and cases by state
library(tidyverse)
library(covid19nytimes)
library(timetk)
library(broom)



# source https://github.com/nytimes/covid-19-data.git

us_states_long <- covid19nytimes::refresh_covid19nytimes_states()
#us_states <- read_csv("~/R Projects/covid-19-data/us-states.csv")
#us_states_live <- read_csv("~/R Projects/covid-19-data/live/us-states.csv")

# Create rolling average changes and lags
us_states <- us_states_long %>%
  pivot_wider(names_from="data_type",values_from="value") %>% 
  rename(state=location) %>%
  select(date,state,cases_total,deaths_total) %>%
  mutate(state = as_factor(state)) %>% 
  arrange(state,date) %>% 
  group_by(state) %>%
  #smooth the data with 7 day moving average
  mutate(cases_7day = (cases_total - lag(cases_total,7))/7) %>%
  mutate(deaths_7day = (deaths_total - lag(deaths_total,7))/7) %>%
  {.}
 
us_states <- us_states %>%   
   # create lags from 5 to 25 days
  tk_augment_lags(cases_7day,.lags = 0:30,.names="auto") %>% 
  {.}

# make long form to nest
models <- us_states %>% ungroup %>% 
  pivot_longer(cols = contains("lag"),
               names_to = "lag",
               values_to = "lagged_cases") %>% 
  select(state,date,deaths_7day,lag,lagged_cases) %>% 
  mutate(lag = as.numeric(str_remove(lag,"cases_7day_lag"))) %>% 
  {.}

# make separate tibbles for each regresion
models <- models %>% 
  nest(data=c(date,deaths_7day,lagged_cases))

#Run a linear regression on lagged cases and date vs deaths
models <- models %>% 
  mutate(model = map(data,
                     function(df) 
                       lm(deaths_7day ~ lagged_cases + date,data = df)))


# Add regression coefficient
# get adjusted r squared
models <- models %>% 
  mutate(adj_r = map(model,function(x) glance(x) %>% 
                       pull(adj.r.squared))
         %>% unlist)

models %>%
  ggplot(aes(lag,adj_r)) + geom_line() +
  facet_wrap(~state)


# best fit lag by state
best_fit <- models %>% 
  group_by(state) %>% 
  summarize(adj_r = max(adj_r)) %>% 
  left_join(models)

hist(best_fit$lag)

# plot all states individually
us_states %>% 
  filter(state %in% c("Florida","Texas","California","New York")) %>% 
  #filter(state %in% state.name[1:10]) %>% 
  ggplot(aes(date,cases_total)) + geom_line() +
  facet_wrap(~state,scales = "fixed") +
  #  scale_y_log10() +
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "none") +
  geom_line(aes(y=deaths_total*20),color="red")

# plot entire US together.
us_states %>% 
  filter(state %in% c("Florida","Texas","California","New York")) %>% 
  #filter(state %in% state.name[1:10]) %>% 
  ggplot(aes(date,cases_total)) + geom_line() +
  facet_wrap(~state,scales = "fixed") +
  #  scale_y_log10() +
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "none") +
  geom_line(aes(y=deaths_total*20),color="red")







# ---- old stuff ---------------
us_states %>% 
  filter(state %in% c("Florida","Texas","California","New York")) %>% 
  #filter(state %in% state.name[1:10]) %>% 
  ggplot(aes(date,cases_total)) + geom_line() +
  facet_wrap(~state,scales = "fixed") +
  #  scale_y_log10() +
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "none") +
  geom_line(aes(y=deaths_total*20),color="red")


  
state1 = "Florida"



coeff = 45
us_states %>%  
  #filter(state %in% c("Florida","Texas","California","New York")) %>% 
  filter(state == state1) %>%
  ggplot(aes(date,cases_7day)) + geom_point(color="orange") +
  geom_point(aes(y=deaths_7day_lag17*coeff),color="red") +
  scale_y_continuous(
    name = "Cases",
    sec.axis = sec_axis(deaths_7day_lag17~./coeff,name="Deaths")) +
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
  ggplot(aes(cases_7day,deaths_7day_lag21)) + 
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



