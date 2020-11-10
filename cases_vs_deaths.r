# correlate deaths and cases by state
library(tidyverse)
library(covid19nytimes)
library(timetk)
library(lubridate)
library(broom)

# source https://github.com/nytimes/covid-19-data.git
us_states_long <- covid19nytimes::refresh_covid19nytimes_states()

# Create rolling average changes
us_states <- us_states_long %>%
  filter(date > as.Date("2020-03-01")) %>% 
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

#reduce to weekly so there not overlapping days
#us_states_wk <- us_states %>% 
#  filter(wday(date,label = TRUE)  == "Mon")

  
 
# national analysis
# ----------------------------------------------
# aggregate state to national
us <- us_states %>%
  group_by(date) %>% 
  summarize(across(.cols=where(is.double),
                   .fns = function(x)sum(x,na.rm = T),
                   .names="{.col}"))

# does a simple scatterplot tell us anything 
# about the relationship of deaths to cases? No.
us %>% 
  ggplot(aes(deaths_7day,cases_7day)) + geom_point() + geom_smooth()


# arbitrary value that makes to make the secondary axis line up
coeff = 30

us %>% 
  ggplot(aes(date,cases_total)) + geom_line(color="orange") +
  theme(legend.position = "none") +
  geom_line(aes(y=deaths_total*coeff),color="red") +
  scale_y_continuous(labels = scales::comma,
                     name = "Cases",
                     sec.axis = sec_axis(deaths_total~./coeff,
                                         name="Deaths",
                                         labels = scales::comma)) +
   theme(
     axis.title.y = element_text(color = "orange", size=13),
     axis.title.y.right = element_text(color = "red", size=13)
   ) +
   labs(title =  "U.S. Cases vs. Deaths",
        x = "Date")
  
#visualize the relationship between rolling average of weekly scases and deaths
coeff = 30
us %>% 
  ggplot(aes(date,cases_7day)) + geom_line(color="orange") +
  theme(legend.position = "none") +
  geom_line(aes(x=date,y=deaths_7day*coeff),color="red") +
  scale_y_continuous(labels = scales::comma,
                     name = "Cases",
                     sec.axis = sec_axis(deaths_7day~./coeff,
                                         name="Deaths",
                                         labels = scales::comma)) +
  theme(
    axis.title.y = element_text(color = "orange", size=13),
    axis.title.y.right = element_text(color = "red", size=13)
  ) +
  labs(title =  "U.S. Cases vs. Deaths",
       subtitle = "7-Day Average",
       x = "Date")


# passage of time affects deaths more than cases
lm(deaths_7day~cases_7day+date,data=us) %>% summary()

# now make the models

#create columns for deaths led 0 to 30 days ahead
us_lags <- us %>%
  # create lags by day
  tk_augment_lags(deaths_7day,.lags = -60:0,.names="auto") %>% 
  # create lags by week is using weekly data
  # tk_augment_lags(deaths_7day,.lags = -8:0,.names="auto") %>% 
  {.}
# fix names to remove minus sign
names(us_lags) <- names(us_lags) %>% str_replace_all("lag-","lead")

coeff = 30
us_lags %>% 
  ggplot(aes(date,cases_7day)) + geom_line(color="orange") +
  theme(legend.position = "none") +
  geom_line(aes(x=date,y=deaths_7day_lead14*coeff),color="red") +
  scale_y_continuous(labels = scales::comma,
                     name = "Cases",
                     sec.axis = sec_axis(deaths_7day~./coeff,
                                         name="Deaths",
                                         labels = scales::comma)) +
  theme(
    axis.title.y = element_text(color = "orange", size=13),
    axis.title.y.right = element_text(color = "red", size=13)
  ) +
  labs(title =  "U.S. Cases vs. Deaths",
       subtitle = "7-Day Average, lead 14",
       x = "Date")

# make long form to nest
# initialize models data frame
models <- us_lags %>% ungroup %>% 
  pivot_longer(cols = contains("lead"),
               names_to = "lead",
               values_to = "led_deaths") %>% 
  select(date,cases_7day,lead,led_deaths) %>% 
  mutate(lead = as.numeric(str_remove(lead,"deaths_7day_lead"))) %>% 
  {.}

# make separate tibbles for each regression
models <- models %>% 
  nest(data=c(date,cases_7day,led_deaths)) %>% 
  arrange(lead)

#Run a linear regression on lagged cases and date vs deaths

# Polynomial degree for date.
degree = 1

models <- models %>% 
  mutate(model = map(data,
                     function(df) 
                       lm(led_deaths~cases_7day+poly(date,degree),data = df)))


# Add regression coefficient
# get adjusted r squared
models <- models %>% 
  mutate(adj_r = map(model,function(x) glance(x) %>% 
                       pull(adj.r.squared))
         %>% unlist)

# Show model fit by lead time
models %>%
  ggplot(aes(lead,adj_r)) + geom_line() +
  labs(subtitle = paste("Best fit lead =",best_fit$lead,"days"),
       x = "Lead Time in Days for Deaths",
       y= "Adjusted R-squared")

# make predictions using best model
best_fit <- models %>% 
  summarize(adj_r = max(adj_r)) %>% 
  left_join(models)

# ------------------------------------------
# see how well our model predicts
show_predictions <- function(single_model){
  complete_cases <- single_model$data[[1]] %>% 
    remove_missing()
  
  predictions <- enframe(predict(single_model$model[[1]],na.action = exclude),
                         name = NULL,
                         value = paste0("predicted_in_","ahead","_days"))
  
  bind_cols(complete_cases,predictions) %>% 
    rename(cases=cases_7day,
           deaths_actual=led_deaths,
           deaths_predicted=predicted_in_ahead_days) %>%
    select(-cases) %>% 
    pivot_longer(cols = where(is.numeric)) %>% 
    ggplot(aes(date,value,color=name)) + geom_line()
  
}

# best adj_r is not the best fit
show_predictions(best_fit)

# local maximum of 14 days gives  much more satisfactory result
show_predictions(models[14,])

# ------------------------------------------
# state by state analysis
# states hit their peaks (to date) at very different times, making state
# a useful feature. We could use one dummy variable for each state but it
# is simpler with tidymodels to conduct a separate set of models for each state

# illustrate selected states
us_states %>% 
  filter(state %in% c("Florida","Texas","California","New York")) %>% 
  ggplot(aes(date,cases_total)) + geom_line() +
  facet_wrap(~state,scales = "fixed") +
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "none")

us_states %>% 
  filter(state %in% c("Florida","Texas","California","Michigan")) %>% 
  ggplot(aes(date,cases_7day)) + geom_line(color="orange") +
  facet_wrap(~state,scales = "free") +
  theme(legend.position = "none") +
  geom_line(aes(y=deaths_7day*coeff),color="red") +
  scale_y_continuous(labels = scales::comma,
                     name = "Cases",
                     sec.axis = sec_axis(deaths_7day~./coeff,
                                         name="Deaths",
                                         labels = scales::comma)) +
  theme(
    axis.title.y = element_text(color = "orange", size=13),
    axis.title.y.right = element_text(color = "red", size=13)
  ) +
  labs(title =  "U.S. Cases vs. Deaths",
       subtitle = "7-Day Average",
       x = "Date")


# create lags
us_states_lags <- us_states %>%
  # create lags by day
  tk_augment_lags(deaths_7day,.lags = -60:0,.names="auto") %>% 
  # create lags by week is using weekly data
  # tk_augment_lags(deaths_7day,.lags = -8:0,.names="auto") %>% 
  {.}
# fix names to remove minus sign
names(us_states_lags) <- names(us_states_lags) %>% str_replace_all("lag-","lead")

# make long form to nest
# initialize models data frame
models <- us_states_lags %>% ungroup %>% 
  pivot_longer(cols = contains("lead"),
               names_to = "lead",
               values_to = "led_deaths") %>% 
  select(state,date,cases_7day,lead,led_deaths) %>% 
  mutate(lead = as.numeric(str_remove(lead,"deaths_7day_lead"))) %>% 
  {.}

# make separate tibbles for each regression
models <- models %>% 
  nest(data=c(date,cases_7day,led_deaths)) %>% 
  arrange(lead)

#Run a linear regression on lagged cases and date vs deaths
# Polynomial degree for date.
degree = 1

models <- models %>% 
  mutate(model = map(data,
                     function(df) 
                       lm(led_deaths~cases_7day+poly(date,degree),data = df)))


# Add regression coefficient
# get adjusted r squared
models <- models %>% 
  mutate(adj_r = map(model,function(x) glance(x) %>% 
                       pull(adj.r.squared))
         %>% unlist)

models %>%
  ggplot(aes(lead,adj_r)) + geom_line() +
  facet_wrap(~state)


# best fit lag by state
best_fit <- models %>% 
  group_by(state) %>% 
  summarize(adj_r = max(adj_r)) %>% 
  left_join(models)

hist(best_fit$adj_r)
hist(best_fit$lead)

