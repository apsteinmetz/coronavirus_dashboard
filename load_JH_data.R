# Grab Johns Hopkins data from local repo into a tibble
library(tidyverse)
# assumes you have PULLed the most recent data from the Johns Hopkins repo
# using the three CSV files you will find here
#https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
date_regex <- "^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}$"

# local repo
git_dir <- "~\\GitHub\\COVID-19\\csse_covid_19_data\\csse_covid_19_time_series\\time_series_covid19_"

# direct from github. not working now
#git_dir <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"

fname <- paste0(git_dir,"confirmed_global.csv")

types = c("confirmed","deaths")

cv_raw <- types %>% purrr::map(function(type_name){
  fname <- paste0(git_dir,type_name,"_global.csv")
  print(fname)
  read_csv(fname) %>% 
    mutate(type = type_name)
}) %>% 
  bind_rows() %>% 
  pivot_longer(cols = matches(date_regex),names_to = "date" , values_to = "cases")


cv <- cv_raw %>% rename(Country.Region = `Country/Region`,Province.State = `Province/State`) %>% 
  mutate(date = as.Date(date,"%m/%d/%y"))


cv_mod <- cv %>%   
  filter(!is.na(cases)) %>% 
  mutate(region = Country.Region) %>% 
  mutate(region = if_else(Country.Region == "United Arab Emirates", "UAE", region)) %>%
  mutate(region = if_else(Country.Region == "China" & Province.State != "Hubei", "China ex Hubei", region)) %>%
  mutate(region = if_else(Country.Region == "China" & Province.State == "Hubei", "Hubei, China", region)) %>%
  mutate(region = if_else(Province.State == "Hong Kong", "Hong Kong", region, missing=region)) %>%
  # change US counties to state
  mutate(Province.State = ifelse(str_ends(Province.State,", [A-Z]{2}"),
                                                state.name[match(str_extract(Province.State,"[A-Z]{2}$"),state.abb)],
                                                Province.State)) %>% 
  {.}

cv_us_states <- cv %>% 
  filter(Country.Region == "US") %>%
  mutate(state = ifelse(str_ends(Province.State,", [A-Z]{2}"),
                                 state.name[match(str_extract(Province.State,"[A-Z]{2}$"),state.abb)],
                                 Province.State)) %>% 
  mutate(county = ifelse(str_ends(Province.State,", [A-Z]{2}"),
                         str_extract(Province.State,"^[a-zA-Z]+"),
                         NA)) %>% 
  {.}

