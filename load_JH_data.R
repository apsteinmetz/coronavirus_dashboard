# Grab Johns Hopkins data from local repo into a tibble
library(tidyverse)
# assumes you have PULLed the most recent data from the Johns Hopkins repo
# using the three CSV files you will find here
#https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
date_regex <- "^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}$"
git_dir <- "~\\GitHub\\COVID-19\\csse_covid_19_data\\csse_covid_19_time_series\\time_series_19-covid-"
types <- as.factor(c("Confirmed","Deaths","Recovered"))

cv_raw <- types %>% purrr::map(function(type_name){
  read_csv(paste0(git_dir,type_name,".csv")) %>% 
    mutate(type = type_name)
}) %>% 
  bind_rows() %>% 
  pivot_longer(cols = matches(date_regex),names_to = "date" , values_to = "cases")

cv <- cv_raw %>% rename(country.region = `Country/Region`,province.state = `Province/State`)
cv$`country.region` %>% unique()

cv_mod <- cv %>%   
  mutate(region = country.region) %>% 
  dplyr::mutate(region = dplyr::if_else(country.region == "United Arab Emirates", "UAE", region)) %>%
  dplyr::mutate(region = dplyr::if_else(country.region == "China" & province.state != "Hubei", "China ex Hubei", region)) %>%
  dplyr::mutate(region = dplyr::if_else(country.region == "China" & province.state == "Hubei", "Hubei, China", region)) %>%
  dplyr::mutate(region = dplyr::if_else(province.state == "Hong Kong", "Hong Kong", region, missing=region)) %>%
  #  dplyr::mutate(region = dplyr::if_else(region == "Republic of Korea", "Korea (South)", region)) %>%
#  dplyr::mutate(region = dplyr::if_else(region == "North Macedonia", "N.Macedonia", region)) %>%
#  dplyr::mutate(region = dplyr::if_else(region == "Iran (Islamic Republic of", "Iran", region)) %>%
#  dplyr::mutate(region = trimws(region)) %>%
  {.}
