#scratchpad
library(dplyr)
library(ggplot2)
#devtools::install_github("RamiKrispin/coronavirus")
#library(coronavirus)
#data(coronavirus)
#load("c:/users/Arthur/downloads/coronavirus.rda")

# get johns hopkins data into a tibble called cv_mod
source("load_JH_data.r")

pop <- readr::read_csv("data/demographic-2019-UNFPA.csv") %>% 
  rename(region = `Countries and areas`) %>% 
  rename(population = 'Total population in millions, 2019') %>% 
  select(region,population) %>% 
  mutate(population= population * 1e6)

# choose type
# type_selection ="Deaths"
# case_threshold = 10
# yaxis_type = "log"

countries_of_interest = c("Australia","Belgium","Denmark","France","Germany", "China ex Hubei",
                          "Hong Kong","Iran","Italy","Korea, South","United Kingdom","Singapore","Japan", "US")

countries_of_interest_small = c("France", "China ex Hubei","Iran","Italy",
                                "Korea, South","United Kingdom","Japan", "US","Germany")
# cum_cases %>% 
#   # filter smaller, non-asian and china
#   filter(region %in% countries_of_interest) %>% 
#   ggplot(aes(days_since,cum_cases,color=region)) + 
#   geom_line() + 
#   scale_y_log10() + 
#   NULL

# type_select <- "Deaths"
# countries <- countries_of_interest
# #make cumulative cases
# cum_cases <- cv_mod %>%
#   filter(region == "Germany") %>% 
#   filter(type == type_select) %>% 
#   group_by(region, date) %>% 
#   summarise(cases = sum(cases)) %>% 
# #  mutate(cum_cases = cumsum(cases)) %>%
#   mutate(cum_cases = cases) %>%
#   filter(cum_cases > case_threshold) %>%
#   group_by(region) %>%
#   mutate(days_since = as.numeric(date-min(date))) %>%
#   # left_join(pop) %>%
#   # mutate(cum_cases_per100k = cum_cases/population*100000)
#   {.}
# 
# cum_cases_wide <<- cum_cases %>% 
#   # filter smaller, non-asian and china
#   filter(region %in% countries) %>% 
#   tidyr::pivot_wider(id_cols="days_since",names_from = "region",values_from = "cum_cases")


plot_cum_trend <- function(countries = "US",
                           type_select = "Confirmed",
                           case_threshold = 10,
                           scale = c("log","linear"),
                           max_days = Inf){
  # Plotting the data
  #make cumulative cases
  cum_cases <- cv_mod %>%
    filter(region %in% countries) %>% 
    filter(type == type_select) %>% 
    group_by(region, date) %>% 
    summarise(cases = sum(cases)) %>% 
    #    mutate(cum_cases = cumsum(cases)) %>%
    # cases from JH data are already cumulative
    mutate(cum_cases = cases) %>%
    filter(cum_cases > case_threshold) %>%
    group_by(region) %>%
    mutate(days_since = as.numeric(date-min(date))) %>%
    # left_join(pop) %>%
    # mutate(cum_cases_per100k = cum_cases/population*100000)
    {.}

    cum_cases_wide <- cum_cases %>% 
    # filter smaller, non-asian and china
    tidyr::pivot_wider(id_cols="days_since",names_from = "region",values_from = "cum_cases")
  
  # constrain the chart a bit
  cum_cases_wide <- cum_cases_wide %>% filter(days_since < max_days + 1)
  
  plot_object <- cum_cases_wide %>% 
    plotly::plot_ly(x = ~days_since,y = ~US,type = "scatter",mode="lines+markers",name="US") %>% 
    plotly::layout(yaxis = list(type = yaxis_type)) %>% 
    {.}
  
  countries[which(countries != "US")] %>% 
    purrr::walk( function(region) {
      print(region)
      plot_object <<- plot_object %>%
        plotly::add_trace(x = ~days_since,
                          y =  cum_cases_wide[[region]], 
                          # type = "scatter", 
                          mode = "lines",
                          name = region)
    })
  
  plot_object %>% plotly::layout(title = "",
                                 legend = list(x = 0.9, y = 0.7),
                                 yaxis = list(title = paste0("Cumulative ",type," Cases")),
                                 xaxis = list(title = paste0("Days Since ",case_threshold," ",type)),
                                 # paper_bgcolor = "black",
                                 # plot_bgcolor = "black",
                                 # font = list(color = 'white'),
                                 hovermode = "compare",
                                 margin =  list(
                                   # l = 60,
                                   # r = 40,
                                   b = 10,
                                   t = 10,
                                   pad = 2
                                 ))
  
}

plot_cum_trend(countries_of_interest_small,"Deaths",10)
plot_cum_trend(countries_of_interest,"Confirmed",100)

