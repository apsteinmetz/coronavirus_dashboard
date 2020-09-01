#excess mortality
library(tidyverse)
library(covid19nytimes)
xsd <- read_csv("data/Excess_Deaths_Associated_with_COVID-19.csv")
View(xsd)

agg_xsd <- xsd %>% rename(date = `Week Ending Date`) %>%
  group_by(date) %>% 
  filter(State == 'United States') %>% 
  filter(Type == 'Predicted (weighted)',Outcome == "All causes") %>% 	
  transmute(date = date,
            expected_deaths=`Average Expected Count`,
            actual_deaths = `Observed Number`) %>%
  mutate(excess_deaths = actual_deaths - expected_deaths) %>% 
  unique()
{.}


us_states_long <- covid19nytimes::refresh_covid19nytimes_states() %>% as_tibble()
agg_cvd <- us_states_long %>% group_by(date) %>% 
  filter(data_type == "deaths_total") %>% 
  summarise(deaths = sum(value)) %>% 
  mutate(covid_deaths = (deaths - lag(deaths,7)))

agg <- full_join(agg_xsd,agg_cvd) %>%
  filter(!is.na(actual_deaths)) %>% 
  select(-deaths) %>% 
  pivot_longer(cols=contains("deaths"),names_to="Type",values_to="weekly_deaths")

agg %>% filter(str_detect(Type,"actual|expected")) %>%  
  ggplot(aes(date,weekly_deaths,color=Type)) + geom_line() +
  scale_y_continuous(labels=scales::comma) +
  labs(title= "U.S. Mortality From All Causes",caption = "source: CDC")

agg %>% filter(str_detect(Type,"excess|covid")) %>%  
  filter(date > as.Date("2020-01-01")) %>% 
  ggplot(aes(date,weekly_deaths,color=Type)) + geom_line() +
  scale_y_continuous(labels=scales::comma) +
  geom_hline(yintercept = 0) +
  labs(title= "U.S. Excess Mortality and \nReported COVID Deaths",
       caption = "source: CDC, Johns Hopkins")
