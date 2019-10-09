## Explore powerlifting dataset. ####
library(tidyverse)
library(here)

ipf_data <- readRDS(file = here::here('data', 'ipf_lifts.rds'))
str(ipf_data)

## Can events last multiple days? Looks like they're all 1-day events. 
ipf_data %>% 
  select(name, meet_name, date) %>% 
  mutate(yearDate = lubridate::year(date)) %>% 
  group_by(name, meet_name, yearDate) %>% 
  summarise(distinctDates = n_distinct(date)) %>% 
  ungroup() %>% 
  arrange(desc(distinctDates))

## How many unique names are there? 
ipf_data %>% 
  select(name) %>% 
  distinct() %>% 
  nrow()

## Which competitors have entered the most events?
ipf_data %>% 
  group_by(name) %>% # nrow()
  summarise(freq = n()) %>% 
  ungroup() %>% 
  arrange(desc(freq))
ipf_data %>% 
  group_by(name) %>% # nrow()
  summarise(freq = n()) %>% 
  ungroup() %>% 
  arrange(desc(freq)) %>% 
  slice(1:20) %>% 
  ggplot(.) +
  geom_bar(aes(name, freq), stat = "identity")
  