## Explore powerlifting dataset. ####
library(tidyverse)
library(here)

ipf_data <- readRDS(file = here::here('data', 'ipf_lifts.rds'))
str(ipf_data)

## Find date range of events ####
range(ipf_data$date)

## Can events last multiple days? Looks like they're all 1-day events. ####
ipf_data %>% 
  select(name, meet_name, date) %>% 
  mutate(yearDate = lubridate::year(date)) %>% 
  group_by(name, meet_name, yearDate) %>% 
  summarise(distinctDates = n_distinct(date)) %>% 
  ungroup() %>% 
  arrange(desc(distinctDates))

## How many unique names are there? #### 
ipf_data %>% 
  select(name) %>% 
  distinct() %>% 
  nrow()

## Which competitors have entered the most events? ####
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
  geom_bar(aes(reorder(name, -freq), freq), stat = "identity") +
  # theme(axis.text.x = element_text(angle = 90)) _
  theme(axis.text.x=element_text(size=12, angle=90,hjust=0.95,vjust=0.2)) +
  labs(title = "Top 20 Most Frequent Participants",
       subtitle = paste0(format(min(ipf_data$date), "%B %d, %Y"), " - ", format(max(ipf_data$date), "%B %d, %Y")),
       x = "Name",
       y = "Number of Events")

## Repeat but include by sex ####
ipf_data %>% 
  group_by(name, sex) %>% # nrow()
  summarise(freq = n()) %>% 
  arrange(sex, desc(freq)) %>%
  ungroup() %>% 
  group_by(sex) %>% 
  mutate(rowNum = row_number()) %>% 
  ungroup() %>% 
  filter(rowNum <= 20) 
ipf_data %>% 
  group_by(name, sex) %>% # nrow()
  summarise(freq = n()) %>% 
  arrange(sex, desc(freq)) %>%
  ungroup() %>% 
  group_by(sex) %>% 
  mutate(rowNum = row_number()) %>% 
  ungroup() %>% 
  filter(rowNum <= 20) %>% 
  ggplot(.) +
  geom_bar(aes(reorder(name, -freq), freq, fill = sex), stat = "identity") +
  # theme(axis.text.x = element_text(angle = 90)) _
  theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
  labs(title = "Top 20 Most Frequent Participants by Sex",
       subtitle = paste0(format(min(ipf_data$date), "%B %d, %Y"), " - ", format(max(ipf_data$date), "%B %d, %Y")),
       x = "Name",
       y = "Number of Events")

## Use facet instead ####
ipf_data %>% 
  group_by(name, sex) %>% # nrow()
  summarise(freq = n()) %>% 
  arrange(sex, desc(freq)) %>%
  ungroup() %>% 
  group_by(sex) %>% 
  mutate(rowNum = row_number()) %>% 
  ungroup() %>% 
  filter(rowNum <= 20) %>% 
  ggplot(.) +
  geom_bar(aes(reorder(name, -freq), freq, fill = sex), stat = "identity") +
  facet_grid(~sex, scales = "free_x") +
  # theme(axis.text.x = element_text(angle = 90)) _
  theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
  labs(title = "Top 20 Most Frequent Participants by Sex",
       subtitle = paste0(format(min(ipf_data$date), "%B %d, %Y"), " - ", format(max(ipf_data$date), "%B %d, %Y")),
       x = "Name",
       y = "Number of Events")  
