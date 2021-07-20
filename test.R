library(tidyverse)
library(waterfalls)
library(lubridate)

rtt_flow <- read_csv("data/waiting list flow for STPs.csv")

rtt_flow_total <- 
  rtt_flow %>% 
  group_by(Year, Month) %>% 
  summarise(
    Completed = sum(Completed),
    Incomplete = sum(Incomplete),
    New = sum(New)
  ) %>% 
  ungroup() %>% 
  
  mutate(Completed = Completed * -1) %>% 
  
  pivot_longer(cols = Completed:New, names_to = "Pathway", values_to = "n_people")

rtt_flow_total %>% 
  mutate(Pathway_num = case_when(
    Pathway == "Completed" ~ 1,
    Pathway == "New" ~ 2,
    Pathway == "Incomplete" ~ 3
  )) %>% 
  
  mutate(Date = ym(paste0(Year, "-", match(Month, month.abb)))) %>% 
  arrange(Date, Pathway_num) %>% 
  
  mutate(index = row_number()) %>% 
  
  group_by(Date) %>% 
  mutate(
    balance = cumsum(n_people)
  ) %>% 
  ungroup()


rtt_flow_total %>% 
  filter(Year == 2021 & Month == "May") %>% 
  
  waterfall(
    values = n_people,
    labels = Pathway
  )
