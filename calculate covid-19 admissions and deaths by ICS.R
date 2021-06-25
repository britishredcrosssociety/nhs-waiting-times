##
## Covid-19 hospital admissions and deaths by ICS
##
library(tidyverse)
library(geographr)
library(lubridate)
library(readxl)
library(httr)

# ---- Hospital admissions ----
# Monthly publication of COVID-19 data
# Source: https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/

# Monthly COVID Publication up to 6 April 2021
GET("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/Covid-Publication-06-04-2021.xlsx",
    write_disk(tf <- tempfile(fileext = ".xlsx")))

admissions_raw <- read_excel(tf, sheet = "Admissions Total", skip = 12)
admissions <- admissions_raw

unlink(tf); rm(tf)

# Convert the date columns to actual dates
admissions_dates <- 
  admissions %>% 
  names() %>% 
  str_match("[0-9]+") %>% 
  na.omit() %>% 
  as.integer() %>% 
  janitor::excel_numeric_to_date() %>% 
  as.character()

names(admissions)[4:length(names(admissions))] <- admissions_dates

admissions_england <- 
  admissions %>% 
  slice(1)

# Remove unneeded rows...
admissions <- 
  admissions %>% 
  # ... national/regional totals
  slice(-c(1:10)) %>% 
  
  # ... providers with no admissions ever
  filter(if_any(where(is.numeric), ~. != 0))

# Calculate monthly totals
#... for England
admissions_england <- 
  admissions_england %>% 
  pivot_longer(where(is.numeric), names_to = "Date", values_to = "Admissions") %>% 
  
  mutate(
    Month = Date %>% ymd %>% month %>% month.abb[.] %>% factor(levels = month.abb),
    Year = Date %>% ymd %>% year
  ) %>% 
  
  group_by(Year, Month, Code, Name) %>% 
  summarise(Admissions = sum(Admissions, na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  mutate(Admissions_cumulative = cumsum(Admissions)) %>% 
  select(-Code)

#... for ICSs
admissions <- 
  admissions %>% 
  pivot_longer(where(is.numeric), names_to = "Date", values_to = "Admissions") %>% 
  
  mutate(
    Month = Date %>% ymd %>% month %>% month.abb[.] %>% factor(levels = month.abb),
    Year = Date %>% ymd %>% year
  ) %>% 
  
  group_by(Year, Month, Code, Name) %>% 
  summarise(Admissions = sum(Admissions, na.rm = TRUE))
  
# Calculate ICS admissions
admissions_ics <- 
  admissions %>% 
  left_join(geographr::lookup_trust_stp, by = c("Code" = "nhs_trust_code")) %>% 
  filter(!is.na(stp_code)) %>% 
  
  group_by(Year, Month, stp_code) %>% 
  summarise(Admissions = sum(Admissions)) %>% 
  
  group_by(stp_code) %>% 
  mutate(Admissions_cumulative = cumsum(Admissions))

# ---- Covid-19 hospital deaths ----
# COVID 19 total announced deaths 08 June 2021
GET("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/06/COVID-19-total-announced-deaths-08-June-2021.xlsx",
    write_disk(tf <- tempfile(fileext = ".xlsx")))

deaths_raw <- read_excel(tf, sheet = "Tab4 Deaths by trust", skip = 15)
deaths <- deaths_raw

# Convert the date columns to actual dates
deaths_dates <- 
  deaths_raw %>% 
  names() %>% 
  str_match("^[0-9]+") %>% 
  na.omit() %>% 
  as.integer() %>% 
  janitor::excel_numeric_to_date() %>% 
  as.character()

names(deaths)[6:(6 + length(deaths_dates) - 1)] <- deaths_dates

deaths_england <- 
  deaths %>% 
  slice(1)

# Remove unneeded rows...
deaths <- 
  deaths %>% 
  # ... national total
  slice(-c(1:2))

# Calculate monthly totals
deaths <- 
  deaths %>% 
  pivot_longer(starts_with("202"), names_to = "Date", values_to = "Deaths") %>% 
  
  mutate(
    Month = Date %>% ymd %>% month %>% month.abb[.] %>% factor(levels = month.abb),
    Year = Date %>% ymd %>% year
  ) %>% 
  
  group_by(Year, Month, Code, Name) %>% 
  summarise(Deaths = sum(Deaths, na.rm = TRUE))

# Calculate ICS deaths
deaths_ics <- 
  deaths %>% 
  left_join(geographr::lookup_trust_stp, by = c("Code" = "nhs_trust_code")) %>% 
  filter(!is.na(stp_code)) %>% 
  
  group_by(Year, Month, stp_code) %>% 
  summarise(Deaths = sum(Deaths, na.rm = TRUE)) %>% 
  
  group_by(stp_code) %>% 
  mutate(Deaths_cumulative = cumsum(Deaths))

# Calculate England deaths
deaths_england <- 
  deaths_england %>% 
  pivot_longer(starts_with("202"), names_to = "Date", values_to = "Deaths") %>% 
  
  mutate(
    Month = Date %>% ymd %>% month %>% month.abb[.] %>% factor(levels = month.abb),
    Year = Date %>% ymd %>% year
  ) %>% 
  
  group_by(Year, Month, Code, Name) %>% 
  summarise(Deaths = sum(Deaths, na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  mutate(Deaths_cumulative = cumsum(Deaths)) %>% 
  select(-Code)

# ---- Save data ----
write_csv(admissions_ics, "data/covid-19-admissions-ics.csv")
write_csv(admissions_england, "data/covid-19-admissions-england.csv")
write_csv(deaths_ics, "data/covid-19-deaths-ics.csv")
write_csv(deaths_england, "data/covid-19-deaths-england.csv")
