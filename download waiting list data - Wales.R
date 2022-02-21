library(tidyverse)
library(lubridate)

# Manually downloaded from https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Hospital-Waiting-Times/Referral-to-Treatment/patientpathwayswaitingtostarttreatment-by-month-groupedweeks
wales_raw <- read_csv("data/waiting lists for Wales - raw data - December 2021.csv", skip = 2,
                      col_types = cols(
                        .default = col_double(),
                        X1 = col_character()
                      ))

wales_waits <- 
  wales_raw %>% 
  select(-contains(".")) %>% 
  mutate(Date = dmy(paste0("01-", X1)),
         Month = month.abb[month(Date)],
         Year = year(Date)) %>% 
  
  filter(Year >= 2019) %>% 
  
  mutate(`Total waiting > 52 weeks` = rowSums(across(`Over 53 weeks and up to 57 weeks`:`Over 105 weeks`), na.rm = TRUE),
         `Total waiting > 18 weeks` = rowSums(across(`Over 18 weeks and up to 19 weeks`:`Over 105 weeks`), na.rm = TRUE)) %>% 
  
  select(Year, Month, `Total waiting > 52 weeks`, `Total waiting > 18 weeks`)

write_csv(wales_waits, "data/waiting lists for Wales.csv")
