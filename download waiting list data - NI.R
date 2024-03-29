library(tidyverse)
library(lubridate)

# Inpatient & Day Case waiting times: https://www.health-ni.gov.uk/publications/northern-ireland-waiting-time-statistics-inpatient-and-day-case-waiting-times-june-2022
ni_inpatient <- read_csv("https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-niwts-tables-total-waiting-q1-22-23.csv",
                         col_types = cols(
                             .default = col_double(),
                             `Quarter Ending` = col_character(),
                             `HSCTrust` = col_character(),
                             Specialty = col_character(),
                             `Programme of Care` = col_character()
                           ))

# Statistics by HSC Trust and Outpatients: https://www.health-ni.gov.uk/publications/northern-ireland-waiting-time-statistics-outpatient-waiting-times-june-2022
ni_outpatient <- read_csv("https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-niwts-tables-outpatients-q1-22-23.csv",
                          col_types = cols(
                            .default = col_character(),
                            `Quarter Ending` = col_character(),
                            `HSC Trust` = col_character(),
                            Specialty = col_character(),
                            `Programme Of Care` = col_character()
                          ))

# ---- Wrangle 2019, 2020 and 2021 data ----
ni_outpatient_sum <- 
  ni_outpatient %>% 
  
  # Remove commas from the data columns
  mutate(across(`0 - 6 weeks`:`Total Waiting`, ~as.numeric(str_remove(.x, ",")))) %>% 
  
  mutate(
    Date = dmy(`Quarter Ending`),
    Month = month.abb[month(Date)],
    Year = year(Date)
  ) %>% 
  
  filter(Year >= 2019) %>% 
  
  group_by(Year, Month, Specialty) %>% 
  summarise(
    `Total waiting > 52 weeks` = sum(`>52-65 weeks`, na.rm = TRUE) + sum(`>65-78 weeks`, na.rm = TRUE) + sum(`>78-91 weeks`, na.rm = TRUE) + sum(`>91-104 weeks`) + sum(`>104 weeks`, na.rm = TRUE),
    `Total waiting > 18 weeks` = sum(`>18 weeks`, na.rm = TRUE)
  )

ni_inpatient_sum <- 
  ni_inpatient %>% 
  mutate(
    Date = dmy(`Quarter Ending`),
    Month = month.abb[month(Date)],
    Year = year(Date)
  ) %>% 
  
  filter(Year >= 2019) %>% 
  
  group_by(Year, Month, Specialty) %>% 
  summarise(
    `Total waiting > 52 weeks` = sum(`>52 weeks`, na.rm = TRUE),
    `Total waiting > 21 weeks` = sum(`> 21 - 26 weeks`, na.rm = TRUE) + sum(`> 26-30 weeks`, na.rm = TRUE) + sum(`> 30 weeks`, na.rm = TRUE)
  )

ni_waits <- 
  bind_rows(
    ni_outpatient_sum,
    ni_inpatient_sum %>% rename(`Total waiting > 18 weeks` = `Total waiting > 21 weeks`)
  ) %>% 
  
  group_by(Year, Month, Specialty) %>% 
  summarise(
    `Total waiting > 52 weeks` = sum(`Total waiting > 52 weeks`, na.rm = TRUE),
    `Total waiting > 18 weeks` = sum(`Total waiting > 18 weeks`, na.rm = TRUE)
  )

ni_waits %>% 
  write_csv("data/waiting lists for NI.csv")
