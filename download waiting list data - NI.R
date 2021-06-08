library(tidyverse)
library(lubridate)

# Inpatient waiting times: https://www.health-ni.gov.uk/publications/northern-ireland-waiting-time-statistics-inpatient-and-day-case-waiting-times-september-2020
ni_inpatient <- read_csv("https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-niwts-tables-total-waiting-q2-20-21.csv",
                         col_types = cols(
                             .default = col_double(),
                             `Quarter Ending` = col_character(),
                             HSCTrust = col_character(),
                             Specialty = col_character(),
                             `Programme of Care` = col_character()
                           ))

# Outpatient waiting times: https://www.health-ni.gov.uk/publications/northern-ireland-waiting-time-statistics-outpatient-waiting-times-september-2020
ni_outpatient <- read_csv("https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-niwts-tables-outpatients-q2-20-21.csv",
                          col_types = cols(
                            .default = col_double(),
                            `Quarter Ending` = col_character(),
                            HSCTrust = col_character(),
                            Specialty = col_character(),
                            `Programme of Care` = col_character()
                          ))

# Calculate number of people waiting 52+ weeks 
ni_inpatient %>% 
  filter(`Quarter Ending` == "30-Sep-20") %>% 
  summarise(Total = sum(`>52 weeks`, na.rm = TRUE)) +

ni_outpatient %>% 
  filter(`Quarter Ending` == "30-Sep-2020") %>% 
  summarise(Total = sum(`>52 weeks`, na.rm = TRUE))

# Calculate number of people waiting 18+ weeks (outpatients only - data not available for inpatients)
ni_outpatient %>% 
  filter(`Quarter Ending` == "30-Sep-2020") %>% 
  summarise(Total = sum(`>18-52 weeks`, na.rm = TRUE) + sum(`>52 weeks`, na.rm = TRUE))

# ---- Wrangle 2019 and 2020 data ----
ni_outpatient_sum <- ni_outpatient %>% 
  mutate(Date = dmy(`Quarter Ending`),
         Month = month.abb[month(Date)],
         Year = year(Date)) %>% 
  
  filter(Year >= 2019) %>% 
  
  group_by(Year, Month, Specialty) %>% 
  summarise(`Total waiting > 52 weeks` = sum(`>52 weeks`, na.rm = TRUE),
            `Total waiting > 18 weeks` = sum(`>18-52 weeks`, na.rm = TRUE) + sum(`>52 weeks`, na.rm = TRUE))

ni_inpatient_sum <- ni_inpatient %>% 
  mutate(Date = dmy(`Quarter Ending`),
         Month = month.abb[month(Date)],
         Year = year(Date)) %>% 
  
  filter(Year >= 2019) %>% 
  
  group_by(Year, Month, Specialty) %>% 
  summarise(`Total waiting > 52 weeks` = sum(`>52 weeks`, na.rm = TRUE))

ni_waits <- bind_rows(ni_outpatient_sum, ni_inpatient_sum) %>% 
  group_by(Year, Month, Specialty) %>% 
  summarise(`Total waiting > 52 weeks` = sum(`Total waiting > 52 weeks`, na.rm = TRUE),
            `Total waiting > 18 weeks` = sum(`Total waiting > 18 weeks`, na.rm = TRUE))

write_csv(ni_waits, "data/waiting lists for NI.csv")
