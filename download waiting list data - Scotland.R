library(tidyverse)

# Manually take numbers from the Excel file linked to here: https://beta.isdscotland.org/find-publications-and-data/healthcare-resources/waiting-times/nhs-waiting-times-18-weeks-referral-to-treatment/
tribble(
  ~Year, ~Month, ~`Total waiting > 18 weeks`,
  2020, "Sep", 14684,
  2020, "Aug", 12589,
  2020, "Jul", 9701,
  2020, "Jun", 8463,
  2020, "May", 5353,
  2020, "Apr", 4453,
  2020, "Mar", 13089,
  2020, "Feb", 20329,
  2020, "Jan", 21926,

  2019, "Dec", 17760,
  2019, "Nov", 22353,
  2019, "Oct", 21862,
  2019, "Sep", 21922,
  2019, "Aug", 21072,
  2019, "Jul", 19060,
  2019, "Jun", 19054,
  2019, "May", 20451,
  2019, "Apr", 20194,
  2019, "Mar", 23326,
  2019, "Feb", 21179,
  2019, "Jan", 22782
) %>% 
  write_csv("analysis/nhs-waiting-times/waiting lists for Scotland.csv")
