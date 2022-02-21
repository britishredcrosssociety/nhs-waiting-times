library(tidyverse)
library(lubridate)

# Manually take numbers from the "Tabel 1 All RTT Data" worksheet in the Excel file linked to here: https://beta.isdscotland.org/find-publications-and-data/healthcare-resources/waiting-times/nhs-waiting-times-18-weeks-referral-to-treatment/
# tribble(
#   ~Year, ~Month, ~`Total waiting > 18 weeks`,
#   2021, "Mar", 17651,
#   2021, "Feb", 13113,
#   2021, "Jan", 12390,
#   
#   2020, "Dec", 14714,
#   2020, "Nov", 17455,
#   2020, "Oct", 16345,
#   2020, "Sep", 17219,
#   2020, "Aug", 14725,
#   2020, "Jul", 11875,
#   2020, "Jun", 8463,
#   2020, "May", 5353,
#   2020, "Apr", 4453,
#   2020, "Mar", 13089,
#   2020, "Feb", 20329,
#   2020, "Jan", 21926,
# 
#   2019, "Dec", 17760,
#   2019, "Nov", 22353,
#   2019, "Oct", 21862,
#   2019, "Sep", 21922,
#   2019, "Aug", 21072,
#   2019, "Jul", 19060,
#   2019, "Jun", 19054,
#   2019, "May", 20451,
#   2019, "Apr", 20194,
#   2019, "Mar", 23326,
#   2019, "Feb", 21179,
#   2019, "Jan", 22782
# ) %>% 
#   write_csv("data/waiting lists for Scotland.csv")

# Download monthly RTT data by Health Board from https://www.opendata.nhs.scot/dataset/18-weeks-referral-to-treatment
scotland <- read_csv("https://www.opendata.nhs.scot/dataset/aa8b22e8-8a02-484d-a6c8-0a0154a6249d/resource/f2598c24-bf00-4171-b7ef-a469bbacbf6c/download/open_data_18_weeks_rtt_june-2021.csv")

# scotland |> 
#   filter(Month == 202106) |> 
#   filter(HBT == "S92000003") |> 
#   summarise(sum(Over18Weeks, na.rm = TRUE))

scotland_summary <- 
  scotland |> 
  filter(HBT == "S92000003") |>  # national waiting lists
  mutate(
    Year = str_sub(Month, 1, 4) |> as.integer(),
    Month = month.abb[as.integer(str_sub(Month, 5, 6))] 
  ) |> 
  select(Year, Month, `Total waiting > 18 weeks` = Over18Weeks)

scotland_summary |> 
  write_csv("data/waiting lists for Scotland.csv")
