library(tidyverse)
library(lubridate)

# Download monthly RTT data by Health Board from https://www.opendata.nhs.scot/dataset/18-weeks-referral-to-treatment
scotland <- read_csv("https://www.opendata.nhs.scot/dataset/aa8b22e8-8a02-484d-a6c8-0a0154a6249d/resource/f2598c24-bf00-4171-b7ef-a469bbacbf6c/download/open_data_18_weeks_rtt_jun22.csv")

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

# Waiting lists by Health Board
scotland_hb <- 
  scotland |> 
  filter(HBT != "S92000003") |>  # Don't include national waiting lists
  mutate(
    Year = str_sub(Month, 1, 4) |> as.integer(),
    Month = month.abb[as.integer(str_sub(Month, 5, 6))] 
  ) |> 
  select(Year, Month, hb19_code = HBT, Specialty, `Total waiting < 18 weeks` = Within18Weeks, `Total waiting > 18 weeks` = Over18Weeks)

scotland_hb |> 
  write_csv("data/waiting lists for Scotland - Health Boards.csv")
