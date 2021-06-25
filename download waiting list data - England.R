library(tidyverse)
library(lubridate)
library(arrow)
library(httr)

# ---- Lookup table for matching old/new NHS Regions ----
nhs_region_lookup <- tribble(
  ~`Provider Parent Name`, ~NHSER20NM,
  "NHS ENGLAND LONDON", "London",
  "NHS ENGLAND NORTH EAST AND YORKSHIRE (YORKSHIRE AND HUMBER)", "North East and Yorkshire",
  "NHS ENGLAND NORTH EAST AND YORKSHIRE (CUMBRIA AND NORTH EAST)", "North East and Yorkshire",
  "NHS ENGLAND NORTH WEST (CHESHIRE AND MERSEYSIDE)", "North West",
  "NHS ENGLAND MIDLANDS (NORTH MIDLANDS)", "Midlands",
  "NHS ENGLAND MIDLANDS (WEST MIDLANDS)", "Midlands",
  "NHS ENGLAND MIDLANDS (CENTRAL MIDLANDS)", "Midlands",
  "NHS ENGLAND EAST OF ENGLAND (EAST)", "East of England",
  "NHS ENGLAND NORTH WEST (GREATER MANCHESTER)", "North West",
  "NHS ENGLAND NORTH WEST (LANCASHIRE AND SOUTH CUMBRIA)", "North West",
  "NHS ENGLAND SOUTH WEST (SOUTH WEST SOUTH)", "South West",
  "NHS ENGLAND SOUTH WEST (SOUTH WEST NORTH)", "South West",
  "NHS ENGLAND SOUTH EAST (HAMPSHIRE, ISLE OF WIGHT AND THAMES VALLEY)", "South East",
  "NHS ENGLAND SOUTH EAST (KENT, SURREY AND SUSSEX)", "South East",
  
  "NHS ENGLAND NORTH (YORKSHIRE AND HUMBER)", "North East and Yorkshire",
  "NHS ENGLAND NORTH (CUMBRIA AND NORTH EAST)", "North East and Yorkshire",
  "NHS ENGLAND NORTH (CHESHIRE AND MERSEYSIDE)", "North West",
  "NHS ENGLAND MIDLANDS AND EAST (NORTH MIDLANDS)", "Midlands",
  "NHS ENGLAND MIDLANDS AND EAST (WEST MIDLANDS)", "Midlands",
  "NHS ENGLAND MIDLANDS AND EAST (CENTRAL MIDLANDS)", "Midlands",
  "NHS ENGLAND MIDLANDS AND EAST (EAST)", "East of England",
  "NHS ENGLAND NORTH (GREATER MANCHESTER)", "North West",
  "NHS ENGLAND NORTH (LANCASHIRE AND SOUTH CUMBRIA)", "North West",
  "NHS ENGLAND SOUTH WEST (SOUTH WEST SOUTH)", "South West",
  "NHS ENGLAND SOUTH WEST (SOUTH WEST NORTH)", "South West",
  "NHS ENGLAND SOUTH EAST (HAMPSHIRE, ISLE OF WIGHT AND THAMES VALLEY)", "South East",
  "NHS ENGLAND SOUTH EAST (KENT, SURREY AND SUSSEX)", "South East"
)


# ---- Download waiting list data ----
# URLs for full waiting list data by month from https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times
urls <- c(
  apr_21 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/06/Full-CSV-data-file-Apr21-ZIP-3110K-54792.zip",
  mar_21 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/Full-CSV-data-file-Mar21-ZIP-2888K-76325.zip",
  feb_21 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/Full-CSV-data-file-Feb21-ZIP-2739K-25692.zip",
  jan_21 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/03/Full-CSV-data-file-Jan21-ZIP-2714K-24158.zip",
  
  dec_20 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/02/Full-CSV-data-file-Dec20-ZIP-2705K-98040.zip",
  nov_20 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/01/Full-CSV-data-file-Nov20-ZIP-2758K-26885.zip",
  oct_20 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/12/Full-CSV-data-file-Oct20-ZIP-2770K-71733.zip",
  sep_20 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/Full-CSV-data-file-Sep20-ZIP-2738K-20720.zip",
  aug_20 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/10/Full-CSV-data-file-Aug20-ZIP-2594K-09869.zip",
  jul_20 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/09/Full-CSV-data-file-Jul20-ZIP-2546K.zip",
  jun_20 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/08/Full-CSV-data-file-Jun20-ZIP-2380K-84459.zip",
  may_20 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/07/Full-CSV-data-file-May20-ZIP-2218K-43296.zip",
  apr_20 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/06/Full-CSV-data-file-Apr20-ZIP-2171K-195371.zip",
  mar_20 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/05/Full-CSV-data-file-Mar20-ZIP-2995K-73640.zip",
  feb_20 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/04/Full-CSV-data-file-Feb20-ZIP-3174K-20005-1.zip",
  jan_20 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/03/Full-CSV-data-file-Jan20-ZIP-3588K-22427.zip",

  dec_19 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/02/Full-CSV-data-file-Dec19-ZIP-3455K-41699.zip",
  nov_19 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Full-CSV-data-file-Nov19-ZIP-3530K-98977.zip",
  oct_19 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/12/Full-CSV-data-file-Oct19-ZIP-3583K-73980.zip",
  sep_19 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/11/Full-CSV-data-file-Sep19-ZIP-3532K-62303.zip",
  aug_19 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Full-CSV-data-file-Aug19-ZIP-3493K-revised.zip",
  jul_19 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Full-CSV-data-file-Jul19-ZIP-3550K-revised.zip",
  jun_19 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Full-CSV-data-file-Jun19-ZIP-3502K.zip",
  may_19 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Full-CSV-data-file-May19-ZIP-3497K-revised.zip",
  apr_19 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Full-CSV-data-file-Apr19-ZIP-3436K-revised.zip",
  mar_19 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/07/Full-CSV-data-file-Mar19-revised-ZIP-3506K.zip",
  feb_19 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/07/Full-CSV-data-file-Feb19-revised-ZIP-3485K.zip",
  jan_19 = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/07/Full-CSV-data-file-Jan19-revised-ZIP-3634K.zip"
)

td <- tempdir()

for (url in urls) {
  GET(url, write_disk(tf <- tempfile(fileext = ".zip")))
  unzip(tf, exdir = td)
  unlink(tf)
}

# list.files(td)

# Sustainability Transformation Partnerships and NHS England (Region) (April 2020) Lookup in England
# Source: https://geoportal.statistics.gov.uk/datasets/sustainability-transformation-partnerships-and-nhs-england-region-april-2020-lookup-in-england/
stp_region <- read_csv("https://opendata.arcgis.com/datasets/00613813dd4b4f2dba16268720b50bd4_0.csv")

# ---- Load waiting list data into separate dataframes ----
# Set up empty dataframes
stp_waits <- tibble()
region_waits <- tibble()

# Debugging:
# file <- list.files(td, pattern = "*.csv", full.names = TRUE)[11]

for (file in list.files(td, pattern = "*.csv", full.names = TRUE)) {
  d <- read_csv(file)
  
  # Add date columns
  d <- 
    d %>% 
    mutate(Date = dmy(str_replace(Period, "RTT", "01")),
           Month = month.abb[month(Date)],
           Year = year(Date))
  
  # Data from April 2020 onward contains STPs/ICSs
  if (d$Date[1] >= dmy("01-04-2020")) {
    # Calculate STP/ICS totals
    d_stp <- 
      d %>% 
      mutate(`Total waiting > 18 weeks` = rowSums(across(`Gt 18 To 19 Weeks SUM 1`:`Gt 52 Weeks SUM 1`), na.rm = TRUE)) %>% 
      
      group_by(Year, Month, `Provider Parent Org Code`, `Provider Parent Name`, `Treatment Function Name`) %>% 
      summarise(`Total waiting > 52 weeks` = sum(`Gt 52 Weeks SUM 1`, na.rm = TRUE),
                `Total waiting > 18 weeks` = sum(`Total waiting > 18 weeks`, na.rm = TRUE))
    
    # Bind to main STP dataframe
    stp_waits <- bind_rows(stp_waits, d_stp)
    
    # Current data contains STPs/ICSs, so merge in NHS Regions
    d <- 
      d %>% 
      left_join(stp_region, by = c("Provider Parent Org Code" = "STP20CDH"))
    
  } else {
    
    # Data before April 2020 already contains NHS Regions, so use lookup table at the top of this script to sanitise the names
    d <- 
      d %>% 
      left_join(nhs_region_lookup, by = c("Provider Parent Name"))
    
      # mutate(NHSER20NM = str_remove(`Provider Parent Name`, "NHS ENGLAND "),
      #        NHSER20NM = str_remove(NHSER20NM, " \\([A-Z\\s,]+\\)"),
      #        NHSER20NM = str_to_title(NHSER20NM))
  }
  
  d_region <- 
    d %>% 
    mutate(`Total waiting > 18 weeks` = rowSums(across(`Gt 18 To 19 Weeks SUM 1`:`Gt 52 Weeks SUM 1`), na.rm = TRUE)) %>% 
    
    group_by(Year, Month, NHSER20NM, `Treatment Function Name`) %>% 
    summarise(`Total waiting > 52 weeks` = sum(`Gt 52 Weeks SUM 1`, na.rm = TRUE),
              `Total waiting > 18 weeks` = sum(`Total waiting > 18 weeks`, na.rm = TRUE))
  
  # Add regional data to main dataframe
  region_waits <- bind_rows(region_waits, d_region)
  
  print(paste0("Finished ", d$Month[1], " ", d$Year[1]))
}

# Convert months to factors so they plot in the right order
stp_waits <- 
  stp_waits %>% 
  mutate(Month = factor(Month, levels = month.abb))

region_waits <- 
  region_waits %>% 
  mutate(Month = factor(Month, levels = month.abb))

# Save
write_csv(stp_waits, "data/waiting lists for STPs.csv")
write_csv(region_waits, "data/waiting lists for NHS Regions.csv")

# ---- Load all waiting list data into a single dataframe ----
# (Commenting out because it takes too much memory)
# waits <-
#   list.files(td, pattern = "*.csv", full.names = TRUE) %>% 
#   map_df(~read_csv(.))
# 
# # Merge regions into waiting list data
# waits <- 
#   waits %>% 
#   left_join(stp_region, by = c("Provider Parent Org Code" = "STP20CDH"))
# 
# # Make date columns
# waits <- 
#   waits %>% 
#   mutate(Date = dmy(str_replace(Period, "RTT", "01")),
#          Month = month.abb[month(Date)],
#          Year = year(Date))
# 
# write_feather(waits, "analysis/nhs-waiting-times/waits.feather")
