##
## Plot STP/ICS waiting list sizes alongside Covid-19 hospitalisations and deaths
##
library(tidyverse)
library(geographr)
library(lubridate)
library(ggrepel)

# ---- Load data ----
stp_waits <- read_csv("data/waiting lists for STPs.csv")
stp_admissions <- read_csv("data/covid-19-admissions-ics.csv")
stp_deaths <- read_csv("data/covid-19-deaths-ics.csv")

# Sustainability and Transformation Partnerships (April 2020) Names and Codes in England
# Source: https://geoportal.statistics.gov.uk/datasets/sustainability-and-transformation-partnerships-april-2020-names-and-codes-in-england/data
stp_names <- read_csv("https://opendata.arcgis.com/datasets/08d5070b4f324560aeef857e26701f77_0.csv")

# ---- Calculate population estimates in STPs/ICSs ----
population_stp <- 
  geographr::population_ccg %>% 
  left_join(geographr::lookup_ccg_stp, by = "ccg_code") %>% 
  
  group_by(stp_code) %>% 
  summarise(total_population = sum(total_population)) %>% 
  ungroup()

population_rate <- 100000  # calculate rates per 100,000 people

stp_data <- 
  stp_waits %>% 
  filter(`Treatment Function Name` == "Total") %>% 
  
  left_join(stp_names, by = c("Provider Parent Org Code" = "STP20CDH")) %>% 
  left_join(stp_admissions, by = c("Provider Parent Org Code" = "stp_code", "Year" = "Year", "Month" = "Month")) %>% 
  left_join(stp_deaths, by = c("Provider Parent Org Code" = "stp_code", "Year" = "Year", "Month" = "Month")) %>% 
  left_join(population_stp, by = c("STP20CD" = "stp_code")) %>% 
  
  # Calculate population rates
  mutate(
    proportion_waiting = (`Total waiting > 52 weeks` / total_population) * population_rate,
    proportion_admissions = (Admissions_cumulative / total_population) * population_rate,
    proportion_deaths = (Deaths_cumulative / total_population) * population_rate
  ) %>% 
  
  mutate(Date = ymd(glue::glue("{Year}-{Month}-01")))

# Elongate data for plotting
stp_data_plot <- 
  stp_data %>% 
  select(
    Code = `Provider Parent Org Code`,
    STP20NM, 
    Date, 
    `Waiting list size` = proportion_waiting, 
    `Admissions` = proportion_admissions, 
    `Deaths` = proportion_deaths
  ) %>% 
  pivot_longer(cols = -c(1:3), names_to = "Statistic", values_to = "Rate")

# Calculate averages across ICSs
stp_data_averages <- 
  stp_data %>% 
  select(
    Code = `Provider Parent Org Code`,
    STP20NM, 
    Date, 
    `Waiting list size` = proportion_waiting, 
    `Admissions` = proportion_admissions, 
    `Deaths` = proportion_deaths
  ) %>% 
  group_by(Date) %>% 
  summarise(
    `Waiting list size` = mean(`Waiting list size`, na.rm = TRUE), 
    `Admissions` = mean(Admissions, na.rm = TRUE), 
    `Deaths` = mean(Deaths, na.rm = TRUE)
  )

# ---- Find STPs/ICSs with worse-than-average waiting lists and covid statistics
worst_performing_ics <- stp_data_plot %>% 
  pivot_wider(names_from = Statistic, values_from = Rate) %>% 
  
  left_join(stp_data_averages, by = "Date") %>% 
  
  filter(Date == max(Date)) %>% 
  filter(
    `Waiting list size.x` > `Waiting list size.y` &
      Admissions.x > Admissions.y & 
      Deaths.x > Deaths.y
  )

worst_performing_ics$STP20NM

# ---- Plots for each STP/ICS ----
for (curr_stp in unique(stp_data_plot$Code)) {
  stp_name <- 
    stp_names %>% 
    filter(STP20CDH == curr_stp) %>% 
    pull(STP20NM)
  
  stp_data_last <- 
    stp_data_plot %>% 
    filter(Code == curr_stp & Date == max(Date))
    
  plt <- 
    stp_data_plot %>% 
    filter(Code == curr_stp) %>% 
    mutate(label = if_else(Date == max(Date), Statistic, NA_character_)) %>%
    
    ggplot(aes(x = Date, y = Rate, group = Statistic, colour = Statistic)) +
    geom_line(size = 1.5) +
    
    geom_text_repel(aes(label = label), nudge_x = 1.5, nudge_y = 1.5, na.rm = TRUE) +
    
    scale_y_continuous(
      labels = scales::comma,
      expand = c(0, 0)
      # sec.axis = dup_axis(
      #   breaks = stp_data_last$Rate,
      #   labels = stp_data_last$Statistic,
      #   name = NULL
      # )
    ) +
    
    scale_color_brewer(palette = "Set2") +
    
    labs(
      title = stp_name,
      subtitle = paste("Showing waiting list size and Covid-19 hospital admissions/deaths", 
                       "per 100,000 people from April 2020 to March 2021",
                       sep = "\n"),
      caption = "Source: British Red Cross analysis of NHSE data",
      x = NULL, 
      y = "No. people on waiting list / admitted to hospital / died\n(per 100,000 in ICS/STP)",
      colour = NULL
    ) +
    
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.2)),
      axis.line.y.right = element_blank(),
      legend.position = "none"
    )
  
  ggsave(
    plot = plt, 
    filename = glue::glue("charts/ICS charts/{stp_name}.png"),
    width = 200,
    height = 150,
    units = "mm"
  )
  
  print(glue::glue("Plotted {stp_name}"))
}

