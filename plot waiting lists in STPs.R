##
## Generic function to plot waiting lists in particular ICSs
##
library(tidyverse)
library(geographr)
library(ggfittext)
library(patchwork)

# ---- Compare STPs as of latest month ----
#' Plot waiting list sizes where people waited over a year for treatment in each STP/ICS
#' @param year The year to plot
#' @param month The month to plot
#' @param highlight_stp String containing the name (or a word in the name) of an STP to highlight, otherwise NULL
#' @param population_percentage If TRUE, show waiting list size as a proportion of the STP/ICSs population, otherwise show absolute waiting list size
plot_waits_latest_month <- function(
    year = 2022,
    month = "Sep",
    highlight_stp = NULL,
    population_percentage = TRUE
) {
  stp_waits <- read_csv("data/waiting lists for STPs.csv")
  
  # Sustainability and Transformation Partnerships (April 2020) Names and Codes in England
  # Source: https://geoportal.statistics.gov.uk/datasets/sustainability-and-transformation-partnerships-april-2020-names-and-codes-in-england/data
  stp_names <- read_csv("https://opendata.arcgis.com/datasets/08d5070b4f324560aeef857e26701f77_0.csv")
  
  # ---- Calculate population estimates in STPs/ICSs ----
  # population_stp <- 
  #   geographr::population_ccg %>% 
  #   left_join(geographr::lookup_ccg_stp, by = "ccg_code") %>% 
  #   
  #   group_by(stp_code) %>% 
  #   summarise(total_population = sum(total_population)) %>% 
  #   ungroup()
  
  # ---- Compare STPs/ICSs ----
  stp_waits <- 
    stp_waits %>% 
    filter(Year == year & Month == month & `Treatment Function Name` == "Total") %>% 
    
    left_join(stp_names, by = c("Provider Parent Org Code" = "STP20CDH")) %>% 
    # left_join(population_stp, by = c("STP20CD" = "stp_code")) %>% 
    
    # mutate(proportion_waiting = `Total waiting > 52 weeks` / total_population) %>% 
    
    # mutate(value_to_plot = ifelse(rep(population_percentage, n()), proportion_waiting, `Total waiting > 52 weeks`))
    mutate(value_to_plot = `Total waiting > 52 weeks`)
  
  if (is_null(highlight_stp)) {
    stp_waits <- 
      stp_waits %>% 
      mutate(bar_label = value_to_plot,
             bar_highlight = "no")
    
  } else {
    stp_waits <- 
      stp_waits %>% 
      mutate(bar_label = ifelse(str_detect(STP20NM, highlight_stp), value_to_plot, NA),
             bar_highlight = ifelse(str_detect(STP20NM, highlight_stp), "yes", "no"))
  }

  plt_output <- 
    stp_waits %>% 
    ggplot(aes(x = reorder(STP20NM, value_to_plot, sum), y = value_to_plot)) +
    geom_col(aes(fill = bar_highlight))
  
  if (population_percentage) {
    plt_output <- 
      plt_output +
      geom_bar_text(mapping = aes(label = scales::percent(bar_label))) +
      scale_y_continuous(labels = scales::percent, position = "right")
    
  } else {
    plt_output <- 
      plt_output +
      geom_bar_text(aes(label = scales::comma(bar_label))) +
      scale_y_continuous(labels = scales::comma, position = "right")
  }
  
  plt_output <- 
    plt_output +
    coord_flip() +
    scale_fill_manual(values = c("yes" = "#D0021B", "no" = "#5C747A"), guide = FALSE) +
    labs(caption = glue::glue("Source: British Red Cross analysis of NHSE data as of {month} {year}"),
         x = NULL, y = NULL) +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.2))
    )
  
  list(
    plt = plt_output,
    data = stp_waits
  )
}

plt_harrogate <- plot_waits_latest_month(highlight_stp = "Harrogate", population_percentage = TRUE)

mean(plt_harrogate$data$proportion_waiting)

plt_harrogate$plt +
  labs(title = "Percentage of people waiting over a year for treatment in West Yorkshire and Harrogate")

ggsave("charts/West Yorkshire and Harrogate waiting times - more than a year - proportion.png", height = 150, width = 350, units = "mm")

# Plot all ICS/STPs
# - Proportions of population -
ics_prop <- plot_waits_latest_month(year = 2021, month = "Jul", population_percentage = TRUE)
ics_prop$plt + labs(title = "Percentage of people waiting more than a year for treatment")
ggsave("charts/ICS waiting times - more than a year - proportion.png", height = 150, width = 350, units = "mm")

ics_prop$data |> 
  arrange(desc(`Total waiting > 52 weeks`)) |> 
  select(STP20NM, `Total waiting > 52 weeks`, proportion_waiting)

# - Absolute numbers -
ics_num <- plot_waits_latest_month(year = 2022, month = "Sep", population_percentage = FALSE)
ics_num$plt + labs(title = "Number of people waiting more than a year for treatment")
ggsave("charts/ICS waiting times - more than a year - number.png", height = 150, width = 250, units = "mm")

ics_prop$data |> 
  arrange(desc(`Total waiting > 52 weeks`))


# ---- Plot waits for South West region ----
stp_waits <- read_csv("data/waiting lists for STPs.csv")

# Sustainability and Transformation Partnerships (April 2020) Names and Codes in England
# Source: https://geoportal.statistics.gov.uk/datasets/sustainability-and-transformation-partnerships-april-2020-names-and-codes-in-england/data
# stp_names <- read_csv("https://opendata.arcgis.com/datasets/08d5070b4f324560aeef857e26701f77_0.csv")

# Sustainability Transformation Partnerships and NHS England (Region) (April 2020) Lookup in England
# Source: https://geoportal.statistics.gov.uk/datasets/sustainability-transformation-partnerships-and-nhs-england-region-april-2020-lookup-in-england/
stp_region <- read_csv("https://opendata.arcgis.com/datasets/00613813dd4b4f2dba16268720b50bd4_0.csv")

stp_region |> distinct(NHSER20NM)

stp_waits <- 
  stp_waits %>% 
  filter(Year == 2022 & Month == "Sep" & `Treatment Function Name` == "Total") %>% 
  
  # left_join(stp_names, by = c("Provider Parent Org Code" = "STP20CDH")) %>% 
  left_join(stp_region, by = c("Provider Parent Org Code" = "STP20CDH"))

stp_waits |> 
  filter(NHSER20NM == "South West") |> 
  
  ggplot(aes(x = reorder(STP20NM, `Total waiting > 52 weeks`, sum), y = `Total waiting > 52 weeks`)) +
  geom_col(fill = "#5C747A") +
  geom_bar_text(aes(label = scales::comma(`Total waiting > 52 weeks`))) +
  scale_y_continuous(labels = scales::comma, position = "right") +
  coord_flip() +
  labs(
    title = "Number of people in the South West waiting more than a year for treatment",
    caption = "Source: British Red Cross analysis of NHSE data as of September 2022",
    x = NULL, 
    y = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.2)),
    plot.title.position = "plot"
  )

ggsave("charts/South West ICS waiting times - more than a year.png", height = 150, width = 250, units = "mm")

stp_waits |> 
  filter(NHSER20NM == "South West") |> 
  
  ggplot(aes(x = reorder(STP20NM, `Total waiting > 18 weeks`, sum), y = `Total waiting > 18 weeks`)) +
  geom_col(fill = "#5C747A") +
  geom_bar_text(aes(label = scales::comma(`Total waiting > 18 weeks`))) +
  scale_y_continuous(labels = scales::comma, position = "right") +
  coord_flip() +
  labs(
    title = "Number of people in the South West waiting more than 18 weeks for treatment",
    caption = "Source: British Red Cross analysis of NHSE data as of September 2022",
    x = NULL, 
    y = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.2)),
    plot.title.position = "plot"
  )

ggsave("charts/South West ICS waiting times - more than 18 weeks.png", height = 150, width = 250, units = "mm")

stp_waits |> 
  mutate(bar_highlight = if_else(NHSER20NM == "South West", "yes", "no")) |> 
  
  ggplot(aes(x = reorder(STP20NM, `Total waiting > 18 weeks`, sum), y = `Total waiting > 18 weeks`)) +
  geom_col(aes(fill = bar_highlight)) +
  geom_bar_text(aes(label = scales::comma(`Total waiting > 18 weeks`))) +
  scale_y_continuous(labels = scales::comma, position = "right") +
  scale_fill_manual(values = c("yes" = "#D0021B", "no" = "#5C747A"), guide = "none") +
  coord_flip() +
  labs(
    title = "Number of people in the South West waiting more than 18 weeks for treatment",
    caption = "Source: British Red Cross analysis of NHSE data as of September 2022",
    x = NULL, 
    y = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.2)),
    plot.title.position = "plot"
  )

ggsave("charts/ICS waiting times - more than 18 weeks - highlight South West.png", height = 250, width = 250, units = "mm")

stp_waits |> 
  mutate(bar_highlight = if_else(NHSER20NM == "South West", "yes", "no")) |> 
  
  ggplot(aes(x = reorder(STP20NM, `Total waiting > 52 weeks`, sum), y = `Total waiting > 52 weeks`)) +
  geom_col(aes(fill = bar_highlight)) +
  geom_bar_text(aes(label = scales::comma(`Total waiting > 52 weeks`))) +
  scale_y_continuous(labels = scales::comma, position = "right") +
  scale_fill_manual(values = c("yes" = "#D0021B", "no" = "#5C747A"), guide = "none") +
  coord_flip() +
  labs(
    title = "Number of people in the South West waiting more than 52 weeks for treatment",
    caption = "Source: British Red Cross analysis of NHSE data as of September 2022",
    x = NULL, 
    y = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.2)),
    plot.title.position = "plot"
  )

ggsave("charts/ICS waiting times - more than a year - highlight South West.png", height = 250, width = 250, units = "mm")
