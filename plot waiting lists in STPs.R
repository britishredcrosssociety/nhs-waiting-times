##
## Generic function to plot waiting lists in particular ICSs
##
library(tidyverse)
library(geographr)

# ---- Compare STPs as of latest month ----
#' Plot waiting list sizes where people waited over a year for treatment in each STP/ICS
#' @param year The year to plot
#' @param month The month to plot
#' @param highlight_stp String containing the name (or a word in the name) of an STP to highlight, otherwise NULL
#' @param population_percentage If TRUE, show waiting list size as a proportion of the STP/ICSs population, otherwise show absolute waiting list size
plot_waits_latest_month <- function(
    year = 2021,
    month = "Feb",
    highlight_stp = NULL,
    population_percentage = TRUE
) {
  stp_waits <- read_csv("analysis/nhs-waiting-times/waiting lists for STPs.csv")
  
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
  
  # ---- Compare STPs/ICSs ----
  stp_waits <- 
    stp_waits %>% 
    filter(Year == 2021 & Month == "Feb" & `Treatment Function Name` == "Total") %>% 
    
    left_join(stp_names, by = c("Provider Parent Org Code" = "STP20CDH")) %>% 
    left_join(population_stp, by = c("STP20CD" = "stp_code")) %>% 
    
    mutate(proportion_waiting = `Total waiting > 52 weeks` / total_population) %>% 
    
    mutate(value_to_plot = ifelse(rep(population_percentage, n()), proportion_waiting, `Total waiting > 52 weeks`))
  
  if (is_null(highlight_stp)) {
    stp_waits <- 
      stp_waits %>% 
      mutate(bar_label = NA,
             bar_highlight = "no")
    
  } else {
    stp_waits <- 
      stp_waits %>% 
      mutate(bar_label = ifelse(str_detect(STP20NM, highlight_stp), proportion_waiting, NA),
             bar_highlight = ifelse(str_detect(STP20NM, highlight_stp), "yes", "no"))
  }
  
  plt_output <- 
    stp_waits %>% 
    ggplot(aes(x = reorder(STP20NM, value_to_plot, sum), y = value_to_plot)) +
    geom_col(aes(fill = bar_highlight))
  
  if (population_percentage) {
    plt_output <- 
      plt_output +
      geom_text(aes(label = scales::percent(bar_label)), hjust = 1.3, colour = "white") +
      scale_y_continuous(labels = scales::percent, position = "right")
    
  } else {
    plt_output <- 
      plt_output +
      geom_text(aes(label = scales::comma(bar_label)), hjust = 1.3, colour = "white") +
      scale_y_continuous(labels = scales::comma, position = "right")
  }
  
  plt_output <- 
    plt_output +
    coord_flip() +
    scale_fill_manual(values = c("yes" = "#D0021B", "no" = "#5C747A"), guide = FALSE) +
    labs(caption = "Source: British Red Cross analysis of NHSE data as of February 2021",
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

ggsave("analysis/nhs-waiting-times/West Yorkshire and Harrogate waiting times - more than a year - proportion.png", height = 150, width = 350, units = "mm")
