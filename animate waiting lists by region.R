library(tidyverse)
library(ggfittext)
library(tidytext)
library(gganimate)
library(lubridate)

region_waits <- read_csv("data/waiting lists for NHS Regions.csv")
wales_waits <- read_csv("data/waiting lists for Wales.csv")
sco_waits <- read_csv("data/waiting lists for Scotland.csv")
ni_waits <- read_csv("data/waiting lists for NI.csv")

# ---- Plot regional waiting list sizes over time in UK: more than 18 weeks ----
ni_waits_total <- 
  ni_waits %>%
  mutate(Region = "Northern Ireland") %>% 
  group_by(Year, Month, Region) %>% 
  summarise(`Total waiting > 18 weeks` = sum(`Total waiting > 18 weeks`, na.rm = TRUE))

uk_waits <- 
  bind_rows(
    region_waits %>% filter(`Treatment Function Name` == "Total") %>% select(Year, Month, Region = NHSER20NM, `Total waiting > 18 weeks`),
    wales_waits %>% mutate(Region = "Wales") %>% select(Year, Month, Region, `Total waiting > 18 weeks`),
    sco_waits %>% filter(Year >= 2019) %>% mutate(Region = "Scotland") %>% select(Year, Month, Region, `Total waiting > 18 weeks` = `Total waiting > 18 weeks`),
    ni_waits_total %>% select(Year, Month, Region, `Total waiting > 18 weeks`)
  ) |> 
  mutate(Date = ym(paste0(Year, Month))) |> 
  
  mutate(Year = factor(Year),
         Month = factor(Month, levels = month.abb))

plt_waits <- 
  uk_waits %>% 
  ggplot(aes(x = Month, y = `Total waiting > 18 weeks`)) +
  geom_line(aes(colour = Year, group = Year)) +
  geom_point(aes(colour = Year)) +
  
  # geom_text(data = scotland_text, aes(label = label)) +
  
  facet_wrap(~Region, nrow = 2) +
  scale_x_discrete(labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_manual(values = rev(c("#a50f15", "#ef3b2c", "#fc9272"))) +
  labs(
    title = "Number of people waiting more than 18 weeks for treatment",
    caption = "Source: I&I analysis of NHSE, NHSW, NHS Scotland and NHS NI data",
    x = NULL,
    y = "Number of people waiting more than 18 weeks", 
    colour = NULL
  ) +
  transition_reveal(Date) + 
  theme_classic() +
  theme(
    legend.position = c(0.14, 0.95), 
    legend.direction = "horizontal",
    legend.background = element_blank()
  )

animate(
  plt_waits,
  width = 205, height = 90, units = "mm", res = 200,
  end_pause = 10
)

anim_save("charts/NHS waiting list by region - more than 18 weeks.gif")
