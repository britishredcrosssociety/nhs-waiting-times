library(tidyverse)
library(ggfittext)
library(tidytext)

region_waits <- read_csv("analysis/nhs-waiting-times/waiting lists for NHS Regions.csv")
wales_waits <- read_csv("analysis/nhs-waiting-times/waiting lists for Wales.csv")
sco_waits <- read_csv("analysis/nhs-waiting-times/waiting lists for Scotland.csv")
ni_waits <- read_csv("analysis/nhs-waiting-times/waiting lists for NI.csv")

# ---- Plot regional waiting times ----
# Total numbers of people waiting more than a year in each region
region_waits %>% 
  filter(Year == 2020 & Month == "Dec" & `Treatment Function Name` == "Total") %>% 
  
  # Manually include stats from devolved nations
  add_row(NHSER20NM = "Wales", `Total waiting > 52 weeks` = 94827) %>%  # source: https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Hospital-Waiting-Times/Referral-to-Treatment/patientpathwayswaitingtostarttreatment-by-month-groupedweeks
  add_row(NHSER20NM = "Scotland", `Total waiting > 52 weeks` = NA) %>%  # >52 week wait data not published by NHS Scotland
  add_row(NHSER20NM = "Northern Ireland", `Total waiting > 52 weeks` = 58521) %>%  # see `download waiting list data - NI.R`
  
  ggplot(aes(x = reorder(NHSER20NM, `Total waiting > 52 weeks`, sum), y = `Total waiting > 52 weeks`)) +
  geom_col() +
  geom_text(aes(label = scales::comma(`Total waiting > 52 weeks`)), hjust = 1.3, colour = "white") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, position = "right") +
  labs(caption = "Source: I&I analysis of NHSE data",
       x = NULL, y = "Number of people waiting more than a year") +
  theme_classic() +
  theme(
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("analysis/nhs-waiting-times/NHS waiting times by region - more than a year.png", height = 100, width = 120, units = "mm")

# Number of people waiting > 18 weeks
region_waits %>% 
  filter(Year == 2020 & Month == "Dec" & `Treatment Function Name` == "Total") %>% 
  
  # Manually include stats from devolved nations
  add_row(NHSER20NM = "Wales", `Total waiting > 18 weeks` = 257620) %>%  # source: https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Hospital-Waiting-Times/Referral-to-Treatment/patientpathwayswaitingtostarttreatment-by-month-groupedweeks
  add_row(NHSER20NM = "Scotland", `Total waiting > 18 weeks` = 14684) %>%  # source: https://beta.isdscotland.org/find-publications-and-data/healthcare-resources/waiting-times/nhs-waiting-times-18-weeks-referral-to-treatment/
  add_row(NHSER20NM = "Northern Ireland", `Total waiting > 18 weeks` = 40160) %>%  # see `download waiting list data - NI.R`
  
  ggplot(aes(x = reorder(NHSER20NM, `Total waiting > 18 weeks`, sum), y = `Total waiting > 18 weeks`)) +
  geom_col() +
  geom_bar_text(aes(label = scales::comma(`Total waiting > 18 weeks`))) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, position = "right") +
  labs(caption = "Source: I&I analysis of NHSE data",
       x = NULL, y = "Number of people waiting more than 18 weeks") +
  theme_classic() +
  theme(
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("analysis/nhs-waiting-times/NHS waiting times by region - more than 18 weeks.png", height = 100, width = 120, units = "mm")

# ---- Plot England waiting list sizes over time ----
region_waits %>% 
  filter(`Treatment Function Name` == "Total") %>% 
  
  group_by(Year, Month) %>% 
  summarise(`Total waiting > 52 weeks` = sum(`Total waiting > 52 weeks`, na.rm = TRUE)) %>% 
  
  mutate(Year = factor(Year),
         Month = factor(Month, levels = month.abb)) %>% 
  
  ggplot(aes(x = Month, y = `Total waiting > 52 weeks`)) +
  geom_line(aes(colour = Year, group = Year)) +
  geom_point(aes(colour = Year)) +
  
  scale_x_discrete(labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL) +
  
  theme_classic()

ggsave("analysis/nhs-waiting-times/NHS waiting list over time - more than 52 weeks.png", height = 100, width = 120, units = "mm")

# ---- Plot regional waiting list sizes over time in England ----
region_waits %>% 
  filter(`Treatment Function Name` == "Total") %>% 
  mutate(Year = factor(Year),
         Month = factor(Month, levels = month.abb)) %>% 
  
  ggplot(aes(x = Month, y = `Total waiting > 52 weeks`)) +
  geom_line(aes(colour = Year, group = Year)) +
  geom_point(aes(colour = Year)) +
  
  facet_wrap(~NHSER20NM, nrow = 2) +
  scale_x_discrete(labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  scale_y_continuous(labels = scales::comma) +
  labs(caption = "Source: I&I analysis of NHSE data",
       x = NULL, y = "Number of people waiting more than a year") +
  theme_classic() +
  theme(legend.position = c(0.85, 0.2))

ggsave("analysis/nhs-waiting-times/NHS waiting list over time by region - more than 52 weeks.png", height = 80, width = 170, units = "mm")

# ---- Plot regional waiting list sizes over time in UK ----
ni_waits_total <- 
  ni_waits %>%
  mutate(Region = "Northern Ireland") %>% 
  group_by(Year, Month, Region) %>% 
  summarise(`Total waiting > 52 weeks` = sum(`Total waiting > 52 weeks`, na.rm = TRUE))

uk_waits <- 
  bind_rows(
    region_waits %>% filter(`Treatment Function Name` == "Total") %>% select(Year, Month, Region = NHSER20NM, `Total waiting > 52 weeks`),
    wales_waits %>% mutate(Region = "Wales") %>% select(Year, Month, Region, `Total waiting > 52 weeks`),
    ni_waits_total %>% select(Year, Month, Region, `Total waiting > 52 weeks`),
    
    # Blank data for Scotland since 52+ week waits data isn't available
    # sco_waits %>% mutate(Region = "Scotland") %>% select(Year, Month, Region, `Total waiting > 52 weeks` = `Total waiting > 18 weeks`),
    expand_grid(Year = c(2019, 2020), Month = month.abb, Region = "Scotland", `Total waiting > 52 weeks` = NA)
  )
 
uk_waits %>% 
  mutate(Year = factor(Year),
         Month = factor(Month, levels = month.abb)) %>% 
  
  ggplot(aes(x = Month, y = `Total waiting > 52 weeks`)) +
  geom_line(aes(colour = Year, group = Year)) +
  geom_point(aes(colour = Year)) +
  
  facet_wrap(~Region, nrow = 2) +
  scale_x_discrete(labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  scale_y_continuous(labels = scales::comma) +
  labs(caption = "Source: I&I analysis of NHSE, NHSW, NHS Scotland and NHS NI data",
       x = NULL, y = "Number of people waiting more than a year", colour = NULL) +
  theme_classic() +
  theme(legend.position = c(0.11, 0.9), legend.direction = "horizontal")

ggsave("analysis/nhs-waiting-times/NHS waiting list over time by region - more than 52 weeks.png", height = 90, width = 205, units = "mm")

# ---- Types of treatment ----
region_waits %>% 
  filter(Year == 2020 & Month == "Dec" & `Treatment Function Name` != "Total") %>%
  
  group_by(`Treatment Function Name`) %>% 
  summarise(`Total waiting > 52 weeks` = sum(`Total waiting > 52 weeks`, na.rm = TRUE)) %>% 
  
  ggplot(aes(x = reorder(`Treatment Function Name`, `Total waiting > 52 weeks`, sum), y = `Total waiting > 52 weeks`)) +
  geom_col() +
  geom_bar_text(aes(label = scales::comma(`Total waiting > 52 weeks`))) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, position = "right") +
  labs(caption = "Source: I&I analysis of NHSE data",
       x = NULL, y = "Number of people waiting more than a year") +
  theme_classic() +
  theme(
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("analysis/nhs-waiting-times/NHS waiting times by type of treatment - more than a year.png", height = 120, width = 120, units = "mm")

# ---- Types of treatment by region ----
# Plot top three treatments in each region
region_waits %>% 
  filter(Year == 2020 & Month == "Dec" & `Treatment Function Name` != "Total") %>%
  
  group_by(NHSER20NM, `Treatment Function Name`) %>% 
  summarise(`Total waiting > 52 weeks` = sum(`Total waiting > 52 weeks`, na.rm = TRUE)) %>% 
  top_n(3, `Total waiting > 52 weeks`) %>%
  
  # ggplot(aes(x = reorder(`Treatment Function Name`, `Total waiting > 52 weeks`, sum), y = `Total waiting > 52 weeks`)) +
  ggplot(aes(x = reorder_within(`Treatment Function Name`, `Total waiting > 52 weeks`, NHSER20NM), y = `Total waiting > 52 weeks`)) +
  
  geom_col() +
  geom_bar_text(aes(label = scales::comma(`Total waiting > 52 weeks`))) +
  coord_flip() +
  facet_wrap(~NHSER20NM, ncol = 2, scales = "free_y") +
  
  scale_x_reordered() +
  scale_y_continuous(labels = scales::comma) +
  labs(caption = "Source: I&I analysis of NHSE data",
       x = NULL, y = "Number of people waiting more than a year") +
  
  theme_classic() +
  theme(
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("analysis/nhs-waiting-times/NHS waiting times by type of treatment by region - more than a year.png", height = 120, width = 170, units = "mm")