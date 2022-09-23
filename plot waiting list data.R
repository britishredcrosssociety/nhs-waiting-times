library(tidyverse)
library(ggfittext)
library(tidytext)
library(demographr)

region_waits <- read_csv("data/waiting lists for NHS Regions.csv")
wales_waits <- read_csv("data/waiting lists for Wales.csv")
sco_waits <- read_csv("data/waiting lists for Scotland.csv")
ni_waits <- read_csv("data/waiting lists for NI.csv")

# ---- Plot regional waiting times ----
# Total numbers of people waiting more than a year in each region
region_waits %>% 
  filter(Year == 2022 & Month == "Jul" & `Treatment Function Name` == "Total") %>% 
  
  # Manually include stats from devolved nations
  add_row(
    NHSER20NM = "Wales", 
    `Total waiting > 52 weeks` = wales_waits %>%
      filter(Year == 2022 & Month == "Jul") %>% 
      pull(`Total waiting > 52 weeks`)
  ) %>%

  add_row(
    NHSER20NM = "Scotland", 
    `Total waiting > 52 weeks` = NA
  ) %>%
  
  add_row(
    NHSER20NM = "Northern Ireland", 
    `Total waiting > 52 weeks` = ni_waits %>% 
      filter(Year == 2022 & Month == "Jun") %>% 
      summarise(Total = sum(`Total waiting > 52 weeks`, na.rm = TRUE)) %>% 
      pull(Total)
  ) %>%
  
  ggplot(aes(x = reorder(NHSER20NM, `Total waiting > 52 weeks`, sum), y = `Total waiting > 52 weeks`)) +
  geom_col() +
  geom_text(aes(label = scales::comma(`Total waiting > 52 weeks`)), hjust = 1.3, colour = "white") +
  annotate("text", x = 10, y = 100000, label = "Data not available for Scotland") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, position = "right") +
  labs(caption = "Source: I&I analysis of NHSE, NHSW, NHS Scotland and NHS NI data",
       x = NULL, y = "Number of people waiting more than a year") +
  theme_classic() +
  theme(
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("charts/NHS waiting list by region - more than a year.png", height = 100, width = 120, units = "mm")

# Number of people waiting > 18 weeks
region_waits %>% 
  filter(Year == 2022 & Month == "Jul" & `Treatment Function Name` == "Total") %>% 
  
  # Manually include stats from devolved nations
  add_row(
    NHSER20NM = "Wales", 
    `Total waiting > 18 weeks` = wales_waits %>%
      filter(Year == 2022 & Month == "Jul") %>% 
      pull(`Total waiting > 18 weeks`)
  ) %>%
  
  add_row(
    NHSER20NM = "Scotland", 
    `Total waiting > 18 weeks` = sco_waits %>%
      filter(Year == 2022 & Month == "Jun") %>% 
      pull(`Total waiting > 18 weeks`)
  ) %>%
  
  add_row(
    NHSER20NM = "Northern Ireland", 
    `Total waiting > 18 weeks` = ni_waits %>% 
      filter(Year == 2022 & Month == "Jun") %>% 
      summarise(Total = sum(`Total waiting > 18 weeks`, na.rm = TRUE)) %>% 
      pull(Total)
  ) %>%
  
  ggplot(aes(x = reorder(NHSER20NM, `Total waiting > 18 weeks`, sum), y = `Total waiting > 18 weeks`)) +
  geom_col() +
  geom_bar_text(aes(label = scales::comma(`Total waiting > 18 weeks`))) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, position = "right") +
  labs(caption = "Source: I&I analysis of NHSE, NHSW, NHS Scotland and NHS NI data",
       x = NULL, y = "Number of people waiting more than 18 weeks") +
  theme_classic() +
  theme(
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("charts/NHS waiting list by region - more than 18 weeks.png", height = 100, width = 120, units = "mm")

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

ggsave("charts/NHS waiting list over time - more than 52 weeks.png", height = 100, width = 120, units = "mm")

# ---- Plot regional waiting list sizes over time in UK: more than a year ----
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
    expand_grid(Year = c(2019, 2020, 2021, 2022), Month = month.abb, Region = "Scotland", `Total waiting > 52 weeks` = NA)
  )

scotland_text <- 
  tibble(
    Year = 2021,
    Month = "Jun",
    Region = "Scotland",
    `Total waiting > 52 weeks` = 125000,
    label = "No data available"
  )
 
uk_waits %>% 
  mutate(Year = factor(Year),
         Month = factor(Month, levels = month.abb)) %>% 
  
  ggplot(aes(x = Month, y = `Total waiting > 52 weeks`)) +
  geom_line(aes(colour = Year, group = Year)) +
  geom_point(aes(colour = Year)) +
  
  geom_text(data = scotland_text, aes(label = label)) +
  
  facet_wrap(~Region, nrow = 2) +
  scale_x_discrete(labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_manual(values = rev(c("#cb181d", "#fb6a4a", "#fcae91", "#fee5d9"))) +
  labs(
    title = "Number of people waiting more than a year for treatment",
    caption = "Source: I&I analysis of NHSE, NHSW, NHS Scotland and NHS NI data",
    x = NULL,
    y = "Number of people waiting more than a year", 
    colour = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.18, 0.95), 
    legend.direction = "horizontal",
    legend.background = element_blank()
  )

ggsave("charts/NHS waiting list over time by region - more than 52 weeks.png", height = 90, width = 205, units = "mm")

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
  )

uk_waits %>% 
  mutate(Year = factor(Year),
         Month = factor(Month, levels = month.abb)) %>% 
  
  ggplot(aes(x = Month, y = `Total waiting > 18 weeks`)) +
  geom_line(aes(colour = Year, group = Year)) +
  geom_point(aes(colour = Year)) +
  
  # geom_text(data = scotland_text, aes(label = label)) +
  
  facet_wrap(~Region, nrow = 2) +
  scale_x_discrete(labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_manual(values = rev(c("#cb181d", "#fb6a4a", "#fcae91", "#fee5d9"))) +
  labs(
    title = "Number of people waiting more than 18 weeks for treatment",
    caption = "Source: I&I analysis of NHSE, NHSW, NHS Scotland and NHS NI data",
    x = NULL,
    y = "Number of people waiting more than 18 weeks", 
    colour = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.18, 0.95), 
    legend.direction = "horizontal",
    legend.background = element_blank()
  )

ggsave("charts/NHS waiting list over time by region - more than 18 weeks.png", height = 90, width = 205, units = "mm")

# ---- Plot national waiting list sizes over time in UK: more than 18 weeks ----
ni_waits_total <- 
  ni_waits %>%
  mutate(Region = "Northern Ireland") %>% 
  group_by(Year, Month, Region) %>% 
  summarise(`Total waiting > 18 weeks` = sum(`Total waiting > 18 weeks`, na.rm = TRUE))

england_waits <- 
  region_waits |> 
  filter(`Treatment Function Name` == "Total") |> 
  group_by(Year, Month) |> 
  summarise(
    `Total waiting > 52 weeks` = sum(`Total waiting > 52 weeks`, na.rm = TRUE),
    `Total waiting > 18 weeks` = sum(`Total waiting > 18 weeks`, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  mutate(Region = "England")

uk_waits <- 
  bind_rows(
    england_waits,
    wales_waits %>% mutate(Region = "Wales") %>% select(Year, Month, Region, `Total waiting > 18 weeks`),
    sco_waits %>% filter(Year >= 2019) %>% mutate(Region = "Scotland") %>% select(Year, Month, Region, `Total waiting > 18 weeks` = `Total waiting > 18 weeks`),
    ni_waits_total %>% select(Year, Month, Region, `Total waiting > 18 weeks`)
  )

plot_uk_waits <- 
  uk_waits %>% 
  mutate(Year = factor(Year),
         Month = factor(Month, levels = month.abb)) %>% 
  
  ggplot(aes(x = Month, y = `Total waiting > 18 weeks`)) +
  geom_line(aes(colour = Year, group = Year)) +
  geom_point(aes(colour = Year)) +
  
  facet_wrap(~Region, nrow = 2, scales = "free_y") +
  scale_x_discrete(labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_manual(values = rev(c("#cb181d", "#fb6a4a", "#fcae91", "#fee5d9"))) +
  labs(
    title = "Number of people waiting more than 18 weeks for treatment",
    caption = "Source: I&I analysis of NHSE, NHSW, NHS Scotland and NHS NI data",
    x = NULL,
    y = "Number of people waiting more than 18 weeks", 
    colour = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "top", 
    legend.direction = "horizontal",
    legend.background = element_blank()
  )

ggsave(
  plot = plot_uk_waits,
  filename = "charts/NHS waiting list over time by nation - more than 18 weeks.png", 
  height = 100, 
  width = 205, 
  units = "mm"
)

# Replot on a log scale
plot_uk_waits + 
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Number of people waiting more than 18 weeks for treatment (log scale)",
    y = "Number of people waiting more than 18 weeks (log scale)", 
  )

ggsave(filename = "charts/NHS waiting list over time by nation - more than 18 weeks - log scale.png", height = 100, width = 205, units = "mm")

# Replot based on waiting lists sizes per capita
uk_waits_per_capita <- 
  uk_waits |> 
  left_join(demographr::population21_country21, by = c("Region" = "country_name")) |> 
  mutate(rtt_rate = `Total waiting > 18 weeks` / population)

uk_waits_per_capita |> 
  mutate(Year = factor(Year),
         Month = factor(Month, levels = month.abb)) %>% 
  
  ggplot(aes(x = Month, y = rtt_rate)) +
  geom_line(aes(colour = Year, group = Year)) +
  geom_point(aes(colour = Year)) +
  
  facet_wrap(~Region, nrow = 2, scales = "free_y") +
  scale_x_discrete(labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  scale_colour_manual(values = rev(c("#cb181d", "#fb6a4a", "#fcae91", "#fee5d9"))) +
  labs(
    title = "Percentage of people waiting more than 18 weeks for treatment",
    caption = "Source: I&I analysis of NHSE, NHSW, NHS Scotland and NHS NI data",
    x = NULL,
    y = NULL,
    colour = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "top", 
    legend.direction = "horizontal",
    legend.background = element_blank(),
    plot.title.position = "plot"
  )

ggsave(filename = "charts/NHS waiting list over time by nation - more than 18 weeks - per capita.png", height = 100, width = 140, units = "mm")

# ---- Types of treatment ----
region_waits %>% 
  filter(Year == 2022 & Month == "Jul" & `Treatment Function Name` != "Total") %>%
  
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

ggsave("charts/NHS waiting list by type of treatment - more than a year.png", height = 120, width = 120, units = "mm")

# ---- Types of treatment by region ----
# Plot top three treatments in each region
region_waits %>% 
  filter(Year == 2022 & Month == "Jul" & `Treatment Function Name` != "Total") %>%
  
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

ggsave("charts/NHS waiting list by type of treatment by region - more than a year.png", height = 120, width = 170, units = "mm")
