library(tidyverse)
library(ggfittext)

stp_waits <- read_csv("data/waiting lists for STPs.csv")

# Sustainability and Transformation Partnerships (April 2020) Names and Codes in England
# Source: https://geoportal.statistics.gov.uk/datasets/sustainability-and-transformation-partnerships-april-2020-names-and-codes-in-england/data
stp_names <- read_csv("https://opendata.arcgis.com/datasets/08d5070b4f324560aeef857e26701f77_0.csv")

# ---- Compare STPs as of latest month ----
stp_waits %>% 
  # filter(str_detect(`Provider Parent Name`, "MANCHESTER")) %>% 
  filter(Year == 2020 & Month == "Nov" & `Treatment Function Name` == "Total") %>% 
  
  left_join(stp_names, by = c("Provider Parent Org Code" = "STP20CDH")) %>% 
  
  mutate(bar_label = ifelse(str_detect(`Provider Parent Name`, "MANCHESTER"), `Total waiting > 52 weeks`, NA),
         bar_highlight = ifelse(str_detect(`Provider Parent Name`, "MANCHESTER"), "yes", "no")) %>% 
  
  # mutate(`Total waiting > 52 weeks` = as.integer(`Total waiting > 52 weeks`)) %>% 
  
  ggplot(aes(x = reorder(STP20NM, `Total waiting > 52 weeks`, sum), y = `Total waiting > 52 weeks`)) +
  geom_col(aes(fill = bar_highlight)) +
  geom_text(aes(label = scales::comma(bar_label)), hjust = 1.3, colour = "white") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, position = "right") +
  scale_fill_manual(values = c("yes" = "#D0021B", "no" = "#5C747A"), guide = FALSE) +
  labs(title = "Greater Manchester has the highest number of people waiting more than a year for treatment",
       caption = "Source: British Red Cross analysis of NHSE data as of November 2020",
       x = NULL, y = NULL) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.2))
  )

ggsave("charts/Greater Manchester waiting times - more than a year.png", height = 150, width = 300, units = "mm")

# ---- Plot Greater Manchester waiting list over time ----
stp_waits %>% 
  filter(`Treatment Function Name` == "Total") %>% 
  mutate(Year = factor(Year),
         Month = factor(Month, levels = month.abb)) %>% 
  
  ggplot(aes(x = Month, y = `Total waiting > 52 weeks`, group = `Provider Parent Org Code`)) +
  geom_line(colour = "grey", alpha = 0.7) +
  
  geom_line(data = stp_waits %>% filter(str_detect(`Provider Parent Name`, "MANCHESTER") & `Treatment Function Name` == "Total"),
            colour = "red") +
  
  # scale_x_discrete(labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  scale_y_continuous(labels = scales::comma) +
  
  labs(title = "Greater Manchester had the longest waiting list and fastest growth rate in 2020",
       subtitle = "Red line shows Greater Manchester Health and Social Care Partnership\nGrey lines show other STPs/ICSs",
       caption = "Source: British Red Cross analysis of NHSE data",
       x = NULL, y = "Number of people waiting more than a year") +
  theme_classic()

ggsave("charts/Greater Manchester waiting list over time - more than 52 weeks.png", height = 100, width = 200, units = "mm")
