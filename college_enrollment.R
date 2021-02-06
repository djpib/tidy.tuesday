####libraries####
library(tidytuesdayR)
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(extrafont)
library(ggplot2)
library(ggtext)

####get data####
tuesdata <- tidytuesdayR::tt_load(2021, week = 6)
hs_students <- tuesdata$hs_students
bach_students <- tuesdata$bach_students

####tidy data####
hs_students <- hs_students %>% 
  filter(Total != "NA") %>% 
  mutate(Total = if_else(Total > 10000, 
                         str_sub(Total, 1, 4) %>% as.double(), 
                         Total)) %>% 
  rename(year = Total) %>% 
  select(!contains("Standard")) %>% 
  select(!contains("Total")) %>%
  mutate_at(c("White1", 
              "Black1", 
              "Hispanic", 
              "Asian/Pacific Islander - Asian", 
              "Asian/Pacific Islander - Pacific Islander", 
              "American Indian/\r\nAlaska Native", 
              "Two or more race"), 
            as.double) %>% 
  pivot_longer(cols = 2:last_col(), names_to = "group", values_to = "high_school")
  
bach_students <- bach_students %>% 
  rename(year = Total) %>% 
  select(!contains("Standard")) %>% 
  select(!contains("Total")) %>%
  mutate_at(c("White1", 
              "Black1", 
              "Hispanic", 
              "Asian/Pacific Islander - Asian", 
              "Asian/Pacific Islander - Pacific Islander", 
              "American Indian/\r\nAlaska Native", 
              "Two or more race"), 
            as.double) %>% 
  pivot_longer(cols = 2:last_col(), names_to = "group", values_to = "bachelor")

combined_students <- hs_students %>% 
  full_join(bach_students) %>% 
  pivot_longer(cols = high_school:bachelor, names_to = "education", values_to = "percentage") %>%
  mutate(group = if_else(
    group == "White1", "White", if_else(
      group == "Black1", "Black", if_else(
        group == "Hispanic", "Hispanic", if_else(
          group == "Asian/Pacific Islander - Asian", "Asian", if_else(
            group == "Asian/Pacific Islander - Pacific Islander", "Pacific Islander", if_else(
              group == "American Indian/\r\nAlaska Native", "American Indian/\r\nAlaska Native",
              "Two or more races")))))))

####plot####
combined_students %>% filter(year > 1990) %>% 
  ggplot(mapping = aes(x = year,  y = percentage, color = education)) +
  geom_point(size = 2) +
  geom_line() + 
  scale_color_manual(values = c("#1b9e77", "#7570b3")) +
  facet_wrap(~group) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title.position = "plot",
        text = element_text(family = "Corbel"),
        plot.title = element_markdown(size = 14),
        plot.subtitle = element_markdown(size = 14)) +
  labs(x = "",
       y = "Percentage graduated",
       title = "Hispanics in the USA historically have the lowest <b style='color:#7570b3'>High School</b> graduation rates.",
       subtitle = "Asians graduate at <b style='color:#1b9e77'>Bachelor</b> level at far higher percentages than any other race.",
       caption = "Data: Data World / NCES\nVisualisation: Dan Irwin-Brown, #tidytuesday, February 2021")

####save plot####
ggsave("college_enrollment.png", device = "png", type = "cairo")
