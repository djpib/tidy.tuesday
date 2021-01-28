#libraries
library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(forcats)
library(extrafont)

#get data
tuesdata <- tidytuesdayR::tt_load(2021, week = 5)
plastics <- tuesdata$plastics

#manipulate data
plastics %>% rename("High density polyethylene" = hdpe,
                    "Low density polyethylene" = ldpe,
                    "Polyester plastic" = pet,
                    "Polypropylene" = pp,
                    "Polystyrene" = ps,
                    "PVC plastic" = pvc) %>% 
  filter(parent_company != "Grand Total", 
         parent_company != "Unbranded", 
         parent_company != "NULL", 
         parent_company != "null", 
         parent_company != "Assorted") %>% 
  pivot_longer(cols = c("empty", 
                        "High density polyethylene", 
                        "Low density polyethylene", 
                        "o", 
                        "Polyester plastic", 
                        "Polypropylene", 
                        "Polystyrene", 
                        "PVC plastic"), 
               names_to = "plastic_type", 
               values_to = "plastic_type_count") %>% 
  group_by(parent_company, 
           plastic_type) %>% 
  summarise(total_per_parent_per_type = sum(plastic_type_count, 
                                            na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(parent_company) %>% 
  mutate(total_per_parent = sum(total_per_parent_per_type, 
                                na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(total_per_parent >= 5000, 
         plastic_type != "empty", 
         plastic_type != "o") %>% 

#plot
  ggplot(aes(x = plastic_type, 
             y = fct_rev(parent_company), 
             fill = total_per_parent_per_type)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, 
                                   hjust = 0), 
        legend.title = element_blank(), 
        text = element_text(family = "Corbel", 
                            size = 14), 
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.background = element_rect(fill = "#FEFEFA", 
                                       colour = "#FEFEFA")) +
  scale_x_discrete(position = "top") +
  labs(x = "", 
       y = "",
       title = "Amongst companies with the highest amount of plastic collected over 2019/2020, polyester (soft \ndrinks bottles, food containers, plastic bottles...) was the most common type of plastic found. This\nwas followed by polypropylene (carry-out beverage cups, microwavable food containers...).",
       subtitle = "Numbers refer to sum of items found by volunteers at counting events worldwide",
       caption = "Data: breakfreefromplastic.org\nVisualisation: Dan Irwin-Brown, #tidytuesday, January 2021") +
  geom_text(aes(label = total_per_parent_per_type), 
            size = 2.5)

#save
ggsave("plastic.png")