#load libraries and fonts
library(tidytuesdayR)
library(tidyr)
library(dplyr)
library(ggplot2)
library(countrycode)
library(forcats)
library(extrafont)
loadfonts(device = "win", 
          quiet = TRUE)

#get data
tuesdata <- tidytuesdayR::tt_load(2021, 
                                  week = 2)
transit_cost <- tuesdata$transit_cost

#tidy data
transit_cost$country <- as.factor(transit_cost$country)

transit_cost <- transit_cost %>%
  select(country, 
         city, 
         line, 
         start_year, 
         end_year, 
         tunnel_per, 
         stations, 
         cost, 
         length, 
         tunnel) %>% 
  filter(country != "NA") %>% 
  group_by(country) %>% 
  mutate(no.projects = n()) %>% 
  ungroup() %>% 
  filter(no.projects > 10)

transit_cost$country <- countrycode(sourcevar = transit_cost$country, 
                                    origin = "iso2c", 
                                    destination = "country.name")

#graph
transit_cost %>% ggplot(mapping = aes(x = fct_rev(country),
                                      y = length,
                                      colour = country)) +
  geom_jitter(alpha = 0.5,
              size = 3,
              width = 0.2) +
  geom_boxplot(fill = NA,
               outlier.shape = NA) +
  labs(x = "",
       y = "Length of lines (completed and in-progress), km",
       title = "Transit lengths per country", 
       subtitle = "Length of lines of completed and in-progress transit-infrastructure projects in countries with >10 projects in data-set",
       caption = "Data: Transit Costs Project\nVisualisation: Dan Irwin-Brown, #tidytuesday, January 2021") +
  theme_classic() +
  theme(text = element_text(family = "Calibri", 
                            size = 12, 
                            colour = "white"), 
        legend.position = "none",
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        plot.title = element_text(size = 30),
        axis.ticks.y = element_blank()) +
  coord_flip() +
  geom_text(aes(x = 10, 
                y = 185,
                label = "The longest line in the data-set is\nin Paris (GPX), it started in 2017\nand is planned to finish in 2030"),
            size = 3.1, 
            color = "grey50",
            hjust = 1) +
  geom_curve(x = 10, 
             xend = 9, 
             y = 186, 
             yend = 199, 
             arrow = arrow(length = unit(0.1, 
                                         "inches")), 
             curvature = -0.2,
             color = "grey50") +
  geom_text(aes(x = 9, 
                y = 75, 
                label = "With a total of 253, China\nhas the most number of lines\ncompleted or in-progress"),
            size = 3.1, 
            color = "grey50",
            hjust = 0) +
  geom_curve(x = 9, 
             xend = 9.5, 
             y = 74, 
             yend = 60, 
             arrow = arrow(length = unit(0.1, 
                                         "inches")), 
             curvature = -0.2,
             color = "grey50") +
  geom_text(aes(x = 6, 
                y = 105, 
                label = "With 128 stations, this\nline (Chennai) in India\nhas the most of all studied"),
            size = 3.1, 
            color = "grey50",
            hjust = 1) +
  geom_curve(x = 6, 
             xend = 7, 
             y = 106, 
             yend = 118, 
             arrow = arrow(length = unit(0.1,
                                         "inches")), 
             curvature = 0.2,
             color = "grey50")

ggsave(filename = "transit.length.png",
       width = 11,
       height = 6,
       dpi = 400)