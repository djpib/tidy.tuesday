#libraries
library(tidytuesdayR)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggalt)
library(forcats)
library(gridExtra)
library(extrafont)

#get data
tuesdata <- tidytuesdayR::tt_load(2021, week = 3)
artwork <- tuesdata$artwork
artists <- tuesdata$artists

#clean / manipulate data
artists <- artists %>% 
  separate(col = placeOfBirth,
           into = c("born.city", 
                    "born.country"),
           sep = ", ",
           remove = FALSE,
           extra = "merge",
           fill = "left") #without extra and fill, warnings are given as some places of birth have more than one comma, and others have no commas. I did not investigate too much, it seemed the ones I looked at were countries so I filled left and left it at that, but it definitely is not correct. 

artists <- artists %>% 
  rename(artistId = id)

artwork <- artwork %>% 
  left_join(artists,
            by = "artistId")

artwork[is.na(artwork)] <- "Not known"

artwork <- artwork %>%
  #not sure if this is the best way to do it? countrycode doesn´t do from multiple languages to English?
  mutate(born.country.new = if_else(
    born.country == "Sverige", "Sweden", if_else(
      born.country == "België", "Belgium", if_else(
        born.country == "Bharat", "India", if_else(
          born.country == "Ceská Republika", "Czech Republic", if_else(
            born.country == "Deutschland", "Germany", if_else(
              born.country == "Éire", "Ireland", if_else(
                born.country == "España", "Spain", if_else(
                  born.country == "Italia", "Italy", if_else(
                    born.country == "Magyarország", "Hungary", if_else(
                      born.country == "Nederland", "Netherlands", if_else(
                        born.country == "Nihon", "Japan", if_else(
                          born.country == "Österreich", "Austria", if_else(
                            born.country == "Polska", "Poland", if_else(
                              born.country == "România", "Romania", if_else(
                                born.country == "Rossiya", "Russia", if_else(
                                  born.country == "Schweiz", "Switzerland", if_else(
                                    born.country == "Sverige", "Sweden", if_else(
                                      born.country == "Ukrayina", "Ukraine", if_else(
                                        born.country == "Yisra'el", "Israel", if_else(
                                          born.country == "Zhonghua", "China", born.country
                                          )))))))))))))))))))))

#graph artists per country
graph.artists.per.country <- artwork %>% 
  group_by(born.country.new) %>% 
  summarise(no.per.country = n_distinct(artistId)) %>% 
  ungroup() %>% 
  filter(no.per.country > 25, 
         born.country.new != "Not known") %>% 
  ggplot(aes(x = fct_rev(born.country.new), 
             y = no.per.country)) + 
  geom_lollipop(point.size = 4, 
                color = "#0072ce") +
  coord_flip() +
  labs(x = "", 
       y = "Number of artists that were born in the specified country and have artwork owned by the Tate", 
       title = "Artists born in the UK are most prevalent at the Tate",
       subtitle = "Unsurprising given it is ´the United Kingdom's national collection of British art, and international modern and contemporary art´.\nGraph shows countries with > 25 artists in the Tate´s collection.") +
  theme(text = element_text(family = "Corbel", 
                            size=10),
        plot.title.position = "plot",
        plot.title = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#E5E5E3"),
        panel.background = element_rect(fill = "#E5E5E3"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(colour = "#E5E5E3"),
        axis.line.y = element_line(colour = "#E5E5E3"),
        axis.title = element_text(size = 9))

#graph number of artworks per artist
graph.number.artworks.per.artist <- artwork %>% 
  group_by(name) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n < 50) %>% 
  ggplot(aes(x = n)) +
  geom_histogram(fill = "#0072ce",
                 bins = 100) + 
  labs(x = "Number of artworks owned by the Tate per artist",
       y = "",
       title = "The Tate mostly owns just one work per artist it has bought artwork from",
       subtitle = "Y-axis is number of artists. Histogram filters out artists with >= 50 works.",
       caption = "Data: @Tate\nVisualisation: Dan Irwin-Brown, #tidytuesday, January 2021") +
  theme(text = element_text(family = "Corbel", 
                            size=10),
        plot.title.position = "plot",
        plot.title = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#E5E5E3"),
        panel.background = element_rect(fill = "#E5E5E3"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(colour = "#E5E5E3"),
        axis.line.y = element_line(colour = "#E5E5E3"),
        axis.title = element_text(size = 9)) +
  geom_text(aes(x = 20, 
                y = 1300, 
                label = "Somewhere over there is Joseph Mallor William Turner.\nHe has the most artworks at the Tate: 39,389.\nThe next largest collection is of George Jones at 1,046."),
            size = 3.1, 
            color = "grey50",
            hjust = 0) +
  geom_curve(x = 38, 
             xend = 50, 
             y = 1300, 
             yend = 1300, 
             arrow = arrow(length = unit(0.1,
                                         "inches")), 
             curvature = 0,
             color = "grey50")

#put plots together and output
final.plot <- grid.arrange(graph.artists.per.country, 
                           graph.number.artworks.per.artist, 
                           ncol = 1)

ggsave(filename = "tate.png",
       plot = final.plot,
       width = 11,
       height = 6,
       dpi = 400)