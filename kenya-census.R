#libraries
library(rKenyaCensus)
library(tidyverse)
library(ggplot2)
library(forcats)
library(extrafont)
library(RColorBrewer)

#get data
population_density <- rKenyaCensus::V2_T2.2
water_source <- rKenyaCensus::V4_T2.15

#tidy/manipulate data
water_source <- water_source %>% 
  filter(AdminArea == "County") %>% 
  left_join(population_density,
            by = "County") %>% 
  rename(density = `Density(Persons per Sq km)`) %>% 
  mutate("Pond/Dam/Lake/Stream/River" = `Pond` + `Dam/Lake` + `Stream/River`,
         "Unprotected spring/well" = `UnprotectedSpring` + `UnprotectedWell`,
         "Protected spring/well" = `ProtectedSpring` + `ProtectedWell`,
         "Piped to yard/plot/dwelling" = `Pipedtoyard/Plot` + `Pipedintodwelling`,
         "Bottled water/water vendor" = `Bottledwater` + `WaterVendor`) %>% 
  mutate("density_ranges" = if_else(
           density <= 125, "0-125", if_else(
             density <= 500, "126-500", if_else(
               density <= 750, "501-750", if_else(
                 density <= 1050, "751-1050", 
                 "5495-6247"))))) %>% 
  pivot_longer(cols = c(`Borehole/TubeWell`,
                        `Rain/Harvestedwater`,
                        `Publictap/Standpipe`,
                        `Pond/Dam/Lake/Stream/River`,
                        `Unprotected spring/well`,
                        `Protected spring/well`,
                        `Piped to yard/plot/dwelling`,
                        `Bottled water/water vendor`),
               names_to = "source",
               values_to = "percent_source") %>% 
  ungroup(County) %>% 
  mutate(County = fct_relevel(County, "BARINGO", "GARISSA", "ISIOLO", "KAJIADO", "KILIFI", "KITUI", "KWALE", "LAIKIPIA", "LAMU", "MAKUENI", "MANDERA", "MARSABIT", "NAROK", "SAMBURU", "TAITA/TAVETA", "TANA RIVER", "TURKANA", "WAJIR", "WEST POKOT", "BOMET", "ELGEYO/MARAKWET", "EMBU", "HOMA BAY", "KERICHO", "KIRINYAGA", "MACHAKOS", "MERU", "MIGORI", "MURANG'A", "NAKURU", "NANDI", "NYANDARUA", "NYERI", "SIAYA", "THARAKA-NITHI", "TRANS NZOIA", "UASIN GISHU", "BUNGOMA", "BUSIA", "KAKAMEGA", "KISUMU", "NYAMIRA", "KIAMBU", "KISII", "VIHIGA", "MOMBASA", "NAIROBI CITY")) %>% 
  mutate(source = fct_relevel(source,
                              "Pond/Dam/Lake/Stream/River",
                              "Rain/Harvestedwater",
                              "Unprotected spring/well",
                              "Protected spring/well",
                              "Borehole/TubeWell",
                              "Publictap/Standpipe",
                              "Bottled water/water vendor",
                              "Piped to yard/plot/dwelling")) %>% 
  mutate(density_ranges = fct_relevel(density_ranges,
                                      "0-125",
                                      "126-500",
                                      "501-750",
                                      "751-1050",
                                      "5495-6247"))

#graph
water_source %>% ggplot(aes(x = percent_source,
                            y = source)) +
  geom_col(aes(fill = density_ranges)) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = c(0.481, 1.04),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(family = "Corbel", 
                            size=10),
        plot.title.position = "plot",
        plot.title = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#E5E5E3"),
        panel.background = element_rect(fill = "#E5E5E3"),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9)) +
  facet_wrap(~ County) +
  labs(y = "",
       x = "Percentage use of source",
       title = "Sources of water, by County, Kenya",
       subtitle = "Counties are coloured by population density (persons per sq km), as so:",
       caption = "Data: rKenyaCensus\nVisualisation: Dan Irwin-Brown, #tidytuesday, January 2021")

#save image
ggsave(filename = "r-kenya-census.png",
       width = 10.8, 
       height = 12.5)