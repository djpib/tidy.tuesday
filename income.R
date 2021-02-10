# libraries -------------------------------------
library(dplyr)
library(ggplot2)
library(forcats)
library(RColorBrewer)
library(extrafont)

# get data --------------------------------------
income_distribution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')

# tidy data -------------------------------------
income_distribution <- 
  income_distribution %>% 
  mutate(race = as.factor(race))

income_distribution$income_bracket <- 
  ordered(income_distribution$income_bracket, 
          levels = c("$200,000 and over", 
                     "$150,000 to $199,999",
                     "$100,000 to $149,999",
                     "$75,000 to $99,999",
                     "$50,000 to $74,999",
                     "$35,000 to $49,999",
                     "$25,000 to $34,999",
                     "$15,000 to $24,999",
                     "Under $15,000"))

# plot ------------------------------------------
income_distribution %>% 
  filter(year == 2019, 
         race != "Asian Alone or in Combination", 
         race != "Black Alone or in Combination", 
         race != "White Alone, Not Hispanic",
         race != "All Races") %>%
  mutate(race = fct_relevel(race, 
                            "White Alone", 
                            "Hispanic (Any Race)", 
                            "Black Alone", 
                            "Asian Alone")) %>% 
  ggplot(aes(y = race, 
             x = income_distribution, 
             fill = income_bracket)) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "BrBG") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#f5f5f5", 
                                        colour = "#f5f5f5"),
        plot.background = element_rect(fill = "#f5f5f5", 
                                       colour = "#f5f5f5"),
        text = element_text(family = "Corbel",
                            size = 14),
        legend.title = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size = 25)) +
  labs(x = "Percentage of population", 
       y = "",
       title = "Income disparity in America, 2019",
       subtitle = "61.80% of those categorised as Asian Alone had incomes over USD 75,000.\n29.40% of those categorised as Black Alone had incomes over this same amount.",
       caption = "Data: @urbaninstitute & US Census\nVisualisation: Dan Irwin-Brown, #tidytuesday, February 2021")

# save ------------------------------------------
ggsave("income.png", device = "png", type = "cairo")
