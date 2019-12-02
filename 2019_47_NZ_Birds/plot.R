nz_bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")

# Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
# Either ISO-8601 date or year/week works!
# Install via devtools::install_github("thebioengineer/tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load("2019-11-19")
tuesdata <- tidytuesdayR::tt_load(2019, week = 47)

nz_bird <- tuesdata$nz_bird

library(viridis)
library(magrittr)
library(ggplot2)
library(dplyr)
library(ggdark)
library(ggthemes)

#Place points
top5_votes <- nz_bird %>%
  filter(!is.na(vote_rank) & !is.na(bird_breed)) %>%
  count(bird_breed) %>%
  rename(total = n) %>%
  arrange(desc(total)) %>%
  top_n(5) 


#Getting names of the top 5 bird breeds to filter
top5_names <- top5_votes$bird_breed

top5_final <- nz_bird %>%
  subset(bird_breed %in% top5_names) %>%
  count(bird_breed, vote_rank) %>%
  left_join(top5_votes) %>%
  group_by(bird_breed)

top5_final$bird_breed = factor(top5_final$bird_breed, 
                                   levels = c("Yellow-eyed penguin", "Kākāpō", "Black Robin", 
                                              "Kākā",
                                              "Banded Dotterel"))

#Building Visualization
ggplot(top5_final, aes(x = bird_breed, y = n)) +
  geom_bar(aes(fill = vote_rank),stat="identity")  +
  scale_fill_viridis(discrete = T, option = "E") +
  labs(x="", y="Votes 2019",
       title="Bird of the year 2019 in New Zeland",
       subtitle="Disaggregation by type of received votes for the top 5 winner birds.",
       caption="Plot: @manginiflor | Data: Nathan Moore - Dragonfly Data Science") + 
  hrbrthemes::theme_ft_rc()+
  ##dark_mode(theme_fivethirtyeight())
  theme(legend.position="bottom")

