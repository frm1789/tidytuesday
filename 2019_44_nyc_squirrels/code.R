nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

tidy_sq <- nyc_squirrels

# Little exploration
skimr::skim(nyc_squirrels)
head(nyc_squirrels)
