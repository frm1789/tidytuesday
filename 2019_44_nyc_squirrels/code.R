nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

tidy_sq <- nyc_squirrels

# Little exploration
skimr::skim(nyc_squirrels)
head(nyc_squirrels)

# hectare_squirrel_number
# hectare # 37F # 05D
tidy_sq <- transform(tidy_sq, hectare_v = substr(hectare, 1, 2), hectare_h = substr(hectare, 3,3))

tidy_sq_max <- tidy_sq %>%
  select(hectare_h, hectare_v, shift, date, hectare_squirrel_number) %>%
  group_by(hectare_h, hectare_v, shift, date) %>%
  summarise(max_value = max(hectare_squirrel_number)) %>%
  arrange(desc(date))

tidy_sq_max_total <- tidy_sq_max %>%
  group_by(hectare_h, hectare_v, shift, date) %>%
  summarise(total = sum(max_value)) %>% 
  arrange(desc(date))


# Heatmap day and night

