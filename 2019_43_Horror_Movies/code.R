library(tidyverse)

horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

#Top 10 producer 
best_producer <- horror_movies %>%
  group_by(release_country) %>%
  summarise(count = n()) %>%
  top_n(10) %>%
  arrange(desc(count))

#Tidy dataset
tidy_horror <- horror_movies %>%
  filter(nchar(horror_movies$release_date) > 6, is.na(budget) != TRUE) %>% 
  dplyr::mutate(date = lubridate::dmy(release_date),
                month = lubridate::month(date, label = TRUE, abbr = FALSE)) %>% 
  #Next step: All the budget in same currency  
 
  
# 1) Are the movies with high budget released in October?
# 2) In others countries, not USA and Canada, wich is the month when more productions are launched?

tidy_horror %>%
  select(month, release_country) %>%  
  ggplot(mapping=aes(x=month, y=release_country)) + 
  geom_point()  

