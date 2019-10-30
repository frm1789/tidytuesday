library(tidyverse)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

tidy_sq <- nyc_squirrels


tidy_sq <- transform(tidy_sq, hectare_v = substr(hectare, 1, 2), hectare_h = substr(hectare, 3,3))


tidy_sq_max <- tidy_sq %>%
  select(hectare_h, hectare_v, shift, date, hectare_squirrel_number) %>%
  group_by(hectare_h, hectare_v, shift, date) %>%
  summarise(max_value = max(hectare_squirrel_number)) %>%
  arrange(desc(date))

tidy_AMPM <- tidy_sq_max %>%
  group_by(hectare_h, hectare_v, shift, date) %>%
  summarise(total = sum(max_value)) %>% 
  arrange(desc(date))  %>% 
  group_by(hectare_h, hectare_v) %>% 
  summarize(totalAMPM = as.integer(sum(total))) 

library(reshape)
tidy_AMPM_wide <- cast(tidy_AMPM, hectare_h ~ hectare_v)


tidy_matrix <- data.matrix(tidy_AMPM_wide)

library(RColorBrewer)
# Heatmap day and night
america_heatmap <- heatmap(tidy_matrix, hectare_h=NA, 
                           hectare_v=NA, col = brewer.pal(9, "Blues"), scale="column", 
                           margins=c(2,6))

library(gplots)
heatmap.2(tidy_matrix)  



??heatmap