library(tidyverse)
library(RColorBrewer)
library(reshape)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")
tidy_sq <- nyc_squirrels

#tidy dataframe
tidy_sq <- transform(tidy_sq, hectare_v = substr(hectare, 1, 2), hectare_h = substr(hectare, 3,3))



