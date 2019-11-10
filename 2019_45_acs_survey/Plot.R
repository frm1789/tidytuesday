library(tidyverse)
library(noncensus)
library(usmap)
library(gridExtra)

install.packages("UScensus2010")
library(UScensus2010)

#read in data
us_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")


#Data for Top 50 largest cities
us_mode$census_2000_walked = c(4.9, 10.4, 3.6, 5.7, 2.3, 9.1, 2.2, 2.2, 3.6, 1.9, 1.4, 2.5, 1.8, 2.0, 2.5, 2.9, 2.4, 3.7, 3.7, 2.2, 6.6, 4.0, 1.4, 1.6)
us_mode$census_2000_bicycled = c(0.6, 0.5, 0.6, 0.5, 0.5, 0.9, 0.9, 0.2, 0.7, 0.1, 0.6, 0.9, 0.4, 0.2, 2.0, 0.3, 0.1, 0.1, 0.2, 0.1, 0.1, 1.0, 1.9, 1.0, 1.2, 0.1, 0.3, 0.4, 1.8, 0.1, 0.3, 0.4, 1.1, 2.2, 0.8, 1.4, 0.7, 0.1, 1.2, 0.3, 0.3, 0.5, 0.3, 0.1, 0.6, 1.2, 0.2, 1.9, 0.2, 0.2, 0.2)
