library(dplyr)
library(chorddiag)

## 1. Reading dataset 
nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")

## 2. Checking null values
sapply(nobel_winners, function(x) sum(is.na(x))) 
#The values "null" is equal to organizations laurated with the Nobel prize

## 3. Counting quantity of winners by gender
nobel_gender <- nobel_winners %>% 
  filter(!is.na(gender))%>% 
  group_by(category, gender)%>%
  summarise(count=n()) %>% 
  tidyr::spread(gender, count)

## 4. nobel_gender transformation to matrix
nobel_gender_mat <- as.matrix(nobel_gender[,-1])
row.names(nobel_gender_mat) <- as.array(nobel_gender$category)

## 5. Creating chord diagram
groupColors <- c("#440154FF", "#482677FF","#2D708EFF", "#238A8DFF", "#55C667FF", "#95D840FF","#FDE725FF", "#39568CFF") 
chorddiag(nobel_gender_mat, type = "bipartite", 
          groupColors = groupColors,
          tickInterval = 50)

