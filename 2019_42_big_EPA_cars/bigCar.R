library(viridis)
library(ggridges)
library(tidyverse)

big_epa_cars <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv")


#Top 10 makers 
best_makers_cars <- big_epa_cars %>%
    group_by(make) %>%
    summarise(count = n()) %>%
    top_n(10) %>%
    arrange(desc(count))

#Visualization
big_epa_cars %>%
  right_join(best_makers_cars) %>%
  separate(trany, c("transmission", "Type"), sep = " ")%>%
  drop_na(transmission) %>% 
  ggplot(aes(y= make, color = youSaveSpend)) +
  geom_density_ridges(aes(x = year, fill = transmission), 
                          alpha = .8, color = "white") +
  theme_minimal()  +
  labs(
    title ="Automatic or Manual cars are better for thrifty consumers?",
    subtitle = "Considering the saving or spending over 5 years vs. an average car: automatic cars are more economical than manual.",
    caption = "Florencia Mangini (@manginiflor)\nsource: BigCar for Tidytuesday") +
  scale_fill_manual(
    name = "Transmission", guide = "legend",
    values=c("#3B528BFF", "#29AF7FFF"), 
    labels = c("Automatic", "Manual")) +
  scale_color_manual(values = c("#3B528B", "#29AF7F"), guide="none") + 
  scale_x_continuous(expand = c(0.01, 0)) +
  labs(x = 	"Evolution of the saving/spending by years",
       y = "Manufacturers") 

#Double check
#Considering the top 10 of economical cars by 2019: are they manual or automatic?
big_epa_cars %>%
  select(make, model, year, VClass, youSaveSpend, trany) %>%  
  filter(youSaveSpend >0, year >= 2018)%>%  
  separate(trany, c("transmission", "Type"), sep = " ") %>%
  drop_na(transmission)  %>%
  select(-Type) %>%
  ggplot(mapping=aes(x=transmission, y=youSaveSpend)) + 
  geom_boxplot(colour = c("#3B528BFF", "#29AF7FFF"), fill = "white") +
  geom_jitter(aes(colour = transmission)) +
  scale_color_manual(values = c("#3B528B", "#29AF7F"), guide="none") +
  theme_minimal() +
  labs(
    title ="Savings for Automatic and Manual cars for models from 2018 & 2019",
     caption = "Florencia Mangini (@manginiflor)\nsource: BigCar for Tidytuesday") +
  labs(x = 	"Types of transmission",
       y = "Total Saving Spend") 


