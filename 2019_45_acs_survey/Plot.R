library(tidyverse)
library(maps) 

data(us.cities)
us.cities$name = substr(us.cities$name,1,nchar(us.cities$name)-3)

#read in data
us_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")
us_mode <- us_mode %>%
  filter(city_size == "Large") 

us_mode$city <- gsub(" city", "", us_mode$city)
us_mode <- left_join(us_mode, us.cities, by = c("city" = "name") )

us_mode <- us_mode %>%
  top_n(20, pop)

#Data for Top 10 largest cities
name_city = unique(us_mode$city)
census_2008_walked = c(4.9, 10.4, 3.6, 5.7, 2.3, 9.1, 2.2, 2.2, 3.6, 1.9)
census_2008_bicycled = c(0.6, 0.5, 0.6, 0.5, 0.5, 0.9, 0.9, 0.2, 0.7, 0.1)
bike_dif_2008 = data.frame(city = name_city, census_2008_bicycled)
walk_dif_2008 = data.frame(city = name_city, census_2008_walked)

#Adding difference 
walk_dif_2012 <- us_mode %>%
  select(city, percent, mode) %>% 
  filter(mode == "Walk")  %>% 
  inner_join(walk_dif_2008, by = "city") %>% 
  mutate(change = percent - census_2008_bicycled)  %>% 
  set_names(c("city", "percent_2012", "mode" ,"percent_2008", "change")) %>% 
  arrange(desc(change)) # change the column names

bike_dif_2012 <- us_mode %>%
  select(city, percent, mode) %>% 
  filter(mode == "Bike")  %>% 
  inner_join(bike_dif_2008, by = "city") %>% 
  mutate(change = percent - census_2008_bicycled) %>% 
  set_names(c("city", "percent_2012", "mode" ,"percent_2008", "change")) %>% 
  arrange(desc(change))

both.dfs <- rbind( walk_dif_2012, bike_dif_2012)

both.dfs$city = factor(both.dfs$city, levels = c("New York", "Philadelphia", "Chicago", "Los Angeles",
                                                 "San Diego", "San Antonio", "San Jose", "Houston", "Dallas",
                                                 "Phoenix"))

ggplot(both.dfs)+
  geom_linerange(aes(x = city, xend=city, ymin = 0, ymax = change, colour = mode), 
                 position = position_dodge(width = 0.5)) +
  geom_point(aes(x = city, y = change, colour = mode),
             stat = "identity", position = position_dodge(width = 0.5))+
  scale_color_manual(values=c('#999999','#E69F00'))+
  labs(x="Top 10 cities by Population", y="Change (%) 2008 - 2012",
       title="Changes in Rates of Walking & Bicycling to Work | 10 Largest Cities",
       subtitle="Data from Census 2000 and American Community Survey 2008–2012",
       caption="Plot: @manginiflor | Data: ACS") + 
  theme_ft_rc()+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.position="none")



# Plot
ggplot(bike_dif_2012, aes(x=city, y=change)) +
  geom_segment(aes(x=reorder(city, -change), xend=city, y=0, yend=change), color="#E69F00") +
  geom_point(color = "#E69F00")  +
  labs(x="Top 10 cities by Population", y="Difference (%) Census 2008 - 2012",
       title="Changes in the Rates of Bicycling to Work for 10 Largest Cities",
       subtitle="Data from Census 2000 and American Community Survey 2008–2012",
       caption="Plot: @manginiflor | Data: ACS") + 
  theme_ft_rc()+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.position="none")


# Plot 
ggplot(walk_dif_2012, aes(x=city, y=change)) +
  geom_segment(aes(x=reorder(city, -change), xend=city, y=0, yend=change), color="#999999") +
  geom_point(color = "#999999")  +
  labs(x="Top 10 cities by Population", y="Difference (%) Census 2008 - 2012",
       title="Changes in the Rates of Walking to Work for 10 Largest Cities",
       subtitle="Data from Census 2000 and American Community Survey 2008–2012",
       caption="Plot: @manginiflor | Data: ACS") + 
  theme_ft_rc()+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.position="none")

