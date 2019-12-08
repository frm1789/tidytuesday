library(tidyverse)
library(osmdata)

map_philadelphia <- getbb("Philadelphia")

tickets <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

tickets_in <- filter(tickets, between(lon, map_philadelphia[1,1], map_philadelphia[1,2]), 
                     between(lat, map_philadelphia[2,1], map_philadelphia[2,2])) %>% 
  mutate_at(c("lat", "lon"), ~round(.x, 3))

streets <- getbb("Philadelphia")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()
streets

small_streets <- getbb("Philadelphia")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- getbb("Philadelphia")%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()



ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "#7fc0ff",
          size = .4,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .2,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .2,
          alpha = .5) +
  geom_bin2d(data = tickets_in, aes(x = lon, y = lat, fill = ..count..), binwidth = 0.001,
             size = .2,
             alpha = .5) +
  coord_sf(xlim = c(-75.2803, -74.95583), 
           ylim = c(39.8670, 40.13796),
           expand = FALSE) +
  scale_fill_viridis_c(option = "plasma") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#282828")) +
  labs(x = NULL,
       y = NULL,
       title = "P H I L A D E L P H I A,  P A",
       caption = "Data: Tidytuesday | Author: @manginiflor") +
  theme(axis.text = element_blank(),
        legend.position = "none")

install.packages("glue")
library(glue)
library(viridis)
install.packages("jkmisc")
library(jkmisc)
install.packages("nord")
library(nord)