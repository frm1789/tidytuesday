library(GGally)
library(ggplot2)
library(janitor)
library(tidyverse)
library(countrycode)
library(ggalt)
library(dplyr)
library(viridis)
library(scales)

#Data
coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")
mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

#Colors
q_colors =  5 # for continents
v_colors =  viridis(q_colors, option = "D")

#Cleaning
df_waste <- waste_vs_gdp %>%
  setNames(c("entity", "code", "year", "per_capita_plastic_waste",
             "per_capita_gdp", "population")) %>%
  drop_na(per_capita_plastic_waste, per_capita_gdp, population)

df_mismanaged <- mismanaged_vs_gdp %>%
  setNames(c("entity", "code", "year", "per_capita_mismanaged_plastic",
             "per_capita_gdp", "population")) %>%
  drop_na(per_capita_mismanaged_plastic, per_capita_gdp, population)

df_coast <- coast_vs_waste %>%
  setNames(c("entity", "code", "year", "total_mismanaged_plastic",
             "coastal_population", "population")) %>%
  drop_na(total_mismanaged_plastic, coastal_population, population)

#Create a unique df
df_waste_misma <- df_waste %>%
  full_join(df_mismanaged, by = c("entity", "code", "year", "per_capita_gdp", "population")) %>%
  mutate(continent = countrycode(sourcevar = entity, 
                                 origin = "country.name.en", 
                                 destination = "continent")) %>%
  full_join(df_coast, by = c("entity", "code", "year", "population"))%>%
  drop_na(per_capita_plastic_waste, per_capita_gdp)

#ggcorr looking for correlations
df_numbers <- df_waste_misma %>%
  select(per_capita_plastic_waste, per_capita_gdp, population, per_capita_mismanaged_plastic,
         total_mismanaged_plastic, coastal_population)

ggpairs(df_numbers, cardinality_threshold = 148) + 
  labs(title="Plastic pollution in Our World", 
       subtitle="Correlation between variables", 
       caption = "Source: Our World in Data | by: Florencia Mangini") +
  theme_minimal() 



## 1) per_capita_mismanaged_plastic vs. coastal population: 0.783
top_vis1 <- df_waste_misma[df_waste_misma$coastal_population > 20000949 & 
                        df_waste_misma$coastal_population <= max(df_waste_misma$coastal_population) & 
                        df_waste_misma$total_mismanaged_plastic > 15466 & 
                        df_waste_misma$total_mismanaged_plastic <= max(df_waste_misma$total_mismanaged_plastic), ]


vis1 <- ggplot(df_waste_misma, aes(y=total_mismanaged_plastic, x=coastal_population)) + 
  geom_point(aes(col=continent, size=df_waste_misma$population)) + 
  scale_color_manual(values=c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF"))+
  geom_smooth(method="loess", se=T) + 
  geom_text(data=top_vis1,
            aes(label=entity))+
  labs(title="Coastal population vs. Total mismanaged plastic", 
       x="Total mismanaged plastic", 
       y="Coastal population", 
       caption = "Source: Our World in Data | by: Florencia Mangini")+
  theme_minimal()
plot(vis1)


## 2) per_capita_mismanaged_plastic vs. per_capita_gdp: -0.43

vis2 <- ggplot(df_waste_misma, aes(x=per_capita_mismanaged_plastic, y=per_capita_gdp)) + 
  geom_point(aes(col=continent, size=per_capita_mismanaged_plastic)) + 
  scale_color_manual(values=c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF"))+
  geom_smooth(method="loess", se=T) + 
  labs(title="Waste Ratio vs. GDP",
       x="Mismanaged plastic (per capita)", 
       y="GDP (per capita)", 
       caption = "Source: Our World in Data | by: Florencia Mangini")+
  theme_minimal()
plot(vis2)




## 3) Mismanaged plastic vs. plastic waste: 0.27
##Ratio bewteen waste_ratio & mismanaged_plastic
# per_capita_mismanaged_plastic: Amount of mismanaged plastic waste per capita in kg/day
# per_capita_plastic_waste: Amount of plastic waste per capita in kg/day
df_waste_misma <- df_waste_misma %>%
  mutate(ratio_waste = per_capita_mismanaged_plastic / per_capita_plastic_waste) 

top_vis3 <- df_waste_misma[df_waste_misma$ratio_waste > 0.50 & 
                df_waste_misma$ratio_waste <= 0.99 & 
                df_waste_misma$per_capita_mismanaged_plastic > 0.135 & 
                df_waste_misma$per_capita_mismanaged_plastic < 0.4, ]

vis3 <- ggplot(df_waste_misma, aes(x=ratio_waste, y=per_capita_mismanaged_plastic)) + 
  geom_point(aes(col=continent, size=per_capita_mismanaged_plastic )) + 
  scale_color_manual(values=c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF"))+
  geom_smooth(method="loess", se=T) + 
  geom_encircle(aes(x=ratio_waste, y=per_capita_mismanaged_plastic), 
                data=top_vis3, 
                color="#440154FF", 
                size=2, 
                expand=0.1) +  
  geom_text(data=top_vis3,
            aes(label=entity)) +
  labs(title="Countries with more mismanaged plastic and  waste ratio per capita", 
       subtitle="In purple, top countries with values above the general average for both waste ratio and mismanaged plastic.",
       x="Waste Ratio", 
       y="Mismanaged Plastic", 
       caption = "Source: Our World in Data | by: Florencia Mangini")+
  theme_minimal() 
plot(vis3)



