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

#clean names 
colnames(coast_vs_waste) <- c("entity", "code", "year", "total_mismanaged_plastic","coastal_population", "population")
colnames(mismanaged_vs_gdp) <- c("entity", "code", "year", "per_capita_mismanaged_plastic","per_capita_gdp", "population")
colnames(waste_vs_gdp) <- c("entity", "code", "year", "per_capita_plastic_waste","per_capita_gdp", "population")

#clean na
df_waste <- waste_vs_gdp %>%
  drop_na(per_capita_plastic_waste, per_capita_gdp, population)

df_mismanaged <- mismanaged_vs_gdp %>%
  drop_na(per_capita_mismanaged_plastic, per_capita_gdp, population)

df_coast <- coast_vs_waste %>%
  drop_na(total_mismanaged_plastic, coastal_population, population)


#Create a unique df
df_waste_misma <- df_waste %>%
  full_join(df_mismanaged, by = c("entity", "code", "year", "per_capita_gdp", "population")) %>%
  full_join(df_coast, by = c("entity", "code", "year", "population"))%>%
  mutate(continent = countrycode(sourcevar = entity, 
                                 origin = "country.name.en", 
                                 destination = "continent")) %>%
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

#Colors
q_colors =  5 # for continents
v_colors =  viridis(q_colors, option = "D")


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


