library(tidyverse)
library(quantmod)
library(packcircles)
library(viridis)

horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

#Tidy horror
tidy_horror <- horror_movies %>%
  filter(nchar(horror_movies$release_date) > 6, is.na(budget) != TRUE) %>% 
  dplyr::mutate(date = lubridate::dmy(release_date),
                month = lubridate::month(date, label = TRUE, abbr = FALSE)) 

# Convertion to the same currency
# This part of the code is from @ewbarba
tidy_horror$currency <- gsub("[0-9,[:space:]]", "", tidy_horror$budget) #strip currency
tidy_horror$currency <- gsub("\\$", "USD", tidy_horror$currency) #convert symbols to currency abbrvs
tidy_horror$currency <- gsub("\\£", "GBP", tidy_horror$currency)
tidy_horror$currency <- gsub("\\€", "EUR", tidy_horror$currency)
tidy_horror$currency <- gsub("RUR", "RUB", tidy_horror$currency) #1 RUB == 1000 RUR (Old Russian Ruble obsolete ca. 1998 - from coinmill.com) - change to get updated currency value
tidy_horror$currency <- gsub("TRL", "TRY", tidy_horror$currency) #1 TRY == 1000000 TRL (Old Turkis Lire obsolete ca. 2005 - from coinmill.com) - change to get updated currency value
tidy_horror$currency <- str_remove_all(tidy_horror$currency, "\\s") #compulsively remove the spaces
# End

currency = unique(tidy_horror$currency)[-1]

currencies <- data.frame(
                  currency = unique(tidy_horror$currency)[-1],
                  value = 1:27,
                  stringsAsFactors = FALSE)

#create table with all the currencies 
for(i in 1:nrow(currencies)){
  s_string <- paste0(currencies$currency[i],"/" ,"USD") 
  s_string2 <- paste0(currencies$currency[i] ,"USD") 
  getFX(s_string)
  currencies$value[i] <- tail(eval(as.name(s_string2)),1)
}

#Adding USD
currencies <- rbind(currencies, data.frame(currency = 'USD', value = 1))

#removing just numbers
tidy_horror$budget.numbers <- gsub('[a-zA-Z$,€£ ]', '',tidy_horror$budget)

#removing all white space, note the double brackets
tidy_horror$budget.numbers <- gsub("[[:space:]]", "", tidy_horror$budget.numbers)

#convert budgets to usd
for(i in 1:nrow(tidy_horror)){
  for(n in 1:nrow(currencies)){
      if(tidy_horror$currency[i] == currencies$currency[n] ){
        m <- n }
  }
  tidy_horror$budget.in.USD[i] <- as.double(tidy_horror$budget.numbers[i])*as.double(currencies$value[m])
}

#Top 10 by budget 
top_budget <- tidy_horror %>%
  group_by(release_country) %>%
  summarise(total = sum(budget.in.USD)) %>%
  top_n(10) %>%
  arrange(desc(total))

#Top 10 producer 
best_producer <- horror_movies %>%
  group_by(release_country) %>%
  summarise(count = n()) %>%
  top_n(10) %>%
  arrange(desc(count))

top_budget$total <- as.integer(top_budget$total/1000)

packing <- circleProgressiveLayout(top_budget$total, sizetype='area')
data <- cbind(top_budget, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "white", alpha = 0.6) +
  scale_fill_manual(values = viridis(nrow(data))) +
  geom_text(data = data, aes(x, y, size=total, label = release_country)) +
  scale_size_continuous(range = c(1,24)) +
  theme_void() + 
  theme(legend.position="none") +
  coord_equal() +
  labs(
    title ="Which are the countries that spend more money producing horror movies?",
    subtitle = "The countries with majors inversions are the USA (3.287 M), UK (91M), and Spain(52).",
    caption = "Plot: Florencia Mangini (@manginiflor)\nsource: IMBd for Tidytuesday")


top_budget$total1 = (top_budget$total/1000000) 

ggplot(top_budget,aes(x=reorder(release_country, -total1), y = total1, fill=release_country))+ 
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.position="none")+
  labs(
    title ="Total inversion in U$S by countries",
    subtitle = "The countries with majors inversions are the USA (3.287 M), UK (91M), and Spain(52).",
    caption = "Plot: Florencia Mangini (@manginiflor)\nsource: IMDb for Tidytuesday",
    x="Country", y="Total budgets per country (u$s Millons)") 

