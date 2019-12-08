library(tidyverse)
library(osmdata)

tickets <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

library(DataExplorer)
DataExplorer::create_report(tickets)

# Tidy data: where is the money?
zip_by_fine <- tickets %>%
  select(violation_desc, zip_code,fine) %>%
  filter(!is.na(zip_code)) %>%
  group_by(zip_code) %>%
  summarise(total_fine = sum(fine)) %>%
  arrange(desc(total_fine)) %>%
  top_n(10)
#zip which apport more money: 19103 & 19107 

zip_by_fine_type <- right_join(tickets, zip_by_fine, by = c("zip_code" = "zip_code") )

zip_by_fine_type <- zip_by_fine_type %>%
  select(violation_desc, zip_code,fine) %>%
  filter(!is.na(zip_code)) %>%
  group_by(zip_code,violation_desc) %>%
  summarise(total_fine = sum(fine)) %>%
  arrange(desc(total_fine)) 
  
#stacked bar
ggplot(zip_by_fine_type, aes(fill=violation_desc, y=total_fine, x=zip_code)) + 
  geom_bar(position="fill", stat="identity")



#zip which apport more money: 19103 & 19107 

#definitely not the most tidy strategy to use
zip_by_fine <- zip_by_fine[-1,]

tidy_data_19103 <- tickets %>%
  select(violation_desc, zip_code,fine) %>%
  filter(zip_code == 19103) %>%
  group_by(violation_desc) %>%
  summarise(total_fine = sum(fine)) %>%
  arrange(desc(total_fine))
#In that specific zip code, which are the violations which apport more money to the city   

tidy_data_19107 <- tickets %>%
  select(violation_desc, zip_code,fine) %>%
  filter(zip_code == 19107) %>%
  group_by(violation_desc) %>%
  summarise(total_fine = sum(fine)) %>%
  arrange(desc(total_fine))


