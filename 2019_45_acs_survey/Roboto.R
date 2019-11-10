# https://github.com/hrbrmstr/hrbrthemes/issues/18

library(hrbrthemes)
library(ggplot2)

# How to solve problem with Roboto Condensed
install.packages("extrafontdb")  # reset fonttable
devtools::install_github("hrbrmstr/hrbrthemes", force = TRUE)
hrbrthemes::import_roboto_condensed()
d <- read.csv(extrafont:::fonttable_file(), stringsAsFactors = FALSE)

# Example 
ggplot(mtcars, aes(mpg, wt)) +
  geom_point(color = ft_cols$yellow) +
  labs(x="Fuel efficiency (mpg)", y="Weight (tons)",
       title="Seminal ggplot2 scatterplot example",
       subtitle="A plot that is only useful for demonstration purposes",
       caption="Brought to you by the letter 'g'") + 
  theme_ft_rc()