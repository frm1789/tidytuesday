library(hrbrthemes)
library(dplyr)
library(ggplot2)


cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")
devtools::install_github("thebioengineer/tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load("2019-11-12")
tuesdata <- tidytuesdayR::tt_load(2019, week = 46)

cran_code <- tuesdata$loc_cran_packages

languages2 <- cran_code%>%
                group_by(pkg_name) %>%
                summarize(total_languages=n())%>%
                top_n(20) %>%
                arrange(desc(total_languages)) %>%
                group_by(total_languages) %>%
                summarize(name = toString(pkg_name)) %>%
                arrange(desc(total_languages)) %>%

languages2$name = factor(languages2$name, levels = c("httpuv", "sass", "igraph, seqminer", 
                                                     "osqp, RcppParallel, XML",
                                                     "cubature, fs, genepop, rmumps", 
                                                     "nloptr, RcppCWB, rgl, rJava, RJSONIO, slickR",
                                                     "BioInstaller, htmltidy, RMixtCompIO")) 

languages2 %>%
  ggplot() + aes(name, total_languages, label = total_languages, fill = name) +
  geom_col()  +
  viridis::scale_fill_viridis(
    name = "CRAN Packages", discrete = TRUE) +
  labs(
    x="Top packages written using several languages", y="Quantity of languages",
    title = "CRAN Packages written using more languages",
    subtitle = "Most of the packages are written in more of one language. (Or two, three... or even seventeen!)",
    caption = "Data source: CRAN Packages | Plot: @manginiflor") +
  theme_ft_rc() +
  theme(legend.position = "bottom",
        axis.text.x = element_blank())



  