library(tidyverse)
library(RColorBrewer)
library(reshape)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")
tidy_sq <- nyc_squirrels

#tidy dataframe
tidy_sq <- transform(tidy_sq, hectare_v = substr(hectare, 1, 2), hectare_h = substr(hectare, 3,3))

tidy_sq_max <- tidy_sq %>%
  select(hectare_h, hectare_v, shift, date, hectare_squirrel_number,date) %>%
  group_by(hectare_h, hectare_v, shift, date) %>%
  summarise(max_value = max(hectare_squirrel_number)) %>%
  arrange(desc(date))

tidy_AMPM <- tidy_sq_max %>%
  group_by(hectare_h, hectare_v, shift, date) %>%
  summarise(total = sum(max_value)) %>% 
  arrange(desc(date))  %>% 
  group_by(hectare_h, hectare_v,date) %>% 
  summarize(totalAMPM = as.integer(sum(total))) 

# hectare_h hectare_v     date        totalAMPM
# <chr>     <chr>        <dbl>        <int>
# 1 A         01        10072018         4
# 2 A         01        10142018         7

saveGIF({
  for(i in c(10062018,10072018)){
    #    for(i in c(1997,2002,2007)){
    print(ggplot(tidy_AMPM %>% filter(date == 10062018),
                 aes(x = hectare_h, y = hectare_v)) +
            geom_tile(aes(fill = totalAMPM)) +
            theme_bw() +
            scale_y_discrete(drop = F) +
            theme(legend.position="top", plot.title = element_text(size=30, face="bold",hjust = 0.5))+
            coord_cartesian(xlim = c(20,85), ylim = c(0,21)) +
            ##scale_fill_manual("%",values = c("#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#bd0026","#800026"),drop=FALSE)+
            annotate(x=80, y=3, geom="text", label=i, size = 7) +
            annotate(x=80, y=1, geom="text", label="@iamreddave", size = 5) +
            ylab("Income") +   # Remove x-axis label
            xlab("Life Expenctancy")+
            ggtitle("Worldwide Life Expectancy and Income")          
          
    )
  }
}, interval=1.0,ani.width = 900, ani.height = 600)

tidy_AMPM_wide <- cast(tidy_AMPM, hectare_h ~ hectare_v)

# heatmap requieres a numerical matrix, for that reason we will move the names of the team as row.names 
# and after that, we will delete the column "Team"
row.names(tidy_AMPM_wide) <- tidy_AMPM_wide$hectare_h
tidy_AMPM_wide <- tidy_AMPM_wide[,-1]

#Creating the heatmap
#heatmap function requires a matrix as input
tidy_matrix <- data.matrix(tidy_AMPM_wide)

gplots::heatmap.2 (tidy_matrix,
                  Rowv = FALSE,
                  Colv = FALSE,
                  main = "Squirrel Map", # heat map title,
                  scale="none", 
                  key=FALSE,
                  dendrogram = "none",
                  margins =c(12,9),     # widens margins around plot
                  col= viridis::viridis_pal(),
                  density.info = "none",
                  colsep=1:ncol(tidy_matrix), # Add vertical grid lines
                  rowsep=1:nrow(tidy_matrix), # Add horizontal grid lines
                  sepcolor = "white", # Color gridlines black
                  trace = "none") 


library(ggplot2)
library(maps)
library(ggthemes)

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()

map <- world +
  geom_point(aes(x = lon, y = lat,
                 text = paste('city: ', location,
                              '<br /> created : ', created_at),
                 size = followers),
             data = rladies, colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8), breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers')




#Function to create the polygon for each hexagon
Hexagon <- function (x, y, unitcell = 1, col = col) {
  polygon(c(x, x, x + unitcell/2, x + unitcell, x + unitcell,
            x + unitcell/2), c(y + unitcell * 0.125,
                               y + unitcell * 0.875,
                               y + unitcell * 1.125,
                               y + unitcell * 0.875,
                               y + unitcell * 0.125,
                               y - unitcell * 0.125),
          col = col, border=NA)
}#function

x <- as.vector(tidy_matrix)

#Number of rows and columns of your SOM
SOM_Rows <- dim(tidy_matrix)[1]
SOM_Columns <- dim(tidy_matrix)[2]

#To make room for the legend
par(mar = c(0.4, 2, 2, 7))

#Initiate the plot window but do show any axes or points on the plot
plot(0, 0, type = "n", axes = FALSE, xlim=c(0, SOM_Columns),
     ylim=c(0, SOM_Rows), xlab="", ylab= "", asp=1)

ColRamp <- viridis::viridis(15)

ColorCode <- rep("#FFFFFF", length(x)) #default is all white
Bins <- seq(min(x, na.rm=T), max(x, na.rm=T), length=length(ColRamp))
for (i in 1:length(x))
  if (!is.na(x[i])) ColorCode[i] <- ColRamp[which.min(abs(Bins-x[i]))]

offset <- 0.5 #offset for the hexagons when moving up a row
for (row in 1:SOM_Rows) {
  for (column in 0:(SOM_Columns - 1))
    Hexagon(column + offset, row - 1, col = ColorCode[row + SOM_Rows * column])
  offset <- ifelse(offset, 0, 0.5)
}

dev.off()

#Add legend to the right if you want to
image.plot(legend.only=TRUE, col=ColRamp, zlim=c(min(x, na.rm=T), max(x, na.rm=T)))