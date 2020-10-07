##The following script creates an interactive map of countries coded with the categorical variable "trip_type"

#Packages required to run the script
library(countrycode)
library(rworldmap)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggthemes)
library(ggiraph)

#Get my country data ready
data <- read.csv("countries.csv", header = TRUE)

data$country_name <- countrycode(data$country_name, origin = 'country.name', destination = 'iso3c')

newdata <- data[-which(is.na(data)), ] %>%
  add_row(country_name = countrycode("United Kingdom", origin = 'country.name', destination = 'iso3c'), trip_type = "visit")

#Load map data and merge with my data
world <- ne_countries(scale = "medium", returnclass = "sf") 
newdata2 <- rename(newdata, iso_a3 = country_name)

world2 <- merge(world, newdata2, all = TRUE)

#Plot interactive map 

WorldMap3 <- ggplot(world2) +
  geom_sf_interactive(
    aes(fill = trip_type,
        tooltip = sprintf("%s<br/>%s", iso_a3, formal_en))) + #Added tooltip here
  theme_wsj(color = "white", base_family = "mono") + 
  ggtitle("Places I've been to", subtitle = paste0("(", sum(!is.na(world2$trip_type)), " countries)")) +
  scale_fill_manual(breaks = c("long_term", "visit", "transit", "NA"), 
                    values=c("#856C8B", "#8BABA2", "#A9A9A9", "#DDDDD"),
                    labels=c("Lived in", "Visited", "Traveled through", "Haven't visited yet")) +
  guides(fill=guide_legend(title=NULL))

WorldMap3 <- girafe(code, 
                    ggobj = WorldMap3,
                    options = list(opts_tooltip(use_fill = TRUE),
                                   opts_zoom(min = 1, max = 5),
                                   opts_toolbar(saveaspng = FALSE)))

#Create iframe to embed on web

