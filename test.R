library(shiny)
library(dplyr)
library(plotly)
#install.packages("choroplethr")
#install.packages("choroplethrMaps")
library(choroplethr)
library(choroplethrMaps)
library(maps)
library(mapproj)
library(ggplot2)
library(ggmap)

.libPaths()

counties_rds <- readRDS("data/counties.rds")

#install.packages("mapproj")

?df_pop_county
data(df_pop_county)
county_choropleth(df_pop_county)

?county_choropleth

votes <- read.csv("data/presidential_general_election_2016_by_county.csv", stringsAsFactors = FALSE)

modified_votes <- votes %>%
  filter(name %in% c('D. Trump', 'H. Clinton')) %>%
  select(fips, geo_name, name, rank, state, votes, vote_pct)

trump_votes <- modified_votes %>%
  filter(name == 'D. Trump') %>%
  mutate(region = fips, value = votes, new = vote_pct) %>%
  select(region, value, new)

trump_votes <- unique(trump_votes)

?county.map

county <- map_data("county")


county_choropleth(trump_votes)

percent_map(county$order, "darkgreen", "test")



map("county", fill = TRUE, col = c("white", "darkgreen"), 
    resolution = 0, lty = 0, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0)
  
ggplot(data = counties_test) +
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "grey") + 
  coord_fixed(1.3)