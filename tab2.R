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
#install.packages("ggrepel")
library(ggrepel)

counties_rds <- readRDS("data/counties.rds")

votes <- read.csv("data/presidential_general_election_2016_by_county.csv", stringsAsFactors = FALSE)

#map("county", fill = TRUE, col = c("white", "darkgreen"), 
    #resolution = 0, lty = 0, projection = "polyconic", 
    #myborder = 0, mar = c(0,0,0,0)
    
counties_test <- map_data("county")
states_test <- map_data("state")  

#ggplot(data = states_test, mapping = aes(x = long, y = lat, group = group)) + 
  #coord_fixed(1.3) + 
  #geom_polygon(color = "black", fill = "grey") +
  #theme_nothing() +
  #geom_polygon(data = counties_test, fill = NA, color = "white") +
  #geom_polygon(color = "black", fill = NA)

modified_votes <- votes %>%
  filter(rank == "1") %>%
  select(geo_name, name, rank, vote_pct, votes)

subregion <- tolower(modified_votes$geo_name) %>%
  str_replace_all(" county", "")

modified_votes <- modified_votes %>%
  mutate(subregion = subregion)

counties_test_new <- inner_join(counties_test, modified_votes, by = "subregion")

ditch_the_axes <- theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )

ggplot(data = states_test, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "grey") +
  theme_nothing() +
  geom_polygon(data = counties_test_new, aes(fill = name), color = NA) +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

states_name_location <- aggregate(cbind(long, lat) ~ region, data = counties_test_new, FUN = function(x) mean(range(x)))
states_name_location <- as.data.frame(states_name_location)  

counties_name_location <- aggregate(cbind(long, lat) ~subregion, data = counties_test_new, FUN = function(x) mean(range(x)))
counties_name_location <- as.data.frame(counties_name_location)

ggplot() +
  coord_map() +
  geom_polygon(data = counties_test_new, aes(x = long, y = lat, group = group, fill = name), color = "white") +
  scale_fill_manual(name = "Candidate", values = c("tomato1", "steelblue1")) +
  geom_polygon(data = states_test, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.5) +
  geom_text(data = states_name_location, aes(long, lat, label = region), size = 4, fontface = "bold") +
  theme_nothing(legend = TRUE)


ggplot() +
  coord_map() +
  geom_polygon(data = counties_test_new, aes(x = long, y = lat, group = group, fill = name), color = NA) +
  scale_fill_manual(name = "Candidate", values = c("tomato1", "steelblue1")) +
  geom_polygon(data = states_test, mapping = aes(x = long, y = lat, group = group), color = "white", fill = NA, size = 0.5) +
  #geom_text_repel(data = states_name_location, aes(long, lat, label = region)) +
  theme_nothing(legend = TRUE)

  #gom_polygon(data = states_test, aes(x = long, y = lat, group = group), color = "black", fill = "grey", size = 0.5) +
  
#hillary_votes <- votes %>%
 # filter(name == "H. Clinton") %>%
 # select(geo_name, name, rank, vote_pct, votes)

#trump_votes <- votes %>%
 # filter(name == "D. Trump") %>%
 # select(geo_name, name, rank, vote_pct, votes)