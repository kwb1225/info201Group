library(shiny)
library(dplyr)
library(plotly)
library(maps)
library(mapproj)
library(stringr)
library(ggplot2)
library(ggmap)

counties_rds <- readRDS("../data/counties.rds")

votes_states <- read.csv("../data/presidential_general_election_2016.csv", stringsAsFactors = FALSE)

votes_counties <- read.csv("../data/presidential_general_election_2016_by_county.csv", stringsAsFactors = FALSE)

counties_test <- map_data("county")
states_test <- map_data("state")  

modified_votes <- votes_counties %>%
  filter(rank == "1") %>%
  select(geo_name, name, rank, vote_pct, votes, state)

subregion <- tolower(modified_votes$geo_name) %>%
  str_replace_all(" county", "")

modified_votes <- modified_votes %>%
  mutate(subregion = subregion)

counties_test_new <- inner_join(counties_test, modified_votes, by = "subregion")

states_name_location <- aggregate(cbind(long, lat) ~ region, data = counties_test_new, FUN = function(x) mean(range(x)))
states_name_location <- as.data.frame(states_name_location)  

counties_name_location <- aggregate(cbind(long, lat) ~subregion, data = counties_test_new, FUN = function(x) mean(range(x)))
counties_name_location <- as.data.frame(counties_name_location)



election_map <- function(state, candidate) {
  if(candidate == "normal"){
    p <- ggplot() +
      coord_map() +
      geom_polygon(data = counties_test_new, aes(x = long, y = lat, group = group, fill = name), color = "white") +
      scale_fill_manual(name = "Candidate", values = c("tomato1", "steelblue1")) +
      geom_polygon(data = states_test, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.5) +
      theme_nothing(legend = TRUE)
  } else {
    counties_test_new[grepl(state, counties_test_new$region), as.integer(8)] <- candidate
    
    p <- ggplot() +
      coord_map() +
      geom_polygon(data = counties_test_new, aes(x = long, y = lat, group = group, fill = name), color = "white") +
      scale_fill_manual(name = "Candidate", values = c("tomato1", "steelblue1")) +
      geom_polygon(data = states_test, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.5) +
      theme_nothing(legend = TRUE)  
  }
  
  #Plotting the graph using the plotly functions
  print(p)
}


election_chart <- function(state, candidate) {
  if(candidate == "normal"){
    chart_data <- counties_test_new[!duplicated(counties_test_new$subregion), ]
    chart_data <- chart_data %>%
      group_by(name) %>%
      summarise(votes = sum(votes))
    
    q <- plot_ly(chart_data, x = ~name, y = ~votes, type = 'bar', marker = list(color = c('rgba(222, 45, 38, 0.8)', 'rgb(49,130,189)')))
  } else{
    counties_test_new[grepl(state, counties_test_new$region), as.integer(8)] <- candidate
    
    chart_data <- counties_test_new[!duplicated(counties_test_new$subregion), ]
    chart_data <- chart_data %>%
      group_by(name) %>%
      summarise(votes = sum(votes))
    
    q <- plot_ly(chart_data, x = ~name, y = ~votes, type = 'bar', marker = list(color = c('rgba(222, 45, 38, 0.8)', 'rgb(49,130,189)'))) #%>%
    #layout(yaxis = list(title = 'Total'), barmode = 'group')
  }
  
  return(q)
}
