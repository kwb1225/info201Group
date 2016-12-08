#Setting Up
library(shiny)
library(dplyr)
library(plotly)
library(maps)
library(mapproj)
library(stringr)
library(ggplot2)
library(ggmap)

#Loading Data Sets
counties_rds <- readRDS("../data/counties.rds")
votes_states <- read.csv("../data/presidential_general_election_2016.csv", stringsAsFactors = FALSE)
votes_counties <- read.csv("../data/presidential_general_election_2016_by_county.csv", stringsAsFactors = FALSE)

#Importing Map Data
counties_test <- map_data("county")
states_test <- map_data("state")  

#Modifying Votes Into Hillary and Trump
modified_votes <- votes_counties %>%
  filter(rank == "1") %>%
  select(geo_name, name, rank, vote_pct, votes, state)

modified_votes$state <- tolower(modified_votes$state)

subregion <- tolower(modified_votes$geo_name) %>%
  str_replace_all(" county", "")

modified_votes <- modified_votes %>%
  mutate(subregion = subregion)

#Merging the Votes Data and Map Data
counties_test_new <- inner_join(counties_test, modified_votes, by = "subregion")

#Naming the Locations
states_name_location <- aggregate(cbind(long, lat) ~ region, data = counties_test_new, FUN = function(x) mean(range(x)))
states_name_location <- as.data.frame(states_name_location)  

counties_name_location <- aggregate(cbind(long, lat) ~subregion, data = counties_test_new, FUN = function(x) mean(range(x)))
counties_name_location <- as.data.frame(counties_name_location)


#Function for Map Generation
election_map <- function(state, county, candidate) {
  
  
  if(candidate == "normal"){ #If candidate not chosen
    p <- ggplot() +
      coord_map() +
      geom_polygon(data = counties_test_new, aes(x = long, y = lat, group = group, fill = name), color = NA) +
      scale_fill_manual(name = "Candidate", values = c("tomato1", "steelblue1")) +
      geom_polygon(data = states_test, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.5) +
      theme_nothing(legend = TRUE)
  } else {
    if(county == "all"){ #If the state is chosen
      counties_test_new[grepl(state, counties_test_new$region), as.integer(8)] <- candidate
      
      p <- ggplot() +
        coord_map() +
        geom_polygon(data = counties_test_new, aes(x = long, y = lat, group = group, fill = name), color = NA) +
        scale_fill_manual(name = "Candidate", values = c("tomato1", "steelblue1")) +
        geom_polygon(data = states_test, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.5) +
        theme_nothing(legend = TRUE) 
    } else { #If a county is chosen
      counties_test_new[grepl(county, counties_test_new$subregion), as.integer(8)] <- candidate
      
      p <- ggplot() +
        coord_map() +
        geom_polygon(data = counties_test_new, aes(x = long, y = lat, group = group, fill = name), color = NA) +
        scale_fill_manual(name = "Candidate", values = c("tomato1", "steelblue1")) +
        geom_polygon(data = states_test, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.5) +
        theme_nothing(legend = TRUE) 
    }
    
  }
  
  #Printing the Graph
  print(p)
}

#Function for Chart creation
election_chart <- function(state, county, candidate) {
  if(candidate == "normal"){ #If no candidate is chosen
    chart_data <- counties_test_new[!duplicated(counties_test_new$subregion), ]
    chart_data <- chart_data %>%
      group_by(name) %>%
      summarise(votes = sum(votes))
    
    q <- plot_ly(chart_data, x = ~name, y = ~votes, type = 'bar', marker = list(color = c('rgba(222, 45, 38, 0.8)', 'rgb(49,130,189)')))
  } else{
    if(county == "all") { #If a state is chosen
      counties_test_new[grepl(state, counties_test_new$region), as.integer(8)] <- candidate
      
      chart_data <- counties_test_new[!duplicated(counties_test_new$subregion), ]
      chart_data <- chart_data %>%
        group_by(name) %>%
        summarise(votes = sum(votes))
      
      q <- plot_ly(chart_data, x = ~name, y = ~votes, type = 'bar', marker = list(color = c('rgba(222, 45, 38, 0.8)', 'rgb(49,130,189)'))) #%>%

    } else{ #If a county is chosen
      counties_test_new[grepl(county, counties_test_new$subregion), as.integer(8)] <- candidate
      
      chart_data <- counties_test_new[!duplicated(counties_test_new$subregion), ]
      chart_data <- chart_data %>%
        group_by(name) %>%
        summarise(votes = sum(votes))
      
      q <- plot_ly(chart_data, x = ~name, y = ~votes, type = 'bar', marker = list(color = c('rgba(222, 45, 38, 0.8)', 'rgb(49,130,189)'))) #%>%
      
    }
    
  }
  
  return(q)
}
