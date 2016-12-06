library(shiny)
counties_rds <- readRDS("data/counties.rds")
votes <- read.csv("data/presidential_general_election_2016_by_county.csv", stringsAsFactors = FALSE)


counties_test <- map_data("county")
states_test <- map_data("state")  


modified_votes <- votes %>%
  filter(rank == "1") %>%
  select(geo_name, name, rank, vote_pct, votes)

subregion <- tolower(modified_votes$geo_name) %>%
  str_replace_all(" county", "")

modified_votes <- modified_votes %>%
  mutate(subregion = subregion)

counties_test_new <- inner_join(counties_test, modified_votes, by = "subregion")

counties_plain <- select(counties_test_new, long, lat, group, region, subregion)

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

states_name_location <- aggregate(cbind(long, lat) ~ region, data = counties_test_new, FUN = function(x) mean(range(x)))
states_name_location <- as.data.frame(states_name_location)  

counties_name_location <- aggregate(cbind(long, lat) ~subregion, data = counties_test_new, FUN = function(x) mean(range(x)))
counties_name_location <- as.data.frame(counties_name_location)


shinyServer(function(input, output) {
  
  #output$selectUI <- renderUI({
   # selectInput("")
  #})
  #selectInput('county', label = 'Choose a County', choices = list("all", state_coloring$subregion))
  
  if(input$candidate = "D. Trump"){
    candidate_actual <- "H. Clinton"
  } else {
    candidate_actual <- "D. Trump"
  }
  
  if(input$county = "all"){
    counties_test_new[grepl(input$state), counties_test_new$region] <- str_replace_all(input$candidate, candidate_actual)
  } else {
    counties_test_new[grepl(input$county), counties_test_new$subregion] <- str_replace_all(input$candidate, candidate_actual)
  }
  
  output$map <- renderPlot({
    election_map <-   ggplot() +
      coord_map() +
      geom_polygon(data = counties_test_new, aes(x = long, y = lat, group = group, fill = name), color = "white") +
      scale_fill_manual(name = "Candidate", values = c("tomato1", "steelblue1")) +
      geom_polygon(data = states_test, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.5) +
      theme_nothing(legend = TRUE)
    
    return(election_map)
  })
})

