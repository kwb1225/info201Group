library(shiny)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)
library(reshape2)

shinyServer(function(input, output) {
  
  output$map <- renderPlot({
  
    # Reads in the data into a .csv
    presidential.county.data <- read.csv("../data/presidential_general_election_2016_by_county_David.csv")

    # Creates a vector of the two candidates
    candidates <- c("H. Clinton", "D. Trump")
    
    # Creates a new dataframe of the filtere
    main.candidates <- presidential.county.data %>% 
      filter(name %in% candidates)
    
    # Creates a sub-list of National, and sub-list of States into one list of choices
    all.choices <- list("National" = "national", States = unique(main.candidates$state))
    
    # Condenses the data to get rid of duplicates
    main.candidates.condensed <- dcast(main.candidates, fips + geo_name + state ~ name, sum)
    
    # Renames the 'fips' column to 'region'
    names(main.candidates.condensed)[1] <- "region"
    
    # Renames the 'D. Trump' column to 'trump'
    names(main.candidates.condensed)[4] <- "trump"
    
    # Renames the 'H. Clinton' column to 'clinton'
    names(main.candidates.condensed)[5] <- "clinton"
    
    # Adds a new column that is the total amount of votes for Trump & Clinton    
    main.candidates.condensed <- mutate(main.candidates.condensed, total = trump + clinton)
    
    # Adds two new columns that is the percentages of votes for Trump and for Clinton
    main.candidates.condensed <- mutate(main.candidates.condensed, trump_pct = round((trump/total)*100), 
                               clinton_pct = round((clinton/total) * 100))
    
    # Sets the county names to lowercase to match the county.regions data
    main.candidates.condensed$geo_name <- tolower(as.character(main.candidates.condensed$geo_name))
    
    # Gets rid of 'county' and leaves only the name of the county to match the county.regions data
    main.candidates.condensed$geo_name <- gsub(' [A-z ]*', '' , main.candidates.condensed$geo_name)
    
    # Loads county.regions data
    data("county.regions")
    
    # Creates a new variable 'all.regions' that is only the county.names and fips codes
    all.regions <- select(county.regions, 'geo_name' = county.name, 'region' = region)
    
    # Joins all.regions with main.candidates.condensed
    df <- left_join(main.candidates.condensed, all.regions)
    
    names(df)[2] <- "county"
    
    # Sets the fips of Olgaga county to '46113' because it was the wrong fips code
    df[2383, "region"] <- 46113
    
    # Add a new column to show each county's winner
    df$winner <- as.factor(ifelse(df$trump_pct > df$clinton_pct, "Trump", "Clinton"))
    
    # Creates a column named 'value' that is the 'winner' column
    df$value = df$winner
    
    # If national is chosen, then the map shown is a map of the whole United States
    if (input$focus == "national") {
      curr.map <- county_choropleth(df, title = "Presidential Election 2016 By County", 
                                    legend = "Candidates", num_colors = 2)
      editedDF <- df
    } else { # Otherwise, it is a zoomed in map of the state they chose
      curr.map <- county_choropleth(df, state_zoom=tolower(input$focus), legend = "Candidates", num_colors=2) +
        ggtitle(c(input$focus, "Presidential Election 2016")) +
        coord_map()
      
      editedDF <- df %>% filter(state == input$focus)
    }
     
    # If checkbox is marked, show dataframe
    if (input$df.show) {
      output$table <- renderDataTable(editedDF)
    }
     
    # Returns whichever map choice
    return(curr.map)
  })
})