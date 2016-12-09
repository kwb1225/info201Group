library(shiny)
library(ggplot2)
library(reshape2)
library(plotly)
library(maps)
library(mapproj)
library(stringr)
library(ggmap)
library(tidyr)
library(dplyr)


shinyServer(function(input, output) {
  
  output$map <- renderPlot({
    library(choroplethrMaps)
    library(choroplethr)
    
    # Reads in the data into a .csv
    presidential.county.data <- read.csv("data/presidential_general_election_2016_by_county_David.csv")
    
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
    ##################################################
  
    #Reads data from the 2016 general presidential election into a dataframe
    election <- read.csv('data/presidential_general_election_2016.csv', stringsAsFactors = FALSE)
    election <- select(election, electoral_votes, rank, name, state, votes, vote_pct) %>% 
      filter(electoral_votes != 107) %>% 
      arrange(state)
    
    #Creates dataframe "winner" by filtering only the winners of the election in each state,
    #selects the colums related to the winning candidate name and electoral votes for the given
    #state
    winner <- group_by(election, state) %>% 
      filter(rank == 1) %>% 
      select(name, electoral_votes) %>% 
      arrange(state)
    
    
    #Embodies reactive variables and creates a pie chart with the correspondent electoral vote percentage
    #per candidate
    output$pie <- renderPlotly({
      detach("package:plotly", unload=TRUE)
      detach("package:dplyr", unload=TRUE)
      library(plotly)
      library(tidyr)
      library(dplyr)

      
      #Gets string values transmitted by input and set them in a dataframe
      plotchange <- as.data.frame(c(input$state, input$state1, input$state2, input$state3, input$state4))
      
      #Embodies reactive data from "plotchange" into dataframe containing winner candidates from each state,
      #and defines the dataframe for the plotly function to use to render a new graph
      colnames(plotchange) <- 'state'
      plotchange$selected <- 2 #selected states in input will have 2 as value for this colum
      change <- full_join(winner, plotchange, by = "state")
      change[is.na(change)] <- 0 #unchanged states (unselected in input) will have 0 as value for this colum
      difference <- as.data.frame(ifelse(change$selected == 2, 
                                         ifelse(grepl("D", change$name), 'H. Clinton', 'D. Trump'),
                                         change$name <- change$name))
      colnames(difference) <- 'alternate'
      difference$state <- change$state
      final <- full_join(change, difference, by = "state")
      final <- filter(final, alternate != 0) %>% 
        arrange(state)
      percandidate <- group_by(final, alternate) %>% 
        summarise(sum = sum(electoral_votes))
      
      colors <- c(toRGB('firebrick2'), 'blue')
      
      # Plots a graph taking in consideration the aggregate electoral votes per candidate at the
      #dataframe containing inputed changes
      plot_ly(percandidate, labels = ~alternate, values = ~sum, type = "pie",
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~sum,
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)),
              #The 'pull' attribute can also be used to create space between the sectors
              showlegend = TRUE) %>%
        layout(title = '(Fig.1) Trump x Clinton (Electoral Votes after change)',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      
      
      
      
      
      
    })

    output$perstate <- renderPlotly({
      detach("package:plotly", unload=TRUE)
      detach("package:dplyr", unload=TRUE)
      library(plotly)
      library(tidyr)
      library(dplyr)
      
      #if checkbox 2 is selected, a graph with the percentage of Electoral Votes per state will be plotted
      if(input$show1){
        
        
        plot_ly(winner, labels = ~state, values = ~electoral_votes, type = "pie",
                textposition = NULL,
                textinfo = 'none',
                insidetextfont = list(color = 'black'),
                hoverinfo = 'label+percent',
                text = ~electoral_votes,
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)),
                
                showlegend = TRUE) %>% 
          layout(title = '(Fig.3) Electoral Votes per State',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        # If checkbox 2 isn't selected, a graph with the percentage of popular vote per state will be plotted
      } else {
        
        #Assemble a dataframe from the main dataset election, with colums state, name and votes
        result <- group_by(election, state) %>% 
          select(name, votes) %>% 
          summarise(total = sum(votes))
        
        plot_ly(result, labels = ~state, values = ~total, type = "pie",
                textposition = NULL,
                textinfo = 'none',
                insidetextfont = list(color = 'black'),
                hoverinfo = 'label+percent',
                text = ~total,
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)),
                
                showlegend = TRUE) %>%
          layout(title = '(Fig.3) Popular Votes per State',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        
      }
      
      
      
      
    })
    
    #Output refering to the third party votes graph, and results per selected state graph
    output$thirdandresults <- renderPlotly({
      detach("package:plotly", unload=TRUE)
      detach("package:dplyr", unload=TRUE)
      library(plotly)
      library(tidyr)
      library(dplyr)
      
      ranks <- c(1, 2)
      #If checkbox 1 is selected, plot a graph with the results per selected state in selectInput 1 through 5
      if(input$show) {
        #Get reactive data from selectInput 1 through 5
        estados <- c(input$state, input$state1, input$state2, input$state3, input$state4)
        
        #Assemble a dataframe with the votes percentage of the top 2 candidates
        winners <- select(election, state, name, electoral_votes, rank, vote_pct) %>%    
          filter(state %in% estados, rank %in% ranks) 
        winners <- group_by(winners, state)
        winners <- select(winners, state, name, vote_pct)
        
        test <- spread(winners, name, vote_pct)
        colnames(test)[2] <- 'Trump'
        colnames(test)[3] <- 'Clinton'
        plot_ly(test, x = ~state, y = ~Trump, type = 'bar', name = 'Trump') %>%
          add_trace(y = ~Clinton, name = 'Clinton') %>%
          layout(yaxis = list(title = 'Votes'), barmode = 'stack', height = 320, title = '(Fig.2) Votes in the 2016 Presidential Election per selected state')
        
        #If the checkbox 1 isn't selected, plot a graph with the percentage of votes for third party candidates for all states
      } else {
        
        ranks1 <- c(3:22)
        
        losers <- select(election, state, name, vote_pct, rank) %>% 
          filter(rank %in% ranks1)
        
        losers1 <- select(losers, state, vote_pct) %>% 
          group_by(state) %>% 
          summarise(third_party = sum(vote_pct))  
        
        
        
        plot_ly(losers1, labels = ~state, values = ~third_party, type = "pie",
                textposition = NULL,
                textinfo = 'none',
                insidetextfont = list(color = 'black'),
                hoverinfo = 'label+percent',
                text = ~third_party,
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)),
                
                showlegend = TRUE) %>%
          layout(title = '(Fig.2) Third Party Votes per State (Percentage within the nation)',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
      }
      
    })
    
    ####################################################
    
    # Reads in two datasets about 2016 Governor and Senator election.
    data1 <- read.csv("data/governor_general_election_2016.csv", stringsAsFactors = FALSE)
    data2 <- read.csv("data/senate_general_election_2016.csv", stringsAsFactors = FALSE)
    
    # Manipulates datasets for easier use by selecting and filtering only necessary data.
    df.gov1 <- data1 %>% select(state, name, rank, individual_party, votes, vote_pct) %>%
      filter(rank == 1)
    
    df.sen1 <- data2 %>% select(state, name, rank, individual_party, votes, vote_pct) %>%
      filter(rank == 1)
    
    # Builds a bar plot which shows the corresponding chart with the user's input.
    BuildBar <- function(election) {
      
      if (election == "Governor") {
        q <- plot_ly(data = df.gov1, x = ~state, y = ~vote_pct, type = "bar", color = ~individual_party, hoverinfo = 'text', 
                     text = ~paste(state, '</br>', 'Elected:', name, '</br> Party: ', individual_party, '</br>', 'Votes percent:', vote_pct, '%', '</br>', 'Votes:', votes)) %>%
          layout(title = "State Governor Election", margin = list(b = 160), yaxis = list(title = "Votes in %", tickangle = 70))
      } else {
        q <- plot_ly(data = df.sen1, x = ~state, y = ~vote_pct, type = "bar", color = ~individual_party, hoverinfo = 'text', 
                     text = ~paste(state, '</br>', 'Elected:', name, '</br> Party: ', individual_party, '</br>', 'Votes percent:', vote_pct, '%', '</br>', 'Votes:', votes)) %>%
          layout(title = "State Senator Election", margin = list(b = 160), yaxis = list(title = "Votes in %", tickangle = 70))
      }
      return(q)
    }
    
    output$bar <- renderPlotly({
      return(BuildBar(input$election))
    })

    #####################################
    
    counties_rds <- readRDS("data/counties.rds")
    votes_states <- read.csv("data/presidential_general_election_2016.csv", stringsAsFactors = FALSE)
    votes_counties <- read.csv("data/presidential_general_election_2016_by_county_David.csv", stringsAsFactors = FALSE)
    
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
    
      output$select1 = renderUI({
        selectInput("county", "Choose Counties", choices = c("all", as.list(modified_votes[grepl(input$location, modified_votes$state), as.integer(7)])))
      })
      
      output$map1 <- renderPlot({
        return(election_map(input$location, input$county, input$candidate))
      })
      
      output$chart <- renderPlotly({
        detach("package:plotly", unload=TRUE)
        detach("package:dplyr", unload=TRUE)
        library(plotly)
        library(dplyr)
        
        return(election_chart(input$location, input$county, input$candidate))
      })
    
})
