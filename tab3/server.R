

library(shiny)
library(plotly)
library(dplyr)
library(tidyr)

# Define server logic 
shinyServer(function(input, output) {
   
   #Reads data from the 2016 general presidential election into a dataframe
   election <- read.csv('data/presidential_general_election_2016.csv', stringsAsFactors = FALSE)
    
    #Creates dataframe "winner" by filtering only the winners of the election in each state,
    #selects the colums related to the winning candidate name and electoral votes for the given
    #state
    winner <- group_by(election, state) %>% 
      filter(rank == 1) %>% 
      select(name, electoral_votes) 
   

  #Embodies reactive variables and creates a pie chart with the correspondent electoral vote percentage
  #per candidate
  output$pie <- renderPlotly({
    
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
    final <- filter(final, alternate != 0)
    percandidate <- group_by(final, alternate) %>% 
      summarise(sum = sum(electoral_votes))
    
    colors <- c('red', 'blue')
    
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
       layout(title = 'Trump x Clinton (Electoral Votes after change)',
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
     
    
    
    
    
   
    
  })
  
  
  output$perstate <- renderPlotly({
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
        layout(title = 'Electoral Votes per State',
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
        layout(title = 'Popular Votes per State',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      
    }
      
      
    
      
  })

  #Output refering to the third party votes graph, and results per selected state graph
  output$thirdandresults <- renderPlotly({
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
      layout(yaxis = list(title = 'Votes'), barmode = 'stack', height = 320, title = 'Votes in the 2016 Presidential Election per selected state')
    
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
        layout(title = 'Third Party Votes per State (Percentage within the nation)',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    
       
      
      
       }
    
  })
  
})




