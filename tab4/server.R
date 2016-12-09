library(shiny)
library(dplyr)
library(plotly)

# Reads in two datasets about 2016 Governor and Senator election.
data1 <- read.csv("../data/governor_general_election_2016.csv", stringsAsFactors = FALSE)
data2 <- read.csv("../data/senate_general_election_2016.csv", stringsAsFactors = FALSE)

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

# Renders a plotly object that returns a bar.
shinyServer(function(input, output) {
  output$bar <- renderPlotly({
    return(BuildBar(input$election))
  })
})