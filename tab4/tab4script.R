#data1 <- read.csv("data/governor_general_election_2016.csv", stringsAsFactors = FALSE)
#data2 <- read.csv("data/senate_general_election_2016.csv", stringsAsFactors = FALSE)

data1 <- read.csv("data/governor_general_election_2016.csv", stringsAsFactors = FALSE)
data2 <- read.csv("data/senate_general_election_2016.csv", stringsAsFactors = FALSE)


df.gov1 <- data1 %>% select(state, name, rank, individual_party, votes, vote_pct) %>%
  filter(rank == 1)
#colnames(df.gov1)[2:6] <- paste("Governor", colnames(df.gov1[,c(2:6)]), sep = '.')

df.sen1 <- data2 %>% select(state, name, rank, individual_party, votes, vote_pct) %>%
  filter(rank == 1)
#colnames(df.sen1)[2:6] <- paste("Senator", colnames(df.sen1[,c(2:6)]), sep = '.')

# Combines two datasets.
#data <- full_join(df.gov1, df.sen1)

#data <- as.data.frame(data)
# Builds a scatter plot which shows the corresponding chart with the user's input.
BuildBar <- function(election) {
  
  
  
  if (election == "Governor") {
    q <- plot_ly(data = df.gov1, x = ~state, y = ~vote_pct, type = "bar", color = ~individual_party, hoverinfo = 'text', 
                 text = ~paste(state, '</br>', name, '</br> Party: ', individual_party, '</br>', vote_pct, '%', '</br>', votes, 'votes')) %>%
      layout(title = "Governor Election", margin = list(b = 160), yaxis = list(title = "Votes in %", tickangle = 70))
  } else {
    q <- plot_ly(data = df.sen1, x = ~state, y = ~vote_pct, type = "bar", color = ~individual_party, hoverinfo = 'text', 
                 text = ~paste(state, '</br>', name, '</br> Party: ', individual_party, '</br>', vote_pct, '%', '</br>', votes, 'votes')) %>%
      layout(title = "Senator Election", margin = list(b = 160), yaxis = list(title = "Votes in %", tickangle = 70))
  }
  return(q)
}
