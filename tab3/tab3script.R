
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

choice <- group_by(election, state) %>% 
  filter(rank == 1) %>% 
  select(name, electoral_votes) %>% 
  mutate(selected = 1)%>%
  arrange(state)
