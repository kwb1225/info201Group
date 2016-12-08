election <- read.csv('data/presidential_general_election_2016.csv', stringsAsFactors = FALSE)

#Creates dataframe "winner" by filtering only the winners of the election in each state,
#selects the colums related to the winning candidate name and electoral votes for the given
#state
winner <- group_by(election, state) %>% 
  filter(rank == 1) %>% 
  select(name, electoral_votes) 

choice <- group_by(election, state) %>% 
  filter(rank == 1) %>% 
  select(name, electoral_votes) %>% 
  mutate(selected = 1)