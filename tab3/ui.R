

library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)

<<<<<<< 009f71749b93b8177a61b766c806727effa1937a

=======
<<<<<<< dc5273f238dadaf83307dcabe99c5d19307d8eb6
election <- read.csv('data/presidential_general_election_2016.csv', stringsAsFactors = FALSE)
election <- select(election, electoral_votes, rank, name, state, votes, vote_pct) %>% 
  filter(electoral_votes != 107) %>% 
  arrange(state)
=======
# Define UI for application 
shinyUI(fluidPage(
  
  
  election <- read.csv('../data/presidential_general_election_2016.csv', stringsAsFactors = FALSE),
>>>>>>> Finished Tab 1
  
  choice <- group_by(election, state) %>% 
    filter(rank == 1) %>% 
    select(name, electoral_votes) %>% 
<<<<<<< dc5273f238dadaf83307dcabe99c5d19307d8eb6
    mutate(selected = 1) 
>>>>>>> Finished Tab 1
  
# Define UI for application 
shinyUI(fluidPage(
  
=======
    mutate(selected = 1),
>>>>>>> Finished Tab 1
  
  tabPanel("Tab 3", 
  # Application title
  titlePanel("What if some states had different outcomes on the 2016 election?
             (Please don't select a state more than once)"),

  
  # Sidebar with selectInputs with state names to change outputs in the election
  sidebarLayout(
    sidebarPanel(
       "As you select a state, its electoral votes will go to the runner-up candidate in it.",
       "For example, if you select California, the pie chart (Fig.1) will change, transfering the 55 electoral vote of California from Clinton to Trump",
       selectInput(inputId = "state", label = "Choose a state to change", c("None", choice$state ), selected = "None"),
       selectInput(inputId = "state1", label = "Choose another state to change", c("None", choice$state ), selected = "None"),
       selectInput(inputId = "state2", label = "Choose a state to change", c("None", choice$state ), selected = "None"),
       selectInput(inputId = "state3", label = "Choose another state to change", c("None", choice$state ), selected = "None"),
       selectInput(inputId = "state4", label = "Choose another state to change", c("None", choice$state ), selected = "None"),
       #Checkboxes to change the graph showcased in plot 2 and 3
       " ",
       "(Fig.2)",
       checkboxInput(inputId = "show", label = "Display the election results for the selected states (check only if you have changed a state already)"),
       " ",
       "(Fig.3)",
       checkboxInput(inputId = "show1", label = "Display electoral votes per state instead"),
       
       width = 4
       ),
  
    
    
    # Show a plot of the generated distribution
    mainPanel(
       plotlyOutput("pie"),
       plotlyOutput("thirdandresults"),
       plotlyOutput("perstate")
       
    )
  )
))
