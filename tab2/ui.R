library(shiny)
library(dplyr)
library(plotly)
library(maps)
library(mapproj)
library(stringr)


counties_rds <- readRDS("../data/counties.rds")
votes_states <- read.csv("../data/presidential_general_election_2016.csv", stringsAsFactors = FALSE)
votes_counties <- read.csv("../data/presidential_general_election_2016_by_county_David.csv", stringsAsFactors = FALSE)

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


shinyUI(navbarPage(inverse = TRUE, 'Presidential Election 2016',
                   #Panel Page
                   
                   tabPanel('Election Distribution',
                            #Title page
                            titlePanel('Changing the Election Results'),
                            sidebarLayout(
                              #sidebars
                              sidebarPanel(
                                #Dropdown Menu
                                selectInput('state', label = 'Choose a State', choices = as.list(states_name_location$region), selected = "normal"),
                               
                                #Interactive UI Output
                                uiOutput("select1"),
                                
                                #Button Menu
                                radioButtons('candidate', label = 'Who should Win?', choices = list('H. Clinton' = "H. Clinton", 'D. Trump' = "D. Trump", 'Default' = "normal"), selected = "normal")
                                
                              ),
                              
                              #Exporting the input value in tabs
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Distribution Map by Counties", p("The following map shows the distribution of presidential election votes by county. You can see the default data by choosing default, or alter the results by changing the state and its counties."), 
                                                     plotOutput("map")),
                                            tabPanel("Popular Votes Histogram", p("The followig histogram shows the balance of votes for both candidates. Choose default to view the overall balance, or alter the results by choosing a state and county, hence seeing the altered balance within that state."), 
                                                     plotlyOutput("chart"))
                                )
                              )
                            )
                   )
                   
                  
))
