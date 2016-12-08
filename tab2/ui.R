library(shiny)
library(dplyr)
library(plotly)
library(maps)
library(mapproj)
library(stringr)


#counties_rds <- readRDS("data/counties.rds")

#votes_states <- read.csv("data/presidential_general_election_2016.csv", stringsAsFactors = FALSE)

#votes_counties <- read.csv("data/presidential_general_election_2016_by_county.csv", stringsAsFactors = FALSE)

source("../tab2working/tab2script.r")

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
