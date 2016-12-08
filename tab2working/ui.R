library(shiny)
library(dplyr)
library(plotly)
library(maps)
library(mapproj)
library(stringr)

#counties_rds <- readRDS("data/counties.rds")

#votes_states <- read.csv("data/presidential_general_election_2016.csv", stringsAsFactors = FALSE)

#votes_counties <- read.csv("data/presidential_general_election_2016_by_county.csv", stringsAsFactors = FALSE)

source("../tabworking/datafunction.r")

shinyUI(navbarPage('Presidential Election 2016',
                   #Panel Page
                   tabPanel('Predicting Election',
                            #Title page
                            titlePanel('What Could Change the Election Result'),
                            sidebarLayout(
                              #sidebars
                              sidebarPanel(
                                #Dropdown Menu
                                selectInput('state', label = 'Choose a State', choices = as.list(states_name_location$region), selected = "normal"),
                               
                                #Button Menu
                                radioButtons('candidate', label = 'Choose a Candidate', choices = list('H. Clinton' = "H. Clinton", 'D. Trump' = "D. Trump", 'Default' = "normal"), selected = "normal")
                                
                              ),
                              
                              #Exporting the input value
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Map", plotOutput("map")),
                                            tabPanel("Histogram", plotlyOutput("chart"))
                                )
                              )
                            )
                   )
                   
))
