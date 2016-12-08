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
                   
                   tabPanel('Predicting Election',
                            #Title page
                            titlePanel('Mapping a Change in the Results'),
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
                                            tabPanel("Distribution Map by Counties", plotOutput("map")),
                                            tabPanel("Popular Votes Histogram", plotlyOutput("chart"))
                                )
                              )
                            )
                   )
                   
                  
))
