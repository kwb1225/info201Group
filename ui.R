library(shiny)
library(dplyr)
library(plotly)
library(maps)
library(mapproj)
library(stringr)


#counties_rds <- readRDS("data/counties.rds")

#votes_states <- read.csv("data/presidential_general_election_2016.csv", stringsAsFactors = FALSE)

#votes_counties <- read.csv("data/presidential_general_election_2016_by_county.csv", stringsAsFactors = FALSE)

source("./tab2working/tab2script.r")
source("./tab1/tab1script.r")
source("./tab3/tab3script.r")

shinyUI(navbarPage(inverse = TRUE, 'Presidential Election 2016',
                   #Panel Page
                  
                   tabPanel('Overview',
                            titlePanel("General Overview of the 2016 Presidential Election"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                
                                # Creates an input that allows the user to choose their choice of focus (National view or a specific state)
                                selectInput('focus', label = 'Select Focus (National or State)', choices = all.choices),
                                
                                # Creates a checkbox, if checked it will show the df used to create the map
                                checkboxInput("df.show", label = "Display data", value = FALSE)
                              ),
                              
                              # Creates a main panel and plots the map and data table (if checkbox is marked)
                              mainPanel(
                                plotOutput('map'),
                                
                                dataTableOutput('table')
                              )
                            )
                    ),
                   tabPanel('Changing the Results',
                            titlePanel("What if some states had different outcomes on the 2016 election?
             (Please don't select a state more than once)"),
                            
                            
                            # Sidebar with selectInputs with state names to change outputs in the election
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "state", label = "Choose a state to change", c("None", choice$state ), selected = "None"),
                                selectInput(inputId = "state1", label = "Choose another state to change", c("None", choice$state ), selected = "None"),
                                selectInput(inputId = "state2", label = "Choose a state to change", c("None", choice$state ), selected = "None"),
                                selectInput(inputId = "state3", label = "Choose another state to change", c("None", choice$state ), selected = "None"),
                                selectInput(inputId = "state4", label = "Choose another state to change", c("None", choice$state ), selected = "None"),
                                #Checkboxes to change the graph showcased in plot 2 and 3
                                checkboxInput(inputId = "show", label = "Display the election results for the selected states (check only if you have changed a state already)"),
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
                            
                    ),
                   
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
                   ),
                   tabPanel('More Information',
                            # Add a titlePanel to your tab
                            titlePanel("2016 State Governor and Senator Election"),
                            
                            # Create a sidebar layout for this tab (page)
                            sidebarLayout(
                              
                              # Create a sidebarPanel for your controls
                              sidebarPanel(
                                
                                # Make a selectInput widget for searching for a part of iris in your scatter plot
                                selectInput('election', label = 'Governor or Senator', choices = list("Governor" = "Governor", "Senator" = "Senator"))
                                
                                
                              ),
                              
                              # Create a main panel, in which you should display your plotly Scatter plot
                              mainPanel(
                                plotlyOutput('bar')
                                
                              )
                            )
                   )
                  
))
