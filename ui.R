library(shiny)
library(dplyr)
library(plotly)
library(maps)
library(mapproj)
library(stringr)

election <- read.csv('data/presidential_general_election_2016.csv', stringsAsFactors = FALSE)
election <- select(election, electoral_votes, rank, name, state, votes, vote_pct) %>% 
  filter(electoral_votes != 107) %>% 
  arrange(state)

choice <- group_by(election, state) %>% 
  filter(rank == 1) %>% 
  select(name, electoral_votes) %>% 
  mutate(selected = 1)




counties_rds <- readRDS("data/counties.rds")
votes_states <- read.csv("data/presidential_general_election_2016.csv", stringsAsFactors = FALSE)
votes_counties <- read.csv("data/presidential_general_election_2016_by_county_David.csv", stringsAsFactors = FALSE)

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

shinyUI(navbarPage("Presidential Election 2016", inverse = TRUE,
  
  tabPanel("Overview",
           # Names the map 
           titlePanel("General Overview of the 2016 Presidential Election"),
           
           br(),
           
           sidebarLayout(
             sidebarPanel(
               
               # Creates an input that allows the user to choose their choice of focus (National view or a specific state)
               selectInput('focus', label = 'Select Focus (National or State)', choices = all.choices),
               
               "Toggle the button below to view the dataset used to create this map view.",
               
               br(), br(),
               
               "If you choose National, it shows the dataset for all states and counties.",
               
               br(), br(),
               
               "If you choose a specific state, it shows the dataset filtered down to specifically that state's counties.",
               
               # Creates a checkbox, if checked it will show the df used to create the map
               checkboxInput("df.show", label = "Display data", value = FALSE)
             ),
             
             # Creates a main panel and plots the map and data table (if checkbox is marked)
             mainPanel(
               plotOutput('map'),
               
               br(), br(),
               
               p("The 2016 Presidential Election was an election that the world will definitely remember.
                 The main Presidential candidate for the Republican party was Donald Trump (dark blue). 
                 The main Presidential candidate for the Democratic party was Hillary Clinton (light blue).
                 Although Donald Trump did win the election, Hillary Clinton was not far behind!"),
               
               br(),      
               
               p("If you look at the map above, you can view the election as a whole at a national level, 
                 or you can zoom in on a particular state and look at the state level by using the \"Select focus\" 
                 dropdown on the left. To see the specific dataset for the given view, select the \"Display data\"
                 checkbox."),
               
               br(),
               
               dataTableOutput('table')
               )
             )
  ),
  
  ###################
  
  
  ######################
  
  tabPanel("Examining Different Outcomes", 
           # Application title
           titlePanel("How Would Different States Outcomes affect the 2016 Presidential Election?"),
           
           br(),
           
           # Sidebar with selectInputs with state names to change outputs in the election
           sidebarLayout(
             sidebarPanel(
               "As you select a state, its electoral votes will go to the runner-up candidate in it.",
               "For example, if you select California, the pie chart (Fig.1) will change, transferring the 55 electoral votes of California from Clinton to Trump.",
               
               br(), br(),
               
               
               selectInput(inputId = "state", label = "Choose a state to change", c("None", choice$state ), selected = "None"),
               selectInput(inputId = "state1", label = "Choose another state to change", c("None", choice$state ), selected = "None"),
               selectInput(inputId = "state2", label = "Choose a state to change", c("None", choice$state ), selected = "None"),
               selectInput(inputId = "state3", label = "Choose another state to change", c("None", choice$state ), selected = "None"),
               selectInput(inputId = "state4", label = "Choose another state to change", c("None", choice$state ), selected = "None"),
               #Checkboxes to change the graph showcased in plot 2 and 3
               
               br(),
               
               "(Fig.2)",
               checkboxInput(inputId = "show", label = "Display the election results for the selected states"),
               
               strong("(Check only if you have changed a state already.)"),
               
               br(), br(),
               
               "(Fig.3)",
               checkboxInput(inputId = "show1", label = "Display electoral votes per state instead"),
               
               strong("(Please don't select a state more than once.)"),
               
               width = 4
             ),
             
             
             
             # Show a plot of the generated distribution
             
             mainPanel(
               
               plotlyOutput("pie"),
               
               br(), br(), 
               
               plotlyOutput("thirdandresults"),
               
               br(), br(),
               
               plotlyOutput("perstate")
               
             )
           )
  ),
  
  tabPanel('Election Distribution',
           #Title page
           titlePanel('Changing the Election Results'),
           
           br(),
           
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
                           tabPanel("Distribution Map by Counties", 
                                    br(), p("The following map shows the distribution of presidential election votes by county. You can see the default data by choosing default, or alter the results by changing the state and its counties."), 
                                    plotOutput("map1")),
                           tabPanel("Popular Votes Histogram", 
                                    br(), p("The following histogram shows the balance of votes for both candidates. Choose default to view the overall balance, or alter the results by choosing a state and county, hence seeing the altered balance within that state."), 
                                    plotlyOutput("chart"))
               )
             )
           )
  ),
  
  
  # Create a tabPanel to show your bar plot
  tabPanel('State Governor & Senator Election',
           # Add a titlePanel to your tab
           titlePanel("2016 State Governor and Senator Election"),
           
           br(), 
           
           # Create a sidebar layout for this tab (page)
           sidebarLayout(
             
             # Create a sidebarPanel for your controls
             sidebarPanel(
               
               # Make a selectInput widget for user to choose between Governor and Senator election.
               selectInput('election', label = 'Governor or Senator', choices = list("Governor" = "Governor", "Senator" = "Senator")),
               
               "Choose either Governor or Senator to see each election's result.",
               br(), br(),
               "This is showing only certains states that voted for the positions."
             ),
             # Create a main panel, in which you should display your plotly bar plot
             mainPanel(
               plotlyOutput('bar'),
               
               br(), 
               
               p("These are bar graphs of the 2016 State Governor and Senator Election.
                  They give a visualization of the amount of votes the Governor or Senator
                 received as well as the party and state they were running for.")
             )
           )
  )
  
  
  ))
