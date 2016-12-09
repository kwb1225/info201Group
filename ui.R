library(shiny)

election <- read.csv('data/presidential_general_election_2016.csv', stringsAsFactors = FALSE)
election <- select(election, electoral_votes, rank, name, state, votes, vote_pct) %>% 
  filter(electoral_votes != 107) %>% 
  arrange(state)

choice <- group_by(election, state) %>% 
  filter(rank == 1) %>% 
  select(name, electoral_votes) %>% 
  mutate(selected = 1)

shinyUI(navbarPage("Presidential Election 2016", inverse = TRUE,
  
  tabPanel("Overview",
           # Names the map 
           titlePanel("General Overview of the 2016 Presidential Election"),
           
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
                 Although Donald Trump did win the election, Hillary Clinton was not far beind!"),
               
               br(),      
               
               p("If you look at the map above, you can view the election as a whole at a national level, 
                 or you can zoom in on a particular state and look at the state level by using the \"Select focus\" 
                 dropdown on the left. To see the specific dataset for the given view, select the \"Display data\"
                 checkbox"),
               
               br(),
               
               dataTableOutput('table')
               )
             )
  ),
  
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
  ),
  
  # Create a tabPanel to show your bar plot
  tabPanel('2016 State Governor & Senator Election',
           # Add a titlePanel to your tab
           titlePanel("2016 State Governor and Senator Election"),
           
           # Create a sidebar layout for this tab (page)
           sidebarLayout(
             
             # Create a sidebarPanel for your controls
             sidebarPanel(
               
               # Make a selectInput widget for user to choose between Governor and Senator election.
               selectInput('election', label = 'Governor or Senator', choices = list("Governor" = "Governor", "Senator" = "Senator")),
               
               "Choose either Governor or Senator to see each election's result.",
               br(), br(),
               "Showing only states which voted for the positions"
             ),
             # Create a main panel, in which you should display your plotly bar plot
             mainPanel(
               plotlyOutput('bar')
             )
           )
  )
  ))
