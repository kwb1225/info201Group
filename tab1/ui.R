library(shiny)

shinyUI(fluidPage(
  
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
))
