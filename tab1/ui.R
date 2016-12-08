library(shiny)

shinyUI(fluidPage(
  
  # Names the map 
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
))
