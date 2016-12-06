shinyUI(navbarPage('Presidential Election 2016',
                   #Panel Page
                   tabPanel('Predicting Election',
                            #Title page
                            titlePanel('What Could Change the Election Result'),
                            sidebarLayout(
                              #sidebars
                              sidebarPanel(
                                #Dropdown Menu
                                selectInput('state', label = 'Choose a State', choices = list("alabama", "washington")),
                               
                                #Select a county
                                #selectInput('county', label = 'Choose a County', choices = list("all", state_coloring$subregion)),
                                
                                #Button Menu
                                radioButtons('candidate', label = 'Choose a Candidate', choices = list('H. Clinton' = "D. Trump", 'D. Trump' = "H. Clinton"))
                                
                              ),
                              
                              #Exporting the input value
                              mainPanel(
                                plotOutput('map')
                              )
                            )
                   )
                   
))
