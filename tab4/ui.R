library(shiny)
library(plotly)
shinyUI(navbarPage('2016 State Governor & Senator Election',
                   
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
