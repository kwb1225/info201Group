library(shiny)
library(plotly)
shinyUI(navbarPage('2016 State Governor & Senator Election',
                   
                   # Create a tabPanel to show your scatter plot
                   tabPanel('Bar',
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
