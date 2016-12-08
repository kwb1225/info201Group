library(shiny)
library(dplyr)
library(plotly)
library(maps)
library(mapproj)
library(stringr)
library(ggplot2)
library(ggmap)
library(tidyr)
library(choroplethr)
library(choroplethrMaps)
library(reshape2)

source("../tab2working/datafunction.r")
source("../tab2working/tab4script.r")
source("../tab2working/tab1script.r")

shinyServer(function(input, output) {
  
  ############################################################
  
  output$map <- renderPlot({
    # If national is chosen, then the map shown is a map of the whole United States
    if (input$focus == "national") {
      curr.map <- county_choropleth(df, title = "Presidential Election 2016 By County", 
                                    legend = "Candidates", num_colors = 2)
      editedDF <- df
    } else { # Otherwise, it is a zoomed in map of the state they chose
      curr.map <- county_choropleth(df, state_zoom=tolower(input$focus), legend = "Candidates", num_colors=2) +
        ggtitle(c(input$focus, "Presidential Election 2016")) +
        coord_map()
      
      editedDF <- df %>% filter(state == input$focus)
    }
    
    # If checkbox is marked, show dataframe
    if (input$df.show == TRUE) {
      output$table <- renderDataTable(editedDF)
    }
    
    # Returns whichever map choice
    return(curr.map)
  })
  
  ######################################################################################
  
  output$select1 = renderUI({
    selectInput("county", "Choose Counties", choices = c("all", as.list(modified_votes[grepl(input$state, modified_votes$state), as.integer(7)])))
  })
  
  output$map <- renderPlot({
    return(election_map(input$state, input$county, input$candidate))
  })
  
  output$chart <- renderPlotly({
    return(election_chart(input$state, input$county, input$candidate))
  })
  
  output$bar <- renderPlotly({
    return(BuildBar(input$election))
  })
  
})
