library(shiny)
library(dplyr)
library(plotly)
library(maps)
library(mapproj)
library(stringr)
library(ggplot2)
library(ggmap)


source("../tab2working/tab2script.r")

shinyServer(function(input, output) {
  
  output$select1 = renderUI({
    selectInput("county", "Choose Counties", choices = c("all", as.list(modified_votes[grepl(input$state, modified_votes$state), as.integer(7)])))
  })
  
  output$map <- renderPlot({
    return(election_map(input$state, input$county, input$candidate))
  })
  
  output$chart <- renderPlotly({
    return(election_chart(input$state, input$county, input$candidate))
  })
  
 
})
