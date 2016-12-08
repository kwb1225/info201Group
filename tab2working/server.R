library(shiny)
library(dplyr)
library(plotly)
library(maps)
library(mapproj)
library(stringr)
library(ggplot2)
library(ggmap)

source("../tabworking/datafunction.r")

shinyServer(function(input, output) {
  
  output$map <- renderPlot({
    return(election_map(input$state, input$candidate))
  })
  
  output$chart <- renderPlotly({
    return(election_chart(input$state, input$candidate))
  })
  
})
