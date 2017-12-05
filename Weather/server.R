library(shiny)
library(dplyr)
library(tidyr)
source('analysis.R')

all.states <- read.csv(file = "states.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  observeEvent(input$show.alert, {
    state.input <- as.vector(input$final.state)
    showNotification(GetAlert(state.input,input$final.city))
  })
})