library(shiny)
library(dplyr)
library(tidyr)
source('analysis.R')

all.states <- read.csv(file = "data/states.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$line.plot <- renderPlotly({
      
      # Filter data with state and city selection.
      location1 <- GetForecast(input$initial.state, input$initial.city)
      location1.data <- GetValues(location1)
      location2 <- GetForecast(input$final.state, input$final.city)
      location2.data <- GetValues(location2)
      
      
      # Make Area Graph
      # 1st City Plot
      plot_ly(location1.data, x = ~date.day, y = ~high.fahrenheit, type = 'scatter', mode = 'lines',
              line = list(color = 'blue'), name = paste(location1, 'High Temperatures')) %>%
      add_trace(y = ~low.fahrenheit, type = 'scatter', mode = 'lines', fill = 'tonexty',
                fillcolor = 'rgba(0,100,80,0.2)', line = list(color = 'blue'),
                name = paste(location1, 'Low Temperatures')) %>%
        
      # 2nd City Plot
      add_trace(x = ~date.day, y = ~location2.data$low.fahrenheit, type = 'scatter', mode = 'lines',
                line = list(color = 'red'), name = paste(location2, 'High Temperatures')) %>%
      add_trace(y = ~location2.data$high.fahrenheit, type = 'scatter', mode = 'lines', fill = 'tonexty',
                fillcolor = 'rgba(255, 0, 0, 0.2)', line = list(color = 'red'),
                name = paste(location2, 'Low Temperatures')) %>%
        
        
      layout(title = 'Low and High Temperature Comparison',
             xaxis = list(title = 'Day'),
             yaxis = list(title = 'Temperature (degrees F)'))
      
  
    })
})