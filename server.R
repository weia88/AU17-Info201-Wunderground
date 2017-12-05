library(shiny)
library(dplyr)
library(tidyr)
source('analysis.R')

all.states <- read.csv(file = "data/states.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$line.plot <- renderPlotly({
      
      # Filter data with state and city selection.
      location1.data <- GetValues(GetForecast(input$initial.state, input$initial.city))
      location2.data <- GetValues(GetForecast(input$final.state, input$final.city))

      
      # Make Area Graph
      # 1st City Plot
      plot_ly(location1.data, x = ~date.day, y = ~high.fahrenheit, type = 'scatter', mode = 'lines',
              line = list(color = 'blue'), name = paste(input$initial.city, 'Highs'), hoverinfo = 'text',
              text = ~paste(input$initial.city,
              '</br> High: ', high.fahrenheit)) %>%
      add_trace(y = ~low.fahrenheit, type = 'scatter', mode = 'lines', fill = 'tonexty',
                fillcolor = 'rgba(0,100,80,0.2)', line = list(color = 'blue'),
                name = paste(input$initial.city, 'Lows'),  hoverinfo = 'text',
                text = ~paste(input$initial.city,
                '</br> Low: ', low.fahrenheit)) %>%
        
      # 2nd City Plot
      add_trace(x = ~date.day, y = ~location2.data$low.fahrenheit, type = 'scatter', mode = 'lines',
                line = list(color = 'red'), name = paste(input$final.city, 'Lows'),
                hoverinfo = 'text',
                text = ~paste(input$final.city,
                '</br> Low: ', location2.data$low.fahrenheit)) %>%
      add_trace(y = ~location2.data$high.fahrenheit, type = 'scatter', mode = 'lines', fill = 'tonexty',
                fillcolor = 'rgba(255, 0, 0, 0.2)', line = list(color = 'red'),
                name = paste(input$final.city, 'Highs'), 
                hoverinfo = 'text',
                text = ~paste(input$final.city,
                '</br> High: ', location2.data$high.fahrenheit)) %>%
        
        
      layout(title = 'Low and High Temperature Comparison',
             xaxis = list(title = 'Day', showgrid = TRUE, showline = FALSE, showticklabels = TRUE, dtick = 1),
             yaxis = list(title = 'Temperature (degrees F)', showgrid = TRUE, showline = FALSE,
              showticklabels = TRUE, tick.vals = 4),
            autosize = FALSE, width = 900, height = 500)
      
  
    })
})