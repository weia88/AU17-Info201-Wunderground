library(shiny)
library(dplyr)
library(tidyr)
source('analysis.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    render10DayPlot <- renderPlotly({
      # Filter data with state and city selection.
      forecast1 <- GetForecast(values$initial.state, values$initial.city)
      forecast2 <- GetForecast(values$final.state, values$final.city)
      
      # Early exit if invalid name detected
      if(forecast1 == "Error" || forecast2 == "Error"){
        showNotification("City not found", type = "error", duration = 2, closeButton = TRUE)
        return ()
      }
      
      GenerateAnalysis(forecast1, forecast2)
      
      location1.data <- GetValues(forecast1)
      location2.data <- GetValues(forecast2)
    
      # Make Area Graph
      # 1st City Plot
      plot_ly(location1.data, x = ~date.day, y = ~high.fahrenheit, type = 'scatter', mode = 'lines',
              line = list(color = 'blue'), name = paste(values$initial.city, 'Highs'), hoverinfo = 'text',
              text = ~paste(values$initial.city,
              '<br> High: ', high.fahrenheit)) %>%
      add_trace(y = ~low.fahrenheit, type = 'scatter', mode = 'lines', fill = 'tonexty',
                fillcolor = 'rgba(0,0,255,0.2)', line = list(color = 'blue'),
                name = paste(values$initial.city, 'Lows'),  hoverinfo = 'text',
                text = ~paste(values$initial.city,
                '<br> Low: ', low.fahrenheit)) %>%
        
      # 2nd City Plot
      add_trace(x = ~date.day, y = ~location2.data$low.fahrenheit, type = 'scatter', mode = 'lines',
                line = list(color = 'red'), name = paste(values$final.city, 'Lows'),
                hoverinfo = 'text',
                text = ~paste(values$final.city,
                '<br> Low: ', location2.data$low.fahrenheit)) %>%
      add_trace(y = ~location2.data$high.fahrenheit, type = 'scatter', mode = 'lines', fill = 'tonexty',
                fillcolor = 'rgba(255, 0, 0, 0.2)', line = list(color = 'red'),
                name = paste(values$final.city, 'Highs'), 
                hoverinfo = 'text',
                text = ~paste(values$final.city,
                '<br> High: ', location2.data$high.fahrenheit)) %>%
        
        
      layout(title = '10-Day Temperature Comparison',
             xaxis = list(title = 'Day of Month', showgrid = TRUE, showline = FALSE, showticklabels = TRUE, dtick = 1),
             yaxis = list(title = 'Temperature (degrees F)', showgrid = TRUE, showline = FALSE,
              showticklabels = TRUE, tick.vals = 4)) %>% 
      layout(margin = list(l = 50, r = 40 , b = 60, t = 50, pad = 3))
    })
    
    # To render historical data
    render.historical.plot <- renderPlotly({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      progress$set(message = "Generating plots", value = 0)
      location1.data <- GetHistoricalData(values$initial.state, values$initial.city, progress)
      location2.data <- GetHistoricalData(values$final.state, values$final.city, progress)

      if(location1.data == "Error" || location2.data == "Error"){
        return ()
      }

      lowlist1 <- as.vector(location1.data$low)
      highlist1 <- as.vector(location1.data$high)
      lowlist2 <- as.vector(location2.data$low)
      highlist2 <- as.vector(location2.data$high)
      
      # 1st City Plot
      # Make seasons usable on x axis
      monthNames <- c("Spring", "Summer", "Autumn", "Winter")
      # From analysis.R
      months <- factor(months, levels = months, labels = monthNames)
      GenerateHistoricalAnalysis(location1.data, location2.data)
      RenderPrecipPlot(location1.data, location2.data)
      plot_ly(location1.data, x = months, y = highlist1, type = 'scatter', mode = 'lines',
              line = list(color = 'blue'), name = paste(values$initial.city, 'Highs'), hoverinfo = 'text',
              text = ~paste(values$initial.city,
                            '<br> High: ', highlist1)) %>%
        add_trace(y = lowlist1, type = 'scatter', mode = 'lines', fill = 'tonexty',
                  fillcolor = 'rgba(0,0,255,0.2)', line = list(color = 'blue'),
                  name = paste(values$initial.city, 'Lows'),  hoverinfo = 'text',
                  text = ~paste(values$initial.city,
                                '<br> Low: ', lowlist1)) %>%

        # 2nd City Plot
        add_trace(x = months, y = highlist2, type = 'scatter', mode = 'lines',
                  line = list(color = 'red'), name = paste(values$final.city, 'Lows'),
                  hoverinfo = 'text',
                  text = ~paste(values$final.city,
                                '<br> Low: ', highlist2)) %>%
        add_trace(y = lowlist2, type = 'scatter', mode = 'lines', fill = 'tonexty',
                  fillcolor = 'rgba(255, 0, 0, 0.2)', line = list(color = 'red'),
                  name = paste(values$final.city, 'Highs'),
                  hoverinfo = 'text',
                  text = ~paste(values$final.city,
                                '<br> High: ', lowlist2)) %>%


        layout(title = 'Historical Temperature Comparison',
               xaxis = list(title = 'Season', showgrid = TRUE, showline = FALSE, showticklabels = TRUE, dtick = 1),
               yaxis = list(title = 'Temperature (degrees F)', showgrid = TRUE, showline = FALSE,
                            showticklabels = TRUE, tick.vals = 4)) %>% 
        layout(margin = list(l = 50, r = 40 , b = 60, t = 50, pad = 3))
    })
    
    # Show precipitation (historical, avg) information
    RenderPrecipPlot <- function(data1, data2){
      output$precip.plot <- renderPlotly({
      precip1 <- data1$precip
      precip2 <- data2$precip

      seasonNames <- c("Spring", "Summer", "Autumn", "Winter")
      months <- factor(months, levels = months, labels = seasonNames)
      plot_ly(data1, x = months, y = as.numeric(precip1), type = 'scatter', mode = 'lines',
              line = list(color = 'blue'), name = paste(values$initial.city, 'Avg'), hoverinfo = 'text',
              text = ~paste(values$initial.city,
                            '<br> Avg: ', precip1, "in")) %>%
        add_trace(x = months, y = as.numeric(precip2), type = 'scatter', mode = 'lines',
                  line = list(color = 'red'), name = paste(values$final.city, 'Avg'),
                  hoverinfo = 'text',
                  text = ~paste(values$final.city,
                                '<br> Avg: ', precip2, "in")) %>%
        layout(title = 'Historical Precipitation Comparison (Avg)',
               xaxis = list(title = 'Season', showgrid = TRUE, showline = FALSE, showticklabels = TRUE, dtick = 1),
               yaxis = list(title = 'Precipitation (in)', showgrid = TRUE, showline = FALSE,
                            showticklabels = TRUE, tick.vals = 4)) %>% 
        layout(margin = list(l = 50, r = 40 , b = 60, t = 50, pad = 3))
      })
    }
    
    GenerateAnalysis <- function(forecast1, forecast2){
      output$analysis.text <- renderText({
        data <- GetCityStats(forecast1$low.fahrenheit, forecast1$high.fahrenheit, forecast2$low.fahrenheit, forecast2$high.fahrenheit)
        
        diff.pop <- round(abs(mean(forecast1$pop) - mean(forecast2$pop)), 1)
        diff.humid <- round(abs(mean(forecast1$avehumidity) - mean(forecast2$avehumidity)), 1)
        text <- paste0("Comparing ", values$initial.city, " and ", values$final.city, ", ") %>%
          paste0(GenerateHighLowAnalysis(data)) %>% paste0("For the next ten days, ", ifelse(mean(forecast1$pop) > mean(forecast2$pop), values$initial.city, valuesfinal.city)) %>%
          paste0(" will be more likely to rain by ", diff.pop, "%, and ") %>%
          paste0(ifelse(mean(forecast1$pop) > mean(forecast2$pop), values$initial.city, valuesfinal.city), " will be more humid") %>%
          paste0(" by ", diff.humid, "%.")
          
        c("<div id='analysis'><h2>Analysis</h2>", text, "</div>")
      })
    }
    
    GetCityStats <- function(lows1, highs1, lows2, highs2){
      low.sd1 <- sd(lows1)
      low.sd2 <- sd(lows2)
      high.sd1 <- sd(highs1)
      high.sd2 <- sd(highs2)
      higher.low <- ifelse(low.sd1 > low.sd2, round(low.sd1), round(low.sd2))
      higher.low.city <- ifelse(low.sd1 > low.sd2, values$initial.city, values$final.city)
      higher.high <- ifelse(high.sd1 > high.sd2, round(high.sd1), round(high.sd2))
      higher.high.city <- ifelse(high.sd1 > high.sd2, values$initial.city, values$final.city)
      return (data.frame(low.sd1, low.sd2, high.sd1, high.sd2, higher.low, higher.low.city, higher.high, higher.high.city))
    }
    
    GenerateHighLowAnalysis <- function(data){
      return (paste0(data$higher.low.city, " will have more unpredictable lows", " with a standard deviation of ") %>%
        paste0(data$higher.low, " degrees F. ", data$higher.high.city, " will have more unpredictable highs") %>%
        paste0(" with a standard deviation of ", data$higher.high, " degrees F.") %>% paste0("<br>"))
    }
    
    GenerateHistoricalAnalysis <- function(location.data1, location.data2){
      output$historical.analysis.text <- renderText({
        data <- GetCityStats(as.vector(location.data1$low), as.vector(location.data1$high), as.vector(location.data2$low), as.vector(location.data2$high))
        text <- paste0("Historically, ", GenerateHighLowAnalysis(data))
        c("<div id='analysis'>", text, "</div>")
      })
    }
  
    
    # Shows the alert if an area has an alert
    observeEvent(input$show.alert, ignoreNULL = TRUE, { 
      showNotification(GetAlert(input$final.state, input$final.city), type = "error", duration = 10, closeButton = TRUE)
      })
    
    # Display about information
    observeEvent(input$about.button, ignoreNULL = TRUE, {
      showNotification(about.description, id = "id", type = "message", duration = 100, closeButton = TRUE)
    })
    
    # Use reactive values so the plots only update once a button is pressed
    values <- reactiveValues(variable = NA)
    observe({
      if(input$compare.button > 0){
        values$initial.city <- isolate(input$initial.city)
        values$initial.state <- isolate(input$initial.state)
        values$final.city <- isolate(input$final.city)
        values$final.state <- isolate(input$final.state)
      }
    })
    
    # Render plots when compare button clicked
    observeEvent(input$compare.button, ignoreNULL = TRUE, {
        output$line.plot <- render10DayPlot
        output$historical.plot <- render.historical.plot
    })
})

