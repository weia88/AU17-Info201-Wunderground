library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "style.css",

  # Application titles
  titlePanel("Weather Comparison"),
  fluidRow(
    column(2),
    column(1, br(), h5("From"), br(), br(), h5("To")),
    column(2, textInput("initial.city", "City:", value = "Seattle"),
           textInput("final.city", "City:", value = "Orange Cove")),
    column(2, selectInput("initial.state", "State:", all.states, multiple = FALSE, selected = "WA"),
           selectInput("final.state", "State:", all.states, multiple = FALSE, selected = "CA")),
    column(2, br(), actionButton("show.alert", "Show Alerts in Destination", icon = NULL, width = NULL),
           actionButton("compare.button", "Compare", icon = NULL, width = NULL))
    ),
    
  fluidRow(column(12, plotlyOutput("line.plot"), br())),
  fluidRow(column(12, plotlyOutput("historical.plot")))
  # Text input for initial city and drop down menu for all the states
  
  # Text input for final destination city and drop down menu for all the states and also button for alert
 
  # Show a plot of the generated distribution
    
    
  )
)

