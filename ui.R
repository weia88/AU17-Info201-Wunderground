library(shiny)
library(plotly)
source('analysis.R')

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    theme = "style.css",
    # Application titles
    titlePanel("Weather Comparison"),
    fluidRow(
      column(3),
      column(1, br(), h5("From:") , br(), br(), h5("To:")),
      column(3,
        textInput("initial.city", "City:", value = "Seattle"),
        textInput("final.city", "City:", value = "Sun City")
      ),
      # Text input for initial city and drop down menu for all the states
      column(2,
        selectInput(
          "initial.state", "State:", all.states, multiple = FALSE, selected = "WA"),
        # Text input for final destination city and drop down menu for all the states and also button for alert
        selectInput("final.state", "State:", all.states, multiple = FALSE, selected = "CA")
      )
    ),
    fluidRow(column(4),
             column(3,
               actionButton("show.alert", "Show Alerts in Destination", icon = NULL, width = '205px', style = 'height: 45px')
             ),
             column(1,
               actionButton("compare.button", "Compare", icon = NULL, width = '125px', style = 'height: 45px')
             )),
    # Show a plots of generated distributions
      
    hr(), 
    fluidRow(column(12, plotlyOutput("line.plot"))),
    hr(),
    fluidRow(column(12, plotlyOutput("historical.plot"))),
    hr(),
    fluidRow(column(12, plotlyOutput("precip.plot")))
  )
)
