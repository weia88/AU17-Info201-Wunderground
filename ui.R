library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application titles
  titlePanel("Weather"),
  
  # Text input for initial city and drop down menu for all the states
    sidebarLayout(
      sidebarPanel(
        selectInput("initial.state", "State:", all.states, multiple = FALSE, selected = "WA"),
        textInput("initial.city", "City:", value = "Seattle") 
      ),
      
  # Text input for final destination city and drop down menu for all the states
    sidebarLayout(
      sidebarPanel(
        selectInput("final.state", "State:", all.states, multiple = FALSE, selected = "CA"),
        textInput("final.city", "City:", value = "Orange Cove") 
        ),
  # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("line.plot")
    )
  )
)
)
)