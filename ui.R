library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Weather"),
  
  # Text input for initial city and drop down menu for all the states
    sidebarLayout(
      sidebarPanel(
        selectInput("initial.state", "State:", all.states, multiple = FALSE),
        textInput("initial.city", "City:", value = "") 
      ),
      
  # Text input for final destination city and drop down menu for all the states and also button for alert
    sidebarLayout(
      sidebarPanel(
        selectInput("final.state", "State:", all.states, multiple = FALSE),
        textInput("final.city", "City:", value = ""),
        actionButton("show.alert", "Show Alert", icon = NULL, width = NULL)
        ),
 
  # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("line.plot")
    )
  )
)
)
)