#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Weather"),
  
  # Text input for initial city and drop down menu for all the states
    sidebarLayout(
      sidebarPanel(
        selectInput("intial.state", "State:", all.states, multiple = FALSE),
        textInput("initial.city", "City:", value = "") 
      ),
      
  # Text input for final destination city and drop down menu for all the states
    sidebarLayout(
      sidebarPanel(
        selectInput("final.state", "State:", all.states, multiple = FALSE),
        textInput("final.city", "City:", value = "") 
        ),
  # Show a plot of the generated distribution
    mainPanel(
    )
  )
)
)
)