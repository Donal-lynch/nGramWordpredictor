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
  titlePanel('Data Science Capstone: Word Predictor'),
  p("When the JHU data science logo appears below, the app has loaded and ready to run"),
  
  # Sidebar with a slider input for number of bins
  # sidebarLayout(
  #   sidebarPanel(
  #      sliderInput("bins",
  #                  "Number of bins:",
  #                  min = 1,
  #                  max = 50,
  #                  value = 30)
  # ),
    
    mainPanel(
        imageOutput("myImage", height = '100%', inline = TRUE),
       
        textInput("caption", "Input text here", ""),
        verbatimTextOutput("value", placeholder = TRUE),
        
        
        uiOutput("tab")

    )
  # )
))
