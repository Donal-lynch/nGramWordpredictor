library(shiny)

# source the prediction function
source('PredictFunc3.R')

shinyServer(function(input, output) {
   
    output$value <- renderText({
        if (input$caption == '') {
            return(NULL)
        }
        # Get the predictions
        nextWord <- nGramPredictor(input$caption)
        as.character(nextWord)
    })
    
    # Paste the JHU Ds image
    output$myImage <- renderImage({
        list(src = 'CourseraDSS.png',
             contentType = 'image/png',
             width = 350,
             height = 350,
             alt = "This is alternate text")
    }, deleteFile = FALSE)
    
    url <- a("here", href="http://rpubs.com/Donal_Lynch/WordPredictPres")
    output$tab <- renderUI({
        tagList("Information about this R Shiny app can be found", url)
    })
    
    
})




