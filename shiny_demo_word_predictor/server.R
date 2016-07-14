library(shiny)
source("word_predictor.R")

shinyServer(
  function(input, output){
    next_word_model.predict <- reactive({
      word_model_predict_query(input$query)
    })
    
    output$query <- renderPrint({input$query})
    output$recommendation_1 <- renderText({next_word_model.predict()[1]})
    output$recommendation_2 <- renderText({next_word_model.predict()[2]})
    output$recommendation_3 <- renderText({next_word_model.predict()[3]})
  }
)