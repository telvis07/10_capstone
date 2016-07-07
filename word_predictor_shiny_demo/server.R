library(shiny)
source("word_predictor.R")

shinyServer(
  function(input, output){
    next_word_model.predict <- reactive({
      word_model_predict_query(input$query)
    })
    
    output$query <- renderPrint({input$query})
    output$recommendations <- renderPrint({next_word_model.predict()})
  }
)