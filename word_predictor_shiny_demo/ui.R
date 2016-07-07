library(shiny)
# TODO: https://www.youtube.com/watch?v=wFFfaKGVIdo
# TODO: Add HELP tab that describes the application
# TODO: Add number of recommended words
# TODO: make accuracy vs time slider. More accuracy, increases latency

shinyUI(
  pageWithSidebar(
    headerPanel("Word Predictor Shiny Demo"),
    sidebarPanel(
      textInput(inputId="query", label="Enter Text"),
      actionButton("predict", label="Predict next word")
    ),
    mainPanel(
      p('Query Sentence'),
      verbatimTextOutput('query'),
      p('Recommended Next Words'),
      verbatimTextOutput('recommendations')
    )
  )
)