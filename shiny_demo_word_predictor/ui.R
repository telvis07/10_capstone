library(shiny)
# TODO: https://www.youtube.com/watch?v=wFFfaKGVIdo
# TODO: Add HELP tab that describes the application
# TODO: Add number of recommended words
# TODO: make accuracy vs time slider. More accuracy, increases latency
# TODO: http://shiny.rstudio.com/articles/layout-guide.html
#     : add "append text buttons"
# TODO: App loading time

shinyUI(
  pageWithSidebar(
    headerPanel("Word Predictor Shiny Demo"),
    sidebarPanel(
      textInput(inputId="query", label="Enter Text", value="This class is ...")
      # actionButton("predict", label="Predict next word")
    ),
    mainPanel(
      p('Query Sentence'),
      verbatimTextOutput('query'),
      p('Most likely next word'),
      verbatimTextOutput('recommendation_1'),
      p('Second Most likely next word'),
      verbatimTextOutput('recommendation_2'),
      p('Third Most likely next word'),
      verbatimTextOutput('recommendation_3')
    )
  )
)