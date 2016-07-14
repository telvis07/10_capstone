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

      helpText("Help:"),
      helpText("Please type a sentence into the input box. The word predictions will appear on the
               right.", style = "color:purple"),
      br(),
      helpText("Note:"),
      helpText("The app must initialize when it first opens. 
               When intialization completes, you will see the predictions
               for the default sentence example \"This class is ...\"
               on the right side. "),
      br(),
      h6("This App was developed for:"),   
      a("Data Science Capstone 2016", href = "https://www.coursera.org/learn/data-science-project/home/welcome"),

      h6("By: "),
      a("Telvis Calhoun", href = "http://technicalelvis.com/blog/")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Prediction",
                 br(),
                 textInput(inputId="query", label="Enter Text", value="This class is ...", width='100%'),
                 
                 br(),
                 p('You entered the following text.'),
                 verbatimTextOutput('query'),
                 br(), br(),
                 p('Most likely next word'),
                 verbatimTextOutput('recommendation_1'),
                 p('Second Most likely next word'),
                 verbatimTextOutput('recommendation_2'),
                 p('Third Most likely next word'),
                 verbatimTextOutput('recommendation_3')
                 ),
          tabPanel("Documentation",
                   h3("Help:"),
                   helpText("Please type a sentence into the input box on the left. 
                            The word predictions will appear on the
                            right."),
                   br(),
                   h3("Background"),
                   p("In this project, we use `R` text mining tools to build a statistical model for
                      word sequences. The model will predict the next word as a user types - similar to the Swiftkey text messaging app. 
                      For example, when the user types: \"I went to the \" : the predictor 
                      should present options for what the next word might be. 
                     For example, the three words might be `gym, store, restaurant`."),
                   br(),
                   p("We train the model using text scraped from blogs, twitter and news sites. Then we predict the next word by 
                     calculating the maximum likelihood estimate (MLE) using ngram statistics"),
                   
                   br(),
                   h3("More Details"),
                   tags$ul(
                     tags$li(a("Capstone Milestone Report 1: Language Modeling and Text Prediction", 
                               href="http://rpubs.com/telvis/capstone_report_1")),
                     tags$li(a("Project Presentation", href="https://telvis07.github.io/slides_demo_word_predictor/")),
                     tags$li(a("Data Science Capstone Course", href="https://www.coursera.org/learn/data-science-project/home/welcome"))
                   )

        )            
      )
    )
  )
)