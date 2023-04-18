QuizTab <- tabItem(
    tabName = "quiz",
    
    fluidRow(
        column(
            width = 12,
            box(
                # box appearance settings 
                width = 12,
                title = "What Bird Is This?",
                background = "black",
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "primary",
                # image of bird
                htmlOutput("quizImage"),
                textOutput("quizScore"),
                radioButtons("guess", "", choices = c(1,2,3,4,5)),
                
                actionButton("quizSubmit", label = "Submit"),
                actionButton("resetQuiz", label = "Reset")
                
            )
        )
    )
)