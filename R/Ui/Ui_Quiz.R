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
                imageOutput("quizImage")

            )
        )
    )
)