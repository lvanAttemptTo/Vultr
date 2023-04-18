observeEvent(input$quizSubmit,
{
    if(as.integer(input$quizSubmit) == 1)
    {
        correct <<- 0
        incorrect <<- 0
    }
    
    output$quizScore <- renderText({
        # check previous answer
        if (as.integer(input$quizSubmit) != quizSubmit)
        {
            quizSubmit <<- as.integer(input$quizSubmit)
            guess <- input$guess
            
            if(guess == TRUE)
            {
                correct <<- correct + 1
            }
            else
            {
                incorrect <<- incorrect + 1
            }
            
            # display score
            c("Correct: ", correct, " Incorrect: ", incorrect)
        }
        else
        {
            c("Correct: ", correct, " Incorrect: ", incorrect)
            
        }
    })
    
    output$quizImage <- renderText({
        
        
        
        # change bird
        speciesIndex <- round(runif(1, 1, length(speciesCodeList)))
        choices <- c(speciesCodeList[speciesIndex], speciesCodeList[round(runif(4, 1, length(speciesCodeList)))])
        choices <- sample(choices)
        values <- c()
        for (i in 1:5)
        {
            if (choices[i] == speciesCodeList[speciesIndex])
            {
                values <- append(values, TRUE)
            }
            else
            {
                values <- append(values, FALSE)
            }
        }
        updateRadioButtons(inputId = "guess", choiceNames = choices, choiceValues = values)
        photo <- getPhotoSearch(tags = c(speciesCodeList[speciesIndex]), per_page = 3, sort = "relevance")
        photoIndex <- round(runif(min = 1, max = 3, n = 1))
        serverID <- photo$server[photoIndex]
        ID <- photo$id[photoIndex]
        secret <- photo$secret[photoIndex]
        size = "w"
        format = "jpg"
        
        src= paste(
            sep = "",
            "https://live.staticflickr.com/",
            serverID,
            "/",
            ID,
            "_",
            secret,
            "_",
            size,
            ".",
            format
                   )
        cite <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "url")$content
        print(cite)
        author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all")$owner$realname
        print(author)
        c('<img src="', src, '">', '<br/>
          <p>Photo property of 
          <a href="', cite, '" target="_blank">' ,author, '</a>
          and API services via
          <a href="flickr.com" target="_blank">Flickr</a>
          </p>')
    })
})

observeEvent(input$resetQuiz,
{
    correct <<- 0
    incorrect <<- 0
    output$quizScore <- renderText({
        c("Correct: ", correct, " Incorrect: ", incorrect)
        
    })
    output$quizImage <- renderText({
        
        
        
        # change bird
        speciesIndex <- round(runif(1, 1, length(speciesCodeList)))
        choices <- c(speciesCodeList[speciesIndex], speciesCodeList[round(runif(4, 1, length(speciesCodeList)))])
        choices <- sample(choices)
        values <- c()
        for (i in 1:5)
        {
            if (choices[i] == speciesCodeList[speciesIndex])
            {
                values <- append(values, TRUE)
            }
            else
            {
                values <- append(values, FALSE)
            }
        }
        updateRadioButtons(inputId = "guess", choiceNames = choices, choiceValues = values)
        photo <- getPhotoSearch(tags = c(speciesCodeList[speciesIndex]), per_page = 3, sort = "relevance")
        photoIndex <- round(runif(min = 1, max = 3, n = 1))
        serverID <- photo$server[photoIndex]
        ID <- photo$id[photoIndex]
        secret <- photo$secret[photoIndex]
        size = "w"
        format = "jpg"
        
        src= paste(
            sep = "",
            "https://live.staticflickr.com/",
            serverID,
            "/",
            ID,
            "_",
            secret,
            "_",
            size,
            ".",
            format
        )
        cite <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "url")$content
        print(cite)
        author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all")$owner$realname
        print(author)
        c('<img src="', src, '">', '<br/>
          <p>Photo property of 
          <a href="', cite, '" target="_blank">' ,author, '</a>
          and API services via
          <a href="flickr.com" target="_blank">Flickr</a>
          </p>')
    })
})