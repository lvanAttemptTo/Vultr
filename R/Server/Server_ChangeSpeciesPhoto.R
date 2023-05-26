observeEvent(c(input$previousphotosearch), {
    
    speciesInput <- searchedSpecies
    outHTML <- NA
    
    if (speciesInput != "")
    {
        photo <- speciesSearchPhoto
        if (nrow(photo) > 0)
        {
            if (speciesPhotoIndex > 1)
            {
                speciesPhotoIndex <<- speciesPhotoIndex - 1
            
                photoIndex <- speciesPhotoIndex
                
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
                cite <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "url", api_key = flickerAPIkey)$content
                print(cite)
                author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$realname
                print(author)
                if (author == "")
                {
                    author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$username
                }
                outHTML <- c('<img src="', src, '", width="100%">', '<br/>
                      <p>Photo property of 
                      <a href="', cite, '" target="_blank">' ,author, '</a>
                      and API services via
                      <a href="https://www.flickr.com" target="_blank">Flickr</a>
                      </p>')
            }
            else
            {
                shinyalert("First Image.")
                outHTML <- NA
            }
        }
    }
    if (!is.na(outHTML[[1]]))
    {
        output$SpeciesPhoto <- renderText({
            outHTML
        })
    }
    
})

observeEvent(c(input$nextphotosearch), {
    
    speciesInput <- searchedSpecies
    outHTML <- NA
    print("1")
    if (speciesInput != "")
    {
        print("2")
        
        photo <- speciesSearchPhoto
        if (nrow(photo) > 0)
        {
            print("3")
            
            if (speciesPhotoIndex < nrow(photo))
            {
                print("4")
                
                speciesPhotoIndex <<- speciesPhotoIndex + 1
            
                photoIndex <- speciesPhotoIndex
                
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
                cite <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "url", api_key = flickerAPIkey)$content
                print(cite)
                author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$realname
                print(author)
                if (author == "")
                {
                    author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$username
                }
                outHTML <- c('<img src="', src, '", width="100%">', '<br/>
                      <p>Photo property of 
                      <a href="', cite, '" target="_blank">' ,author, '</a>
                      and API services via
                      <a href="https://www.flickr.com" target="_blank">Flickr</a>
                      </p>')
            }
            else
            {
                shinyalert("No More Images.")
                outHTML <- NA
            }
        }
        
    }
    print("5")
    
    if (!is.na(outHTML[[1]]))
    {
        output$SpeciesPhoto <- renderText({
            outHTML
        })
    }
})