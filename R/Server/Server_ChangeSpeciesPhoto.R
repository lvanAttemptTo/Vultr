# if the user presses go to the previous image
observeEvent(c(input$previousphotosearch), {
    # the global var for the currently searched species
    speciesInput <- searchedSpecies
    outHTML <- NA # the output HTML for the image and cite
    
    if (speciesInput != "") # if there is a species searched
    {
        # polls the flikr API for the top 50 most relevant images of the searched bird
        photo <- getPhotoSearch(tags = c(speciesInput), per_page = 50, sort = "relevance", api_key = flickerAPIkey, img_size = "w")
        if (nrow(photo) > 0) # if there is any photos of the bird
        {
            if (speciesPhotoIndex > 1) # if the selected photo index is greater then 1
            {
                speciesPhotoIndex <<- speciesPhotoIndex - 1
            
                photoIndex <- speciesPhotoIndex
                
                # info for getting the photo's URL
                serverID <- photo$server[photoIndex]
                ID <- photo$id[photoIndex]
                secret <- photo$secret[photoIndex]
                size = "w"
                format = "jpg"
                
                # the url source for the image
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
                # the url for the image
                cite <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "url", api_key = flickerAPIkey)$content
                print(cite)
                # the author of the image
                author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$realname
                print(author)# this catches the case that the author does not have a real name set and sets it to their username
                if (author == "")
                {
                    author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$username
                }
                # outHTML is set to the html for the image and site
                outHTML <- c('<img src="', src, '", width="100%">', '<br/>
                      <p>Photo property of 
                      <a href="', cite, '" target="_blank">' ,author, '</a>
                      and API services via
                      <a href="https://www.flickr.com" target="_blank">Flickr</a>
                      </p>')
            }
            else # if the species index is 1
            {
                shinyalert("First Image.") # send alert to the user saying this is the first image
                outHTML <- NA
            }
        }
    }
    # if there is a new image
    if (!is.na(outHTML[[1]]))
    {
        # outputs the new image
        output$SpeciesPhoto <- renderText({
            outHTML
        })
    }
    
})

# if the user presses go to next image
observeEvent(c(input$nextphotosearch), {
    
    speciesInput <- searchedSpecies # global searched species var
    outHTML <- NA # output for image HTML
    
    if (speciesInput != "") # if there is a species searched
    {
        # polls the flikr API for the 50 most relivant images to the searched bird
        photo <- getPhotoSearch(tags = c(speciesInput), per_page = 50, sort = "relevance", api_key = flickerAPIkey, img_size = "w")
        if (nrow(photo) > 0) # if there is any photos
        {
            # if the photo index is less than the amount of photos
            if (speciesPhotoIndex < nrow(photo))
            {
                speciesPhotoIndex <<- speciesPhotoIndex + 1
            
                photoIndex <- speciesPhotoIndex
                
                # info for the photos URL
                serverID <- photo$server[photoIndex]
                ID <- photo$id[photoIndex]
                secret <- photo$secret[photoIndex]
                size = "w"
                format = "jpg"
                
                # the url source for the image
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
                # the url for the image
                cite <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "url", api_key = flickerAPIkey)$content
                print(cite)
                # the authors name
                author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$realname
                print(author)
                # catches the case where the author does not have their real name set so it uses their username
                if (author == "")
                {
                    author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$username
                }
                # sets outHTML to the image
                outHTML <- c('<img src="', src, '", width="100%">', '<br/>
                      <p>Photo property of 
                      <a href="', cite, '" target="_blank">' ,author, '</a>
                      and API services via
                      <a href="https://www.flickr.com" target="_blank">Flickr</a>
                      </p>')
            }
            else # if the index is the same as the amount of images
            {
                shinyalert("No More Images.") # sends alert to user
                outHTML <- NA
            }
        }
        
    }
    
    # if the image changed
    if (!is.na(outHTML[[1]]))
    {
        # sets the new image
        output$SpeciesPhoto <- renderText({
            outHTML
        })
    }
})
