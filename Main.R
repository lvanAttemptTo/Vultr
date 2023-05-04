library("httr")
library("stringr")


nearbyObs <- function(key = NA, lat = NA, lng = NA, dist = 25, back = 14, cat = "all", hotspot = FALSE, includeProvisional = FALSE, sort = "date", sppLocale = "en", maxResults = "all")
{
    if (!is.na(key))
    {
        headers = c(
            
            'X-eBirdApiToken' = key
        )
        
        res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/geo/recent?lat=", lat, "&lng=", lng, "&sort=", sort), add_headers(headers))
        
        catRes <- content(res, 'text')
        parsedSightings <- unlist(str_split(catRes[[1]], "[}]"))
        parsedSightings <- parsedSightings[!duplicated(parsedSightings)]
        print(parsedSightings[[1]])
        parsedSightingInformation <-data.frame(speciesCode = NA, comName = NA, sciName = NA, locId = NA, locName = NA, obsDt = NA, howMany = NA, lat = NA, lng = NA, obsValid = NA, obsReviewed = NA, locationPrivate = NA, subId = NA)
        
        for (i in 1:length(parsedSightings))
        {
            testParse1 <- unlist(str_split(parsedSightings[i], "[{]"))[2]
            print(testParse1)
            testParse2 <- unlist(str_split(testParse1, ',\"'))
            print(testParse2)
            testParse3 <- c()
            for (j in testParse2)
            {
                p <- unlist(str_split(j, "[:]"))[2]
                
                p <- gsub('"', "", p)
                
                if (p == "true")
                {
                    p <- TRUE
                }
                else if (p == "false")
                {
                    p <- FALSE
                }
                try(
                    
                    if (!is.na(as.numeric(p)))
                    {
                        p <- as.numeric(p)
                    }
                    ,
                    silent = TRUE
                )
                
                testParse3 <- append(testParse3, p)
            }
            print(testParse3)
            parsedSightingInformation[i,] <- testParse3
        }
        # df <- data.frame(speciesCode = NA, comName = NA, sciName = NA, locId = NA, locName = NA, obsDt = NA, howMany = NA, lat = NA, lng = NA, obsValid = NA, obsReviewed = NA, locationPrivate = NA, subId = NA)
        # for (i in 1:length(catRes))
        # {
        #     
        #     df[i,] <- catRes[[i]]
        # }
        # return(df)
    }
    else if (is.na(key))
    {
        stop("No key set")
    }
    else if (is.na(lat))
    {
        stop("No latitude set")
    }
    else if (is.na(lng))
    {
        stop("No longitude set")
    }
}

nearbyObs(key = "vmgu1o6c6ihc", lat = 45.5896568645855, lng = -122.738592624664)
