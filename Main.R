library("httr")


nearbyObs <- function(key = NA, lat = NA, lng = NA, dist = 25, back = 14, cat = "all", hotspot = FALSE, includeProvisional = FALSE, sort = "date", sppLocale = "en", maxResults = "all")
{
    if (!is.na(key))
    {
        headers = c(
            
            'X-eBirdApiToken' = key
        )
        
        res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/geo/recent?lat=", lat, "&lng=", lng, "&sort=", sort), add_headers(headers))
        
        catRes <- content(res, 'parsed')
        df <- data.frame(speciesCode = NA, comName = NA, sciName = NA, locId = NA, obsDt = NA, howMany = NA, lat = NA, lng = NA, obsValid = NA, obsReviewed = NA, locationPrivate = NA, subId = NA)
        print(df)
        for (i in 1:length(catRes))
        {
            speciesCode1 <- catRes[[i]]$speciesCode
            comName1 <- catRes[[i]]$comName
            sciName1 <- catRes[[i]]$sciName
            locId1 <- catRes[[i]]$locId
            obsDt1 <- catRes[[i]]$obsDt
            howMany1 <- catRes[[i]]$howMany
            lat1 <- catRes[[i]]$lat
            lng1 <- catRes[[i]]$lng
            obsValid1 <- catRes[[i]]$obsValid
            obsReviewed1 <- catRes[[i]]$obsReviewed
            locationPrivate1 <- catRes[[i]]$locationPrivate
            subId1 <- catRes[[i]]$subId
            print(c(speciesCode, comName, sciName, locId, obsDt, howMany, lat, lng, obsValid, obsReviewed, locationPrivate, subId))
            df2 <- data.frame(speciesCode = speciesCode1, comName = comName1, sciName = sciName1, locId = NA, obsDt = NA, howMany = NA, lat = NA, lng = NA, obsValid = NA, obsReviewed = NA, locationPrivate = NA, subId = NA)
            df<- rbind(df, )
        }
        return(df)
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

print(nearbyObs(key = "vmgu1o6c6ihc", lat = 45.5896568645855, lng = -122.738592624664))
warnings()
