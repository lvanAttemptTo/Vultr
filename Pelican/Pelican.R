library("httr")
library("stringr")


nearbyObs <- function(key = NA, lat = NA, lng = NA, dist = 25, back = 14, cat = "all", hotspot = FALSE, includeProvisional = FALSE, sort = "date", sppLocale = "en", maxResults = "all")
{
    if (!is.na(key) & !is.na(lat) & !is.na(lng))
    {
        headers = c(
            
            'X-eBirdApiToken' = key
        )
        if (cat == "all" & maxResults == "all")
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/geo/recent?lat=", lat, "&lng=", lng, "&sort=", sort, "&dist=", dist, "&back=", back, "&hotspot=", hotspot, "&includeProvisional=", includeProvisional, "&sppLocal=", sppLocale), add_headers(headers))
        }
        else if (cat != "all" & maxResults != "all")
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/geo/recent?lat=", lat, "&lng=", lng, "&sort=", sort, "&dist=", dist, "&back=", back, "&hotspot=", hotspot, "&includeProvisional=", includeProvisional, "&sppLocal=", sppLocale, "&cat=", cat, "&maxResults=", maxResults), add_headers(headers))
        }
        else if (cat != "all")
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/geo/recent?lat=", lat, "&lng=", lng, "&sort=", sort, "&dist=", dist, "&back=", back, "&hotspot=", hotspot, "&includeProvisional=", includeProvisional, "&sppLocal=", sppLocale, "&cat=", cat), add_headers(headers))
        }
        else
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/geo/recent?lat=", lat, "&lng=", lng, "&sort=", sort, "&dist=", dist, "&back=", back, "&hotspot=", hotspot, "&includeProvisional=", includeProvisional, "&sppLocal=", sppLocale, "&maxResults=", maxResults), add_headers(headers))
        }
        catRes <- content(res, 'text')
        parsedSightings <- unlist(str_split(catRes[[1]], "[}]"))
        parsedSightings <- parsedSightings[!duplicated(parsedSightings)]
        parsedSightingInformation <- data.frame(speciesCode = NA, comName = NA, sciName = NA, locId = NA, locName = NA, obsDt = NA, howMany = NA, lat = NA, lng = NA, obsValid = NA, obsReviewed = NA, locationPrivate = NA, subId = NA)
        if (length(parsedSightings) > 1)
        {
            for (i in 1:(length(parsedSightings)-1))
            {
                testParse1 <- unlist(str_split(parsedSightings[i], "[{]"))[2]
                testParse2 <- unlist(str_split(testParse1, ',\"'))
                testParse3 <- list()
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
                    suppressWarnings(
                        expr = {
                            if (!is.na(as.numeric(p)))
                            {
                                p <- as.numeric(p)
                            }
                        }
                        
                        
                    )
    
                    
                    testParse3 <- append(testParse3, p)
                }
                if (length(testParse3) == 12)
                {
                    temp <- testParse3[1:6]
                    temp <- append(temp, NaN)
                    temp <- append(temp, testParse3[7:12])
                    testParse3 <- temp
                }
                else if(length(testParse3) == 14)
                {
                    testParse3 <- testParse3[1:13]
                }
                parsedSightingInformation[i,] <- testParse3
            }
        }
        # df <- data.frame(speciesCode = NA, comName = NA, sciName = NA, locId = NA, locName = NA, obsDt = NA, howMany = NA, lat = NA, lng = NA, obsValid = NA, obsReviewed = NA, locationPrivate = NA, subId = NA)
        # for (i in 1:length(catRes))
        # {
        #     
        #     df[i,] <- catRes[[i]]
        # }
        # return(df)
        return(parsedSightingInformation)
    }
    else if (is.na(key))
    {
        warning("No key set")
    }
    else if (is.na(lat))
    {
        warning("No latitude set")
    }
    else if (is.na(lng))
    {
        warning("No longitude set")
    }
}


regionObs <- function(key = NA, regionCode = NA, back = 14, cat = "all", hotspot = FALSE, includeProvisional = FALSE, sppLocale = "en", maxResults = "all")
{
    if (!is.na(key) & !is.na(regionCode))
    {
        headers = c(
            
            'X-eBirdApiToken' = key
        )
        
        if (cat == "all" & maxResults == "all")
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/", regionCode, "/recent", "?back=", back, "&hotspot=", hotspot, "&includeProvisional=", includeProvisional, "&sppLocal=", sppLocale), add_headers(headers))
        }
        else if (cat != "all" & maxResults != "all")
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/", regionCode, "/recent", "?back=", back, "&hotspot=", hotspot, "&includeProvisional=", includeProvisional, "&sppLocal=", sppLocale, "&cat=", cat, "&maxResults=", maxResults), add_headers(headers))
        }
        else if (cat != "all")
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/", regionCode, "/recent", "?back=", back, "&hotspot=", hotspot, "&includeProvisional=", includeProvisional, "&sppLocal=", sppLocale, "&cat=", cat), add_headers(headers))
        }
        else
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/", regionCode, "/recent", "?back=", back, "&hotspot=", hotspot, "&includeProvisional=", includeProvisional, "&sppLocal=", sppLocale, "&maxResults=", maxResults), add_headers(headers))
        }
        
        catRes <- content(res, 'text')
        parsedSightings <- unlist(str_split(catRes[[1]], "[}]"))
        parsedSightings <- parsedSightings[!duplicated(parsedSightings)]
        parsedSightingInformation <-data.frame(speciesCode = NA, comName = NA, sciName = NA, locId = NA, locName = NA, obsDt = NA, howMany = NA, lat = NA, lng = NA, obsValid = NA, obsReviewed = NA, locationPrivate = NA, subId = NA)
        if (length(parsedSightings) > 1)
        {
            for (i in 1:(length(parsedSightings)-1))
            {
                testParse1 <- unlist(str_split(parsedSightings[i], "[{]"))[2]
                testParse2 <- unlist(str_split(testParse1, ',\"'))
                testParse3 <- list()
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
                    suppressWarnings(
                        expr = {
                            if (!is.na(as.numeric(p)))
                            {
                                p <- as.numeric(p)
                            }
                        }
                        
                        
                    )
                    
                    testParse3 <- append(testParse3, p)
                }
                if (length(testParse3) == 12)
                {
                    temp <- testParse3[1:6]
                    temp <- append(temp, NaN)
                    temp <- append(temp, testParse3[7:12])
                    testParse3 <- temp
                }
                else if(length(testParse3) == 14)
                {
                	testParse3 <- testParse3[1:13]
                }
                parsedSightingInformation[i,] <- testParse3
            }
        }
        # df <- data.frame(speciesCode = NA, comName = NA, sciName = NA, locId = NA, locName = NA, obsDt = NA, howMany = NA, lat = NA, lng = NA, obsValid = NA, obsReviewed = NA, locationPrivate = NA, subId = NA)
        # for (i in 1:length(catRes))
        # {
        #     
        #     df[i,] <- catRes[[i]]
        # }
        # return(df)
        return(parsedSightingInformation)
    }
    else if (is.na(key))
    {
        warning("No key set")
    }
    else if (is.na(lat))
    {
        warning("No latitude set")
    }
    else if (is.na(lng))
    {
        warning("No longitude set")
    }
}

nearbyHotspots <- function(key = NA, lat = NA, lng = NA, dist = 25, back = NA)
{
    if (!is.na(key) & !is.na(lat) & !is.na(lng))
    {
        headers = c(
            
            'X-eBirdApiToken' = key
        )
        if (is.na(back))
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/ref/hotspot/geo?lat=", lat, "&lng=", lng, "&dist=", dist, "&fmt=json"), add_headers(headers))
        }
        else
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/ref/hotspot/geo?lat=", lat, "&lng=", lng, "&dist=", dist, "&back=", back, "&fmt=json"), add_headers(headers))
        }
        catRes <- content(res, "text")
        parsedSightings <- unlist(str_split(catRes[[1]], "[}]"))
        parsedSightings <- parsedSightings[!duplicated(parsedSightings)]
        parsedSightingInformation <-data.frame(locId = NA, country = NA, subregion1 = NA, subregion2 = NA, lat = NA, lng = NA, locName = NA, lastSighting = NA, speciesCount = NA)
        if (length(parsedSightings) > 1)
        {
            for (i in 1:(length(parsedSightings)-1))
            {
                testParse1 <- unlist(str_split(parsedSightings[i], "[{]"))[2]
                testParse2 <- unlist(str_split(testParse1, ',\"'))
                testParse3 <- list()
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
                    suppressWarnings(
                        expr = {
                            if (!is.na(as.numeric(p)))
                            {
                                p <- as.numeric(p)
                            }
                        }
                        
                        
                    )
                    
                    testParse3 <- append(testParse3, p)
                }
                
                parsedSightingInformation[i,] <- testParse3
            }
        }
        parsedSightingInformation[i,] <- testParse3
    }
    return(parsedSightingInformation)
}

nearbyHotspots(key = "vmgu1o6c6ihc", lat = 45.5896568645855, lng = -122.738592624664)