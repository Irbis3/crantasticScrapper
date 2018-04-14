
######################## functions ################################
MapIt <- function(latitude, longitude, size, objggmap) {
        objdf <- data.frame('latitude'=latitude,
                            'longitude'=longitude,
                            'size'=size)
        
        # add new point to ggmap
        require("ggplot2")
        require("ggmap")
        objggmap <- objggmap + geom_point(
                aes(x=longitude, y=latitude, size=size, 
                    show_guide = TRUE, colour=size), 
                data=objdf, alpha=.8, na.rm = T)  
        return (objggmap)
}

# yelp rest API call
GetBestYelpLocation <- function(boundedcoordinates, term) {
        limit <- 1
        print(boundedcoordinates)
        # Pick first florist in San Francisco, CA
        #YelpUrl <- paste0("http://api.yelp.com/v2/search/?limit=",limit,
        #                  "&location=San%20Francisco%20CA&term=florist")
        # or 10 bars by geo-coordinates
        #YelpUrl <- paste0("http://api.yelp.com/v2/search/?limit=",
        #       limit,"&ll=",latitude,",",longitude,"&term=florist")
        # or by bounded geo-spatial coordinates
        YelpUrl <- paste0("http://api.yelp.com/v2/search/?limit=20&bounds=",
                          boundedcoordinates[1],",",boundedcoordinates[2],
                          "|",boundedcoordinates[3],",",boundedcoordinates[4],"&term=",term)
        
        locationdata=GET(YelpUrl, sig)
        listMembersContent = content(locationdata)
        listMembers=jsonlite::fromJSON(toJSON(listMembersContent))
        
        yelpResults = tryCatch({
                data.frame(listMembers)
        }, error = function(e) { 
                NULL
        })
        if (!is.null(yelpResults)) {
                set1 <- data.frame("name"=yelpResults$businesses.name,'city'=yelpResults$businesses.location$city,
                                   'rating'=yelpResults$businesses.rating, 
                                   'latitude'=yelpResults$businesses.location$coordinate$latitude,
                                   'longitude'=yelpResults$businesses.location$coordinate$longitude,
                                   'state'=yelpResults$businesses.location$state_code)
                
                for (ind in seq(1:nrow(set1))) {
                        if ((set1$latitude[ind] <= boundedcoordinates[1]) &
                                    (set1$latitude[ind] >= boundedcoordinates[3]) &
                                    (set1$longitude[ind] >= boundedcoordinates[2]) &
                                    (set1$longitude[ind] <= boundedcoordinates[4]))
                                return(set1[ind,])
                }
        }
        return(NULL)
}

# Movement controls
GetPossibleCoordinates<-function(lat, lon, area=1) {
        # this is hard to keep track but each location's next move
        # equates attempting to move forward one square, the same area
        # as the previous one but half-way above and half-way below the
        # previous one.
        halfarea <- area/2
        
        # new forward top square area
        topArea_upperleft_latitude <- lat+area
        topArea_upperleft_longitude <- lon+halfarea
        
        topArea_lowerright_latitude <- lat
        topArea_lowerright_longitude <- lon+(halfarea+area)
        
        # new forward bottom square area
        bottomArea_upperleft_latitude <- lat
        bottomArea_upperleft_longitude <- lon+halfarea
        
        bottomArea_lowerright_latitude <- lat-area
        bottomArea_lowerright_longitude <- lon+(halfarea+area)
        
        
        rownames <- c('new_top_area','new_bottom_area')
        latitude_point1 <- c(topArea_upperleft_latitude, bottomArea_upperleft_latitude)
        longitude_point1 <- c(topArea_upperleft_longitude, bottomArea_upperleft_longitude)
        latitude_point2 <- c(topArea_lowerright_latitude, bottomArea_lowerright_latitude)
        longitude_point2 <- c(topArea_lowerright_longitude, bottomArea_lowerright_longitude)
        
        return (data.frame('direction'=rownames,
                           'latitude_point1'=latitude_point1,
                           'longitude_point1'=longitude_point1,
                           'latitude_point2'=latitude_point2,
                           'longitude_point2'=longitude_point2))
        
}

MakeAMove <- function(lat,lon,sizebox, searchTerm, lat_endPoint) {
        possibleCoordinates <- GetPossibleCoordinates(lat,lon,sizebox)
        
        # go up or down first depending on latitude of end point
        searchOrder <- c('new_top_area','new_bottom_area')
        if (lat > lat_endPoint)
                searchOrder <- c('new_bottom_area','new_top_area')
        
        for (directiontogo in searchOrder) {
                coords <- possibleCoordinates[possibleCoordinates$direction==directiontogo,]
                print(paste('Scanning',directiontogo, 'for', searchTerm,'...'))
                foundLocation <- GetBestYelpLocation(as.numeric(as.vector(coords[2:5])), searchTerm)
                if (!is.null(foundLocation))
                        return (foundLocation)
        }
        return (NULL)
}

############################## YELP SETUP ###############################
# yelp credentials
consumerKey = "xxxxx"
consumerSecret = "xxxxx"
token = "xxxxx"
token_secret = "xxxxx"

require(httr)
require(jsonlite)

# authorization
myapp = oauth_app("Yelp", key=consumerKey, secret=consumerSecret)
sig=sign_oauth1.0(myapp, token=token,token_secret=token_secret)

############################### Manual Code ###############################
require(ggplot2)
require(ggmap)

# Get our starting and ending points
# Fisherman's Wharf, San Francisco, CA 94109
startingpoint <- geocode(c("Fishersman's Wharf, San Francisco, CA"))
# Chelsea Piers 10011
endingpoint <- geocode(c("Chelsea Piers, NY"))

# ggplots accepts a data frame, let's make with with start and end points
latitudes <- c(startingpoint$lat, endingpoint$lat)
longitudes <- c(startingpoint$lon, endingpoint$lon)
# importance holds the size and color (we'll leave it up to ggplot to choose)
objdf <- data.frame('latitude'=latitudes,
                    'longitude'=longitudes,
                    'importance'=c(10,10))

# sample ggplot/ggmap with start and end locations
# get a Google map
require(ggmap)
map<-get_map(location='united states', zoom=4, maptype = "terrain",
             source='google',color='color')

# plot it with ggplot2
require("ggplot2")
objMap <- NULL
objMap <- ggmap(map) + geom_point(
        aes(x=longitude, y=latitude, size=importance, 
            show_guide = TRUE, colour = importance), 
        data=objdf, alpha=.8, na.rm = T)  


################################### Logic #####################################

searchTerm <- 'florist'
squareSize <- 1 # setting the bounded area to a square 

# start trip info vectors - we need to remember where we've been!
currentLatitude <- startingpoint$lat
currentLongitude <- startingpoint$lon

# let ggmap keep track of where we've been
objMap <- MapIt(currentLatitude, currentLongitude, 1, objMap)

madeIt=FALSE
safetyCount <- 0
foundCount <- 0
while(madeIt == FALSE) {
        safetyCount <- safetyCount + 1
        foundLocation <- MakeAMove(lat=currentLatitude,
                                   lon=currentLongitude,
                                   sizebox=squareSize,
                                   searchTerm=searchTerm,
                                   endingpoint$lat)
        
        if (!is.null(foundLocation)) {
                print (paste('Our new',searchTerm, 'is', foundLocation$name, 'in', 
                             foundLocation$city, foundLocation$state,
                             'with a', foundLocation$rating, 'start rating'))
                currentLatitude <- foundLocation$latitude
                currentLongitude <- foundLocation$longitude
                
                # reset temporary setting
                squareSize <- 1
                
                # let ggmap keep track of where we've been
                objMap <- MapIt(currentLatitude, currentLongitude, squareSize, objMap)
                
                # let's keep track how our successes!
                foundCount <- foundCount + 1
        } else {
                # increase squareSize
                print(paste("Can't find any", searchTerm,", enlarging square search area to",squareSize + 1))
                
                # temporary settings to get us out of desert
                squareSize <- squareSize + 1
        }  
        
        # have we arrived at our end point
        if ((currentLongitude < (endingpoint$lon + squareSize)) 
            & (currentLongitude > (endingpoint$lon - squareSize)))
        {
                print(paste('We made it!! It took',foundCount,'hops...'))
                break
        }
        if (safetyCount > 100) 
        {
                print(paste('Giving up!! Failed after',foundCount,'hops'))
                break
        }
        
        # be considerate with your Yelp requests
        Sys.sleep(0.5)
}

objMap