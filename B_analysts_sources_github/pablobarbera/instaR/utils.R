#' @import jsonlite httr
#' @importFrom utils packageVersion

.onAttach <- function(...) {
 
   packageStartupMessage("##\n## instaR: Access to Instagram API via R")
   packageStartupMessage("## Note: due to recent changes in the Instagram")
   packageStartupMessage("## API, access to most endpoints now require ")
   packageStartupMessage("## previous approval from Instagram. See:")
   packageStartupMessage("## https://www.instagram.com/developer/review/\n##")

}

unlistWithNA <- function(lst, field){
    if (length(field)==1){
        notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field]])))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst, '[[', field))
    }
    if (length(field)==2){
        notnulls <- unlist(lapply(lst, function(x) tryCatch(!is.null(x[[field[1]]][[field[2]]]),
            error=function(e) FALSE)))
        vect <- rep(NA, length(lst))
        values <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]]))
        if (length(values)>0) { vect[notnulls] <- values }
    }
    if (length(field)==3){
        notnulls <- unlist(lapply(lst, function(x) 
            tryCatch(!is.null(x[[field[1]]][[field[2]]][[field[3]]]), 
                error=function(e) FALSE)))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]]))
    }
    return(vect)
}

searchListToDF <- function(data){
    df <- data.frame(
        type = data$type,
        longitude = data$location.longitude,
        latitude = data$location.latitude,
        location_name = data$location.name,
        location_id = data$location.id,
        comments_count = data$comments.count,
        filter = data$filter,
        created_time = as.POSIXct(as.numeric(data$created_time), origin="1970-01-01"),
        link = data$link,
        likes_count = data$likes.count,
        image_url = data$images.standard_resolution.url,
        caption = data$caption.text,
        username = data$user.username,
        user_id = data$user.id,
        user_fullname = data$user.full_name,
        id = data$id,
        stringsAsFactors=F)
    return(df)
}

likesListToDF <- function(data){
    df <- data.frame(
        username =  unlistWithNA(data, 'username'),
        full_name =  unlistWithNA(data, 'full_name'),
        id =  unlistWithNA(data, 'id'),
        profile_picture =  unlistWithNA(data, 'profile_picture'),
        stringsAsFactors=F)
    return(df)
}
  
commentsListToDF <- function(data){
    df <- data.frame(
        created_time = as.POSIXct(as.numeric(unlistWithNA(data, 'created_time')), origin="1970-01-01"),
        text = unlistWithNA(data, 'text'),
        from_username = unlistWithNA(data, c('from', 'username')),
        from_id = unlistWithNA(data, c('from', 'id')),
        from_profile_picture = unlistWithNA(data, c('from', 'profile_picture')),
        from_full_name = unlistWithNA(data, c('from', 'full_name')),
        id = unlistWithNA(data, "id"),
        stringsAsFactors=F)
    return(df)
}

userListToDF <- function(data){
    df <- data.frame(
        username =  unlistWithNA(data, 'username'),
        bio =  unlistWithNA(data, 'bio'),
        website =  unlistWithNA(data, 'website'),
        profile_picture =  unlistWithNA(data, 'profile_picture'),
        full_name =  unlistWithNA(data, 'full_name'),
        id =  unlistWithNA(data, 'id'),
        stringsAsFactors=F)
    return(df)
}

userDataToDF <- function(data){
    df <- data.frame(
        username = data$username,
        bio = data$bio,
        website = data$website,
        profile_picture = data$profile_picture,
        full_name = data$full_name,
        media_count = data$counts$media,
        followed_by = data$counts$followed_by,
        follows = data$counts$follows,
        stringsAsFactors=F)
    return(df)
}

popularDataToDF <- function(data){
  df <- data.frame(
        type = data$type,
        longitude = data$location.longitude,
        latitude = data$location.latitude,
        location_name = data$location.name,
        location_id = data$location.id,
        comments_count = data$comments.count,
        filter = data$filter,
        created_time = as.POSIXct(as.numeric(data$created_time), origin="1970-01-01"),
        link = data$link,
        likes_count = data$likes.count,
        image_url = data$images.standard_resolution.url,
        caption = data$caption.text,
        username = data$user.username,
        user_id = data$user.id,
        user_fullname = data$user.full_name,
        id = data$id,
    stringsAsFactors=F)
  return(df)
}


callAPI <- function(url, token){
    if (class(token)[1]=="config"){
        url.data <- httr::GET(url, config=token)
    }
    if (class(token)[1]=="Token2.0"){
        url.data <- httr::GET(paste0(url, ifelse(grepl('\\?', url), "&", "?"),
                "access_token=", token$credentials$access_token))
    }  
    if (class(token)[1]!="config" & class(token)[1]!="Token2.0"){
        stop("Error in access token. See help for details.")
    }
    error <- tryCatch(content <- jsonlite::fromJSON(rawToChar(url.data$content), 
        unexpected.escape = "skip"), error=function(e) e)
    # retrying 3 times if error
    if (inherits(error, 'error')){
        err <- 0
        while (inherits(error, 'error')){
            Sys.sleep(.5)
            error <- tryCatch(content <- jsonlite::fromJSON(rawToChar(url.data$content), 
                flatten=TRUE), error=function(e) e)
            err <- err + 1
            if (err==3){ stop("Error!") }
        }
    }
    return(content)
}

downloadPictures <- function(df, folder){
    # creating folder if it doesn't exist
    dir.create(file.path(getwd(), folder), showWarnings = FALSE)
    for (i in 1:nrow(df)){
        filename <- paste0(getwd(), "/", folder, "/", 
                             df$id[i], "_", df$username[i], ".jpg")
        try(r <- httr::GET(df$image_url[i], httr::write_disk(filename, overwrite=TRUE)))
    }
}
