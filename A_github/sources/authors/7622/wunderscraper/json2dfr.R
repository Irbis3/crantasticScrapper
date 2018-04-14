json2dfr <- function(json) {
    pws <- with(pws $current_observation,
                c(with(display_location, c(lon=longitude, lat=latitude)),
                  y=temp_f))
    ## if y=='relative_humidity' substr(y, 1, nchar(y)-1)
    pws <- as.numeric(pws)
    if(length(pws)==3) return(pws)
    else return(c(lon=NA, lat=NA, y=NA))
}
