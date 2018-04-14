library(zipcode)
data(zipcode)
# part code from http://jeffreybreen.wordpress.com/2011/01/05/cran-zipcode/
 
library(Imap)
#gdist(ny[2],ny[1],pdx[2],pdx[1],units="miles")
 
# calculate.distance(10002,97232) # in miles
ZipToZip <- function(zip1,zip2) {
        #gdist(lon.1, lat.1, lon.2, lat.2, units = "nm", a = 6378137.0, b = 6356752.3142, verbose = FALSE)
        if (suppressWarnings(!is.na(as.numeric(zip1)))==T & suppressWarnings(!is.na(as.numeric(zip2)))==T) {
                somedata = data.frame(postal = c(zip1,zip2))
                somedata$zip = clean.zipcodes(somedata$postal)
                somedata = merge(somedata, zipcode, by.x='zip', by.y='zip')
                return(gdist(somedata$longitude[1], somedata$latitude[1], somedata$longitude[2], somedata$latitude[2], units='miles'))
        } else {
                return (NA)
        }
}
