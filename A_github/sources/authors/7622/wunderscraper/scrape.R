#' Scrape Wunderground API
#'
#' Randomly samples from Wunderground API with a sampling strategy based on
#' states, counties, zip codes, or a grid.
#'
#' The sampling strategy has two constraints: 1) the next to last stage of the
#' strategy must be: zip code, city name, or latitude/longitude, and 2) the last
#' stage must sample individual weather stations.  Wunderscraper sends the values
#' of the next to last stage identifier as queries to the Wunderground API.  In
#' addition to stages users may specify weights or strata, and may also generate
#' spatial grids to use as stages or strata.
#'
#' Users specify a sampling strategy through a set of vector-valued arguments
#' that indicate the sampling stages, sizes, strata, and weights.  All sampling
#' parameter vectors are in stage order, from the first to the last, and must be
#' fully nested.
#' 
#' Wunderscraper is limited to the following stage and strata identifiers:
#' states, counties, and arbitrary spatial grids; indicated in sampling
#' parameter vectors as \code{"STATEFP"}, \code{"GEOID"}, and \code{"GRID"}
#' respectively.
#'
#' Wunderscraper may use population or land area as a weighting variable.
#' County population and state population are \code{"COPOP"} and \code{"STPOP"}
#' respectively.  Similarly, county and state area are \code{"COAREA"} and
#' \code{"STAREA"}, respectively, where \code{"COLAND"} and \code{"STLAND"} are
#' land areas without water.  See \code{\link{zctaRel}} for more details on
#' available weighting variables.
#'
#' The sampling parameter vectors will be padded on the right with NA values to
#' the length of the longest parameter vector.  NA for all sampling parameters
#' results in a complete unweighted unstratified sample for that stage.
#'
#' Wunderscraper uses the following template for building api queries:
#' \code{http://api.wunderground.com/conditions/q/<query>.json}
#' wunderscrape returns the value of each query, and can either write the raw
#' json to a file or collect the sample and save all stations as a dataframe in
#' rds format.
#'
#' @param scheduler A scheduler object.
#' @param id A vector of strings specifying variable names for cluster
#'   identifiers.  The id of the last stage must be "id".  If "id" is missing
#'   but 'scrape' can unambiguously assume the last stage is "id" then it will
#'   do so with a warning, otherwise 'scrape' will raise an error message.  The
#'   unit identifiers of the second-to-last stage will also supply the 'q'
#'   parameters for Wunderground geolookups.  The 'q' parameter must be a zip
#'   code, city name, or latitude/longitude.  Zip codes must have 5 digits.  City
#'   names must be strings with underscores for spaces.  Latitude/longitude must
#'   be a string of two floating point numbers separated by a comma.  Data that
#'   does not meet these requirements may find no results from the Wunderground
#'   API, or may cause an error.
#' @param size A vector of integers specifying sample size at each stage. NA
#'   values specify complete sampling.  If not specified for all stages then
#'   unspecified stages are assumed complete sampling.
#' @param strata A vector of strings specifying variable names for strata.  NA
#'   values indicate simple sampling.  Wunderscraper will repeat sampling in
#'   each strata.
#' @param weight A vector of strings specifying variable names for numeric
#'   variables that indicate sampling weights.  NA values specify unweighted
#'   sampling.
#' @param cellsize A vector of numerics specifying cellsize for adding grids to
#'   TIGER county geometries; grids larger than the scale of a county should be
#'   added directly to the sampleFrame.  TIGER geometries are in the unit of
#'   latitude-longitude degrees.  value of NA specifies no grid.  The grids will
#'   be available to the next stage with the identifying variable GRID.
#' @param sampleFrame A dataframe representing the sampling frame.  The
#'   dataframe must contain columns named "STATEFP" and "GEOID", along with
#'   columns for any data required by the sampling strategy.  Defaults to
#'   \code{\link{zctaRel}}.
#' @param form A character string specifying output format.  An NA value sends
#'   output to standard out and will always be in JSON format.  Possible formats
#'   are: "json"; any other value will currently result in an error with the
#'   message "not implemented".
#' @param o A character string specifying output directory or file.  If
#'   \code{form='json'} then this will be a directory with each station, other
#'   formats are not yet supported.
#' @return Wunderscrape may output the data directly to a file or to standard
#'   out.  All file output is named by the station identification code and date
#'   in epoch time.
#' @seealso \code{\link[rwunderground]{conditions}}
#' @examples
#' \dontrun{
#' ## ?setApiKey before running examples
#' schedulerMMDD <- scheduler()
#' ## select random county and sample one station from each 0.01 arc degrees
#' ## (roughly 1km^2 at the equator)
#' scrape(schedulerMMDD, c("GEOID", "ZCTA5"), size=c(1, NA, 1), strata=c(NA, NA, "GRID"),
#'        weight="COPOP", cellsize=c(NA, 0.01))
#' ## same, but limit sampling to southeastern US
#' data(zctaRel)
#' SE <- c("01", "05", "12", "13", "21", "22", "24", "28", "37", "45", "47", "51", "54")
#' scrape(schedulerMMDD, c("GEOID", "ZCTA5"), size=c(1, NA, 1), strata=c(NA, NA, "GRID"),
#'        weight="COPOP", cellsize=c(NA, 0.01), sampleFrame=zctaRel[zctaRel $STATEFP %in% SE, ])
#' ## select two states and in each state select a 1 arc degree area (roughly
#' ## 100km^2 at the equator) and sample five zip codes, each stratified into
#' ## 0.01 arc degree areas
#' scrape(schedulerMMDD, c("STATEFP", "GRID", "ZCTA5"), size=c(2, 1, 5, 1),
#'        strata=c(NA, "STATEFP", "GRID", "GRID"), cellsize=c(1, NA, 0.01))
#' ## periodically resample one location
#' sampleFrame <- with(zctaRel, zctaRel[GEOID==sample(GEOID, 1, weight=COPOP), ])
#' plan(schedulerMMDD, '2 hours')
#' repeat {
#'   scrape(schedulerMMDD, "ZCTA5", strata=c(NA, "GRID"), cellsize=0.01, sampleFrame=sampleFrame)
#'   sync(schedulerMMDD) # sync schedule after each sample to wait for next scheduled sample
#' }
#' ## stratify by rural and urban to ensure both types of areas recieve adequate representation
#' zctaRel $RURAL <- log(zctaRel $COPOP) < 10
#' scrape(schedulerMMDD, c("GEOID", "ZCTA5"), size=c(1, 8, 1), strata=c("RURAL", "RURAL", "GRID"),
#'        weight="COPOP", cellsize=c(NA, 0.01), sampleFrame=zctaRel)
#' }
#' @export
scrape <- function(scheduler, id, size=NA, strata=NA, weight=NA, cellsize=NA,
                   sampleFrame=wunderscraper::zctaRel, form='json', o=NA) {
    stations <- .wuSample(scheduler, id, size, strata, weight, cellsize, sampleFrame)
    if(!is.na(o)) dir.create(o)
    for(station in sample(stations)) .writeResponse(.wuConditions(scheduler, station), form, o)
}
