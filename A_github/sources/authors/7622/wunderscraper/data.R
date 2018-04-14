#' Relationships for zip codes
#' 
#' A dataset containing population and land area of zip codes and administrative
#' boundaries
#'
#' @format A data frame with 44410 rows and 8 variables:
#' \describe{
#'   \item{ZCTA5}{zip code}
#'   \item{COUNTYFP}{3 character string indicating county Federal Information
#'     Processing Standard (FIPS) code}
#'   \item{STATEFP}{2 character string indicating state FIPS code}
#'   \item{GEOID}{5 character string concatenating STATEFP and COUNTYFP}
#'   \item{STPOP}{state population}
#'   \item{COPOP}{county population}
#'   \item{ZPOP}{zip code population}
#'   \item{POPPT}{partial zip code population, one value for each county
#'     intersecting zip code}
#' }
#' @source \url{https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt}
#' @examples
#' data(zctaRel)
#' summary(zctaRel)
"zctaRel"
