calc_gdp <- function(x, year = NULL, country = NULL){
	if (!is.null(year)) {
		x <- x[x$year %in% year, ]
	}
	if (!is.null(country)) {
		x <- x[x$country %in% country, ]
	}
	gdp <- x$pop * x$gdpPercap
	new <- cbind(x, gdp=gdp)
	new
}