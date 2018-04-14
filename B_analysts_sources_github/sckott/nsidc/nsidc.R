library("crul")

base_url <- "https://nsidc.org/api/dataset/2/OpenSearch"

nsidc_scrape <- function(query = "*:*", count = 100, start = 1, ...) {
  cli <- crul::HttpClient$new(url = base_url)
  args <- list(searchTerms = query, startIndex = start, count = count)
  res <- cli$get(query = args, ...)
  xml <- xml2::read_xml(res$parse("UTF-8"))
  xml2::xml_ns_strip(xml)
  entries <- xml2::xml_find_all(xml, ".//entry")

  get <- c('id', 'title', 'updated', 'temporal_duration', 'spatial_area')
  invisible(lapply(entries, function(z) {
    out <- unlist(lapply(get, function(w) {
      tmp <- xml2::xml_find_all(z, paste0(".//", w))
      as.list(stats::setNames(xml2::xml_text(tmp), xml2::xml_name(tmp)))
    }), FALSE)
    links <- list(links =
      lapply(xml2::xml_find_all(z, ".//link"), function(w) {
        list(
          href = xml2::xml_attr(w, "href"),
          rel = xml2::xml_attr(w, "rel"),
          type = xml2::xml_attr(w, "type"),
          description = xml2::xml_attr(w, "description")
        )
      })
    )
    formats <- list(
      formats =
        xml2::xml_text(
          xml2::xml_find_all(z, ".//dif:Distribution_Format"))
    )
    dat <- c(out, links, formats)

    tmp <- jsonlite::toJSON(dat, auto_unbox = TRUE)
    cat(tmp, sep = "\n", file = "nsidc_from_opensearch2.json", append = TRUE)
  }))
}

# scrape all pages
invisible(
  lapply(c(1, seq(100, 1000, 100)), function(z) nsidc_scrape(start = z))
)

