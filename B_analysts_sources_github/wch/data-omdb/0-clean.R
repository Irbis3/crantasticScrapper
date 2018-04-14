library(dplyr)

# Minimum number of Rotten Tomatoes reviews to keep a movie in the database
min_reviews <- 10


# With the Feb 4, 2014 version of the dataset from omdbapi.com,
# the following IDs have an extra tab in the Plot field. I've removed them
# by hand. This should be automated in the future.
# 1308026
omdb_raw <- read.table("raw/omdb.txt", header = TRUE,
  sep = "\t", quote = "", na.strings = c("", "N/A"), stringsAsFactors = FALSE,
  comment.char = "", fileEncoding = "latin1"
)

# Do some rows have problems?
bad_id_idx <- which(is.na(as.integer(omdb_raw$ID)))
if (length(bad_id_idx) > 0) {
  stop("Problem with ID fields for some entries. It's possible the previous entries had extra tabs.")
}

# Clean up the data
omdb <- plyr::mutate(omdb_raw,
  Genre = as.factor(Genre),
  imdbVotes = as.integer(gsub(",", "", imdbVotes)),
  Runtime = local({
    # Convert strings of form "1 h 20 min", "1 h", and "15 min" to numeric minutes
    h <- Runtime
    h[!grepl(" h", h)] <- 0
    h <- as.numeric(sub("(\\d+) h.*", "\\1", h))
    m <- Runtime
    m[!grepl(" min", m)] <- 0
    m <- as.numeric(sub(".*?(\\d+) min", "\\1", m))
    h*60 + m
  }),
  Poster = NULL,
  Plot = NULL,
  FullPlot = NULL,
  Oscars = local({
    awards <- Awards
    awards[!grepl("^Won \\d+ Oscar", awards)] <- "0"
    awards <- sub("Won (\\d+) Oscar.*", "\\1", awards)
    as.integer(awards)
  })
)


# Load Rotten Tomatoes ratings
tomatoes_raw <- read.table("raw/tomatoes.txt", header = TRUE,
  sep  = "\t", quote = "",
  na.strings = "N/A", stringsAsFactors = FALSE, comment.char = "",
  fileEncoding = "latin1"
)

# Clean up Tomatoes data
tomatoes <- plyr::mutate(tomatoes_raw,
  userReviews = as.integer(gsub(",", "", userReviews)),
  BoxOffice = local({
    bo <- sub("^\\$", "", BoxOffice)
    val <- as.numeric(sub("M|k", "", bo))
    k <- grepl("k", bo)
    val[k] <- val[k] * 1000
    m <- grepl("M", bo)
    val[m] <- val[m] * 1000000
    val
  })
)

# Only keep omdb entries for which there is tomatoes data, and there are
# >= 10 reviews
keep_ids <- tomatoes$ID[tomatoes$Reviews >= min_reviews]
omdb <- omdb[omdb$ID %in% keep_ids, ]

# Drop specific movies that have issues
drop_idx <-
  # TV series with a long runtime
  (omdb$Title == "Oliver Twist" & omdb$Year == 1999) |
  # NA for running time
  (omdb$Title == "Short Term 12" & omdb$Year == 2008)

omdb <- omdb[!drop_idx, ]



# Remove existing database if present
unlink("movies.db")

# Store in database
db <- src_sqlite("movies.db", create = TRUE)
omdb <- copy_to(db, omdb, temporary = FALSE, indexes = list("ID", "Year"))
tomatoes <- copy_to(db, tomatoes, temporary = FALSE,
  indexes = list("ID", "Meter", "Rating"))
