library(dplyr)

# Load the database tables
db <- src_sqlite("movies.db")
omdb <- tbl(db, "omdb")
tomatoes <- tbl(db, "tomatoes")

# Join the tables, keeping only the movies that have any reviews from Rotten Tomatoes
movies <- inner_join(omdb, tomatoes, by = "ID") %>%
  filter(Reviews > 0) %>%
  select(ID, imdbID, Title, Year, Rating_m = Rating.x, Runtime, Genre, Released, Director, Writer,
    Cast, imdbRating, imdbVotes, Language, Country, Oscars, Rating = Rating.y, Meter,
    Reviews, Fresh, Rotten, userMeter, userRating, userReviews, BoxOffice, Production)


# Filter for movies that have Tomato Meter rating > 80
m <- movies %>% filter(Meter > 80)

# Convert to a data frame
m2 <- as.data.frame(m)
str(m2)

hist(m2$Rating.y)


# Other sample queries
m <- movies %>% filter(Director %like% "%Miyazaki%") %>% compute()
m2 <- as.data.frame(m)

movies %>% filter(Director == "Woody Allen") %>%
  select(Title, Year, Meter) %>%
  arrange(-Meter) %>%
  compute()

movies %>% filter(Cast %like% "%Tom Hanks%") %>%
  select(Title, Year, Meter) %>%
  arrange(-Meter) %>%
  compute()
