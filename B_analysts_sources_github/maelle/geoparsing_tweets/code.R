library("rtweet")
library("purrr")
library("dplyr")
library("geoparser")
library("ggplot2")
library("viridis")
library("ggmap")
library("rworldmap")
library("magick")
worldMap <- map_data(map="world")

geoparser_q2 <- function(df){
  return(geoparser_q(df$text)$results)
}


# trump_tweets <- get_timeline("realDonaldTrump", n = 2000)
# trump_tweets <- trump_tweets$tweets
# 
# clinton_tweets <- get_timeline("HillaryClinton", n = 2000)
# clinton_tweets <- clinton_tweets$tweets
# 
# 
# # get results from geoparser
# results <- bind_rows(trump_tweets, clinton_tweets) %>%
#   rename(twitter_country = country) %>%
#   purrr::by_row(geoparser_q2)
# 
# # filter only when results
# results <- results %>%
#   dplyr::group_by(status_id) %>%
#   dplyr::mutate(length = nrow(.out[[1]]))
# results <- dplyr::filter(results, length > 0)
# results <- results %>%
#   unnest(.out)
# # not the country only
# results <- filter(results, name != "United States")
# 
# # useless columns
# results <- select(results, - (long1:lat4))
# 
# # when two results take the city
# 
# # day
# results <- mutate(results,
#                   day = as.Date(created_at))
# 
# results <- filter(results,
#                   status_id != "705903554377052162")
# 
# # no link
# results <- mutate(results,
#                   text = gsub("https.*", "", text))
#save(results, file = "results.RData")
load("results.RData")
results <- mutate(results,
                  user_id = ifelse(user_id == "25073877",
                                   "realDonaldTrump", "HillaryClinton"))

map_tweet <- function(i, results){
  results2 <- filter(results, as.character(created_at) == i)
  # map
  gg <- ggplot() + geom_map(data=worldMap, map=worldMap,
                            aes(map_id=region, x=long, y=lat),
                            fill = "grey60") + 
    geom_point(data = results,
               aes(longitude, latitude),
               col = "grey50") + 
    geom_point(data = results2,
               aes(longitude, latitude, frame = created_at)) +
    geom_text(data = results2,
              aes(0, -50, label = text, frame = created_at)) +
    theme_void() +
    facet_grid(user_id ~ .) +
    scale_color_viridis(discrete = TRUE)
  name <- gsub(" ", "", paste0("picture", i, ".PNG"))
  name <- gsub("\\:", "-", name)
  ggsave(filename = name, plot = gg, width = 8, height = 12)
  return(name)
}

as.character(sort(unique(results$created_at))) %>%
  purrr::map(map_tweet, results) %>%
  purrr::map(image_read) %>%
  image_join() %>%
  image_animate(fps=2, loop=1) %>%
  image_write("map.gif")