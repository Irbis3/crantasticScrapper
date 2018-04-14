###--------------------------------------------------
### Scraping ATP shownotes
###--------------------------------------------------

library(ggplot2)
library(scales)
library(dplyr)
library(rvest)
library(stringr)
library(car)
library(knitr)

theme_set(theme_minimal())

## Convenience "not in" function
"%nin%" <- function(x, y) {
  return( !(x %in% y) )
}


### http://wresch.github.io/2013/03/08/asinh-scales-in-ggplot2.html
### Inverse hyperbolic sine transformation
### instead of a log transform

asinh_breaks <- function(x) {
  br <- function(r) {
    lmin <- round(log10(r[1]))
    lmax <- round(log10(r[2]))
    lbreaks <- seq(lmin, lmax, by = 1)
    breaks <- 10 ^ lbreaks
  }
  p.rng <- range(x[x > 0], na.rm = TRUE)
  breaks <- br(p.rng)
  if (min(x) <= 0) {breaks <- c(0, breaks)}
  if (sum(x < 0) > 1) { #more negative values that expected from expanding scale that includes zero
    n.rng <- -range(x[x < 0], na.rm = TRUE)
    breaks <- c(breaks, -br(n.rng))
  }
  return(sort(breaks))
}
test_that("asinh_breaks make sense", {
  expect_equal(asinh_breaks(c(-0.05, 0, 1, 101)), c(0, 1, 10, 100))
  expect_equal(asinh_breaks(c(-0.11, -0.05, 0, 1, 101)), c(-0.1, 0, 1, 10, 100))
  expect_equal(asinh_breaks(c(0, 10, 1001)), c(0, 10, 100, 1000))
  expect_equal(asinh_breaks(c(0, 0.05, 0.07, 0.1, 0.2)), c(0, 0.1))
  expect_equal(asinh_breaks(c(0.01, 0.02)), c(0.01))
})

asinh_trans <- function() {
  trans_new("asinh",
            transform = asinh,
            inverse   = sinh)
}


get.shownotes <- function(episode) {
    name <- episode %>% html_nodes(".entry-title") %>% html_text()
    ep.n <- as.numeric(sapply(str_split(name, ": "), "[", 1))
    link.url <- episode %>% html_nodes("li") %>% html_nodes("a") %>% html_attr("href")
    link.title <- episode %>% html_nodes("li") %>% html_nodes("a") %>% html_text()
    shownotes.df <- data.frame(Number=ep.n,
                               Episode = name,
                               Link.url = link.url,
                               Link.title = link.title)
    ## Clean
    shownotes.df <- shownotes.df %>% filter(Link.title %nin% c("Episodes",
                                          "Feedback",
                                          "Twitter",
                                          "RSS",
                                          "iTunes",
                                          "Listen Live",
                                          "Sponsor",
                                          "Newer",
                                          "Older"))
    return(shownotes.df)
}


## Collapse a character vector into a regexp suitable for str_detect
make.reg <- function(x){
    o <- do.call(paste(x, sep="|"))
}


## Make a "figures" subdirectory if one doesn't exist
ifelse(!dir.exists(file.path("figures")), dir.create(file.path("figures")), FALSE)

###--------------------------------------------------
### Make a list of episode titles and urls
###----------------------------------------

## Early episodes have annoying urls
track.list <- read_html("https://itunes.apple.com/us/podcast/accidental-tech-podcast/id617416468")
old.episodes <- track.list %>% html_nodes(".track-list")  %>% html_nodes(".name .text") %>% html_text()

old.ep.n <- str_split(old.episodes, ": ")
old.ep.n <- sapply(old.ep.n, "[", 1)

old.ep.df <- data.frame(N=as.numeric(old.ep.n),
                    Title=old.episodes, stringsAsFactors=FALSE)
old.ep.df <- old.ep.df %>% filter(N<60)
old.ep.df$Title <- str_replace_all(old.ep.df$Title, "!|,|'|;", "")
old.ep.df$Title <- str_replace(old.ep.df$Title, ": ", "-")
old.ep.df$Title <- str_replace_all(old.ep.df$Title, " ", "-")
old.ep.df$Title <- str_to_lower(old.ep.df$Title)
old.ep.df$URL <- as.character(paste("http://atp.fm/episodes/", old.ep.df$Title, sep=""))

## Rational episode URLs start at episode 60
start.episodes <- 60
end.episodes <- 117
ep.urls <- paste("http://atp.fm/episodes/", c(start.episodes:end.episodes), sep="")
new.episodes.df <- data.frame(N=c(start.episodes:end.episodes), URL=as.character(ep.urls),
                              stringsAsFactors=FALSE)

all.episodes.df <- rbind(old.ep.df[,c("N", "URL")], new.episodes.df)


###--------------------------------------------------
### Get the shownote links data
###-------------------------------------------------

###  And if we've done all *this* already, don't do it again
if(!file.exists("data/atp-shownotes.csv")){
### Get and store all constituency pages and data
    all.results.list <- list()
    for(i in 1:nrow(all.episodes.df)){
        url <- all.episodes.df$URL[i]
        n <- all.episodes.df$N[i]
        page <- read_html(url)
        name <- page %>% html_nodes(".entry-title") %>% html_text()
        fname <- paste("data/", name, ".rda", sep="")
        save(page, file=fname)

        all.results.list[[n]] <- get.shownotes(page)
        message(paste("Completed", name))
        Sys.sleep(1) ## Try to be polite

    }

    data <- rbind_all(all.results.list)
    data <- data %>% arrange(Number)

    ### Create a few variables for tabulation, flagging
    ### Awkward
    data$Link.url <- str_replace_all(data$Link.url, "®", "")

    library(XML)

    parsedurls <- lapply(data$Link.url, parseURI)
    baseurls <- as.character(sapply(parsedurls, "[", "server"))
    urlpath <- as.character(sapply(parsedurls, "[", "path"))

    data$BaseURL <- baseurls
    data$Path <- urlpath

    sponsor <- c("squarespace.com",
                 "hover.com",
                 "lynda.com",
                 "fractureme.com",
                 "filetransporter.com",
                 "backblaze.com",
                 "harrys.com",
                 "atp.ting",
                 "igloosoftware",
                 "warbyparker",
                 "cardsagainsthumanity",
                 "caspersleep",
                 "audiblepodcast",
                 "newrelic",
                 "automatic.com")

    tmp <- paste(sponsor, sep="", collapse="|")
    sponsor.reg <- paste("(",tmp,")",sep="")


    host <- c("hypercritical",
              "marco.org",
              "caseyliss",
              "atp.fm",
              "neutral.fm")

    tmp <- paste(host, sep="", collapse="|")
    host.reg <- paste("(",tmp,")",sep="")


    data$Sponsor <- str_detect(data$BaseURL, sponsor.reg)
    data$Host <- str_detect(data$BaseURL, host.reg)


    ## Data file
    write.csv(data, "data/atp-shownotes.csv")

} else {
    data <- read.csv("data/atp-shownotes.csv", header=TRUE, row.names=1)
}

###--------------------------------------------------


###--------------------------------------------------
### Looking at the data; som summary tables
###--------------------------------------------------

by.url.all <- data %>% group_by(BaseURL) %>% tally() %>% arrange(desc(n)) %>% data.frame()

## Exclude sponsors and hosts from tabulation (imperfect as e.g. lots
## of ars technica links are John's). Five or more links only.

by.url.top <- data %>% group_by(BaseURL) %>%
    filter(Sponsor==FALSE & Host==FALSE) %>% tally() %>%
        filter(n>4) %>%
        arrange(desc(n)) %>%
        data.frame()

wiki.table <- data %>% filter(BaseURL=="en.wikipedia.org") %>%
    select(Number, Link.title) %>% data.frame()


youtube.table <- data %>% filter(BaseURL=="www.youtube.com") %>%
    select(Number, Link.title) %>% data.frame()

amazon.table <- data %>% filter(BaseURL=="www.amazon.com") %>%
    select(Number, Link.title) %>% data.frame()

imore.table <- data %>% filter(BaseURL=="www.imore.com") %>%
    select(Number, Link.title) %>% data.frame()

fbf.table <- data %>% filter(BaseURL=="5by5.tv") %>%
    select(Number, Link.title) %>% data.frame()

## Mostly Siracusa
ars.table <- data %>% filter(BaseURL=="arstechnica.com") %>%
    select(Number, Link.title) %>% data.frame()

verge.table <- data %>% filter(BaseURL=="www.theverge.com") %>%
    select(Number, Link.title) %>% data.frame

github.table <- data %>% filter(BaseURL=="github.com") %>%
    select(Number, Link.title) %>% data.frame

shortlink.table <- data %>% filter(BaseURL=="goo.gl"|BaseURL=="cl.ly") %>%
    select(Number, Link.title) %>% data.frame


relay.table <- data %>% filter(BaseURL=="relay.fm") %>%
    select(Number, Link.title) %>% data.frame


### Most-linked Tweeters
by.tweet <- data %>% filter(BaseURL=="twitter.com")
tweeter <- str_replace(by.tweet$Path, "\\/status/.*", "")
tweeter <- str_replace(tweeter, "/","")
tweeter <- data.frame(obs=c(1:length(tweeter)),
                      id=tweeter)
tweet.table.all <- tweeter %>% group_by(id) %>% tally() %>% arrange(desc(n)) %>% data.frame()

tweet.table.top <- tweeter %>% group_by(id) %>%
    tally() %>% arrange(desc(n)) %>%
        filter(n>1 & id %nin% c("marcoarment", "siracusa", "caseyliss")) %>% data.frame()


###--------------------------------------------------
## Graphs and Tables
###--------------------------------------------------

p <- ggplot(by.url.top, aes(x=n, y=reorder(BaseURL, n)))
p0 <- p + geom_point(size=3) +
    labs(x="Link Count", y="") +
        scale_x_continuous(trans=asinh_trans(),
                           breaks=c(0, 5, 10, 25, 50, 100, 200))
print(p0)

ggsave("figures/atp-link-count.png",
       p0,
       height=7,
       width=5,
       dpi=300)

p <- ggplot(tweet.table.top, aes(x=n, y=reorder(id, n)))
p0 <- p + geom_point(size=3) +
    labs(x="Tweets Linked, Count", y="") +
        scale_x_continuous()
print(p0)

ggsave("figures/atp-tweet-count.png",
       p0,
       height=5,
       width=7,
       dpi=300)


library(knitr)

kable(github.table, format = "markdown")

kable(verge.table, format = "markdown")
