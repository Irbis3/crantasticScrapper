reH1Setext <- "^#{1}[^#]\\s*(.*?)\\s*[#\\s]*$"
reH2Setext <- "^#{2}[^#]\\s*(.*?)\\s*[#\\s]*$"
reHxSetext <- "^#{3,}"

reH1Atx <- "^[=]{3,}+\\s*$"
reH2Atx <- "^[-]{3,}+\\s*$"

reBullet <- "^[-*]+\\s*(.*?)"

data_frame <- function(...) data.frame(..., stringsAsFactors = FALSE)

set_names <- `names<-`

strip_whitespace <- function(content) {
  
  begin <- 1
  while (grepl("^\\s*$", content[[begin]], perl = TRUE))
    begin <- begin + 1
  
  end <- length(content)
  while (grepl("^\\s*$", content[[end]], perl = TRUE))
    end <- end - 1
  
  content[begin:end]
}

normalize <- function(text) {
  stripped <- sub("^\\s*(.*?)\\s*$", "\\1", text)
  gsub("\\n\\s*", " ", stripped, perl = TRUE)
}

extract_ranges <- function(content, reSetext, reAtx) {
  
  setextLocations <- grep(reSetext, content, perl = TRUE)
  setext <- data_frame(
    name = sub(reSetext, "\\1", content[setextLocations], perl = TRUE),
    index = setextLocations,
    is.setext = if (length(setextLocations)) TRUE else logical()
  )
  
  atxLocations <- grep(reAtx, content, perl = TRUE) - 1
  atx <- data_frame(
    name = content[atxLocations],
    index = atxLocations,
    is.setext = if (length(atxLocations)) FALSE else logical()
  )
  
  all <- rbind(setext, atx)
  sorted <- all[order(all$index),]
  sorted$begin <- sorted$index + 2 - sorted$is.setext
  sorted$end <- if (nrow(sorted))
    c(
      tail(sorted$index, n = -1) - 1,
      length(content)
    )
  else numeric()
  
  sorted
  
}

h1_ranges <- function(content) extract_ranges(content, reH1Setext, reH1Atx)
h2_ranges <- function(content) extract_ranges(content, reH2Setext, reH2Atx)

parse_changes <- function(changes) {
  ranges <- h2_ranges(changes)
  
  if (!nrow(ranges))
    ranges <- data_frame(
      name = "CHANGES",
      index = 1,
      begin = 1,
      end = length(changes)
    )
  
  set_names(lapply(seq_len(nrow(ranges)), function(i) {
    
    bullets <- strip_whitespace(changes[ranges$begin[[i]]:ranges$end[[i]]])
    
    indices <- grep(reBullet, bullets, perl = TRUE)
    if (!length(indices)) return("")
    
    preamble <- if (indices[[1]] > 1)
      normalize(paste(bullets[seq(to = indices[[1]] - 1)], collapse = "\n"))
    
    boundaries <- c(indices, length(bullets) + 1)
    
    output <- lapply(seq(to = length(boundaries) - 1), function(j) {
      text <- bullets[seq(from = boundaries[[j]], to = boundaries[[j + 1]] - 1)]
      text[1] <- sub(reBullet, "\\1", text[1], perl = TRUE)
      normalize(paste(text, collapse = "\n"))
    })
    
    if (length(preamble))
      attr(output, "preamble") <- preamble
    
    output
    
  }), ranges$name)
}

parse_news_md <- function(content) {
  ranges <- h1_ranges(content)
  set_names(lapply(seq_len(nrow(ranges)), function(i) {
    changes <- content[seq(from = ranges$begin[[i]], to = ranges$end[[i]])]
    parse_changes(changes)
  }), ranges$name)
}

if (!requireNamespace("curl", quietly = TRUE))
  install.packages("curl")

if (!exists("content")) {
  conn <- curl::curl("https://raw.githubusercontent.com/hadley/dplyr/master/NEWS.md")
  content <- readLines(conn)
  close(conn)
}

parsed <- parse_news_md(content)
