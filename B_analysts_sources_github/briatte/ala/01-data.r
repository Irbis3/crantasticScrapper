# ==============================================================================
# TOPICS
# ==============================================================================

if (!file.exists(f_tags)) {
  
  write_csv(
    read_html("http://alistapart.com/articles") %>%
      html_nodes("article.topic ul") %>%
      lapply(function(x) {
        data_frame(
          parent = html_node(x, "li.parent-topic a") %>%
            html_attr("href") %>%
            str_replace("/topic/", ""),
          tag = html_nodes(x, "li a") %>%
            html_attr("href") %>%
            str_replace("/topic/", "")
        )
      }) %>%
      bind_rows,
    f_tags
  )
  
}

# ==============================================================================
# ARTICLES
# ==============================================================================

cat("Source:", r <- "http://alistapart.com", "\n")

if (!file.exists(f_list)) {
  
  d <- data_frame()
  
  i <- str_c(r, "/articles")
  
  while (length(i) > 0) {
    
    cat("Downloading article listing", i, "\n")
    h <- read_html(i)
    
    d <- rbind(d, data_frame(
      topic = i,
      title = html_nodes(h, "ul.entry-list h3.entry-title") %>%
        html_text,
      url = html_nodes(h, "ul.entry-list h3.entry-title a") %>%
        html_attr("href"),
      date = html_nodes(h, "ul.entry-list p.meta time") %>%
        html_attr("datetime"),
      au_url = html_nodes(h, "ul.entry-list p.meta") %>% 
        lapply(html_nodes, "a.author") %>% 
        lapply(html_attr, "href") %>% 
        sapply(str_c, collapse = ";"),
      au_id = html_nodes(h, "ul.entry-list p.meta") %>% 
        lapply(html_nodes, "a.author") %>% 
        lapply(html_text) %>% 
        sapply(str_c, collapse = ";")
    ))
    
    i <- html_nodes(h, "nav.paginator ul li a.previous") %>%
      html_attr("href")
    
  }
  
  stopifnot(!duplicated(d$url))
  
  write_csv(d, f_list)
  
}

# ==============================================================================
# DOWNLOAD ARTICLES
# ==============================================================================

u <- str_c(r, read_csv(f_list, col_types = s_list)$url)
stopifnot(!duplicated(u))

f <- file.path("raw", str_c(basename(u), ".html")) %>%
  file.exists %>%
  sum

if (f != length(u)) {
  
  cat("Downloading articles...\n")
  p <- txtProgressBar(0, length(u), style = 3)
  
  for (i in u) {
    
    f <- file.path("raw", str_c(basename(i), ".html"))
    if (!file.exists(f)) {
      try(download.file(i, f, mode = "wb", quiet = TRUE), silent = TRUE)
    }
    setTxtProgressBar(p, which(u == i))
    
  }
  
}

cat("\n")

# ==============================================================================
# DETAILS
# ==============================================================================

d <- read_csv(f_list, col_types = s_list) %>%
  select(url) %>%
  mutate(description = NA_character_, tags = NA_character_)

if (!file.exists(f_info)) {

  cat("Parsing articles...\n")
  u <- rev(d$url[ is.na(d$description) ])
  p <- txtProgressBar(0, length(u), style = 3)
  
  for (i in u) {
    
    setTxtProgressBar(p, which(u == i))
    
    h <- file.path("raw", str_c(basename(i), ".html")) %>%
      read_html
    
    j <- which(d$url == i)

    if (!html_nodes(h, "meta[name='description']") %>%
        length) {
      
      warning("Skipped:", d$url[ j ])
      next
      
    }
    
    d$description[ j ] <- html_nodes(h, "meta[name='description']") %>%
      html_attr("content")
    
    d$tags[ j ] <- html_nodes(h, "p.entry-details span.entry-meta a") %>%
      lapply(html_attr, "href") %>%
      str_c(collapse = ";") %>%
      ifelse(length(.), ., NA)
    
  }
  
  cat("\n")
  write_csv(d, f_info)
  
}

# ==============================================================================
# FINALIZE
# ==============================================================================

d <- left_join(
  read_csv(f_info, col_types = s_info),
  read_csv(f_list, col_types = s_list) %>%
    select(-page),
  by = "url"
)

d$date <- as.Date(d$date)
d$url <- str_replace(d$url, "^/(article|column)/", "")
d$tags <- str_replace_all(d$tags, "/topic/", "")
d$au_url <- str_replace_all(d$au_url, "/author/", "")

# empty fields:

sum(is.na(d$description)) # 0
sum(is.na(d$tags))        # ~ 156

# special authors:

table(d$au_id[ str_detect(d$au_id, "[A-Z0-9]{2,}") ])

d <- select(d, date, url, tags, au_url, au_id, title, description) %>%
  mutate(year = str_sub(date, 1, 4) %>%
           as.integer) %>%
  filter(year %in% 1999:2016) %>% # drop single 2017 article
  filter(!is.na(tags)) # drop blog posts, 2013-2015

write_csv(d, f_data)

# show dataset:

glimpse(d)

# ==============================================================================
# CITATIONS ('REFS')
# ==============================================================================

if (!file.exists(f_refs)) {
  
  cat("Finding citations...\n")
  
  f <- file.path("raw", str_c(d$url, ".html"))
  stopifnot(file.exists(f))
  
  e <- data_frame()
  z <- c() # faulty pages where the <div> match (j) goes too far
  # see http://stackoverflow.com/q/41532698/635806 for an explanation
  
  p <- txtProgressBar(0, length(f), style = 3)
  
  for (i in f) {
    
    h <- read_html(i)
    
    # all crossrefs
    j <- html_nodes(h, "div.main-content[itemprop='articleBody'] a") %>%
      html_attr("href") %>%
      basename
    
    j <- j[ j %in% d$url ] # might contain multiple cross-refs
    
    # <aside#related> and <footer> elements
    h <- html_nodes(h, ".nocontent a") %>%
      html_attr("href") %>%
      basename
    
    h <- h[ h %in% j ] # relevant refs
    
    k <- identical(j[ (1 + length(j) - length(h)):length(j) ], h)
    
    if (!length(j)) {
      next
    } else if (!k) {
      z <- c(z, i) # pages with faulty HTML
      next
    } else if (length(h) > 0) {
      j <- j[ 1:(length(j) - length(h)) ] # e.g. j[1:2] when h = j[3:4]
    }
    
    # j might be empty again
    if (!length(j)) {
      next
    }
    
    e <- rbind(e, data_frame(i, j))
    setTxtProgressBar(p, which(f == i))
    
  }
  cat("\n")
  
  e$i <- str_replace(basename(e$i), "\\.html", "")
  
  e <- filter(e, i != j) %>%
    group_by(i, j) %>%
    tally # n = citation weight
  
  if (length(z)) {
    
    z <- str_replace(basename(z), "\\.html", "")
    cat("Removing", length(z), "unparsed pages:", str_c("\n - ", z))
    
    d <- filter(d, !url %in% z)
    e <- filter(e, !i %in% z, !j %in% z)
    
  }
  
  write_csv(e, f_refs)
  
}

e <- read_csv(f_refs, col_types = s_refs)

stopifnot(e$i %in% d$url)
stopifnot(e$j %in% d$url)

# articles with most outgoing links
group_by(select(e, -n), i) %>%
  tally %>%
  arrange(-n) %>%
  rename(url = i) %>%
  left_join(d, by = "url") %>%
  select(date, url, au_url, n)

# articles with most incoming links
group_by(select(e, -n), j) %>%
  tally %>%
  arrange(-n) %>%
  rename(url = j) %>%
  left_join(d, by = "url") %>%
  select(date, url, au_url, n)

# % of articles citing another one
100 * n_distinct(e$i) / n_distinct(d$url)

# % of articles cited by another one
100 * n_distinct(e$j) / n_distinct(d$url)

# kthxbye
