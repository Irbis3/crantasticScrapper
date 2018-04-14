# motions

if (!file.exists("data/bills-tk.csv")) {

  dd <- function(h, x, url = TRUE) {
    x = html_nodes(h, xpath = paste0("//div[@class='paper-header']",
                                "//dt[text()='", x,
                                "']/following-sibling::dd[1]"))
    if (url) {
      html_nodes(x, xpath = "a[1]") %>% html_attr("href")
    } else {
      html_text(x)
    }
  }

  r = "http://www.tweedekamer.nl/kamerstukken/moties"

  for (i in 2012:2015) {

    cat("Year", i)
    f = paste0("raw/tk/bill-lists/bills-", i, "-1.html")
    if (!file.exists(f)) {

      download.file(paste0(r, "?qry=%2A&fld_tk_categorie=Kamerstukken&srt=date%3Adesc%3Adate&clusterName=Moties&Type=Kamerstukken&fld_prl_kamerstuk=Moties&fromdate=", ifelse(i == 2012, "20%2F09", "01%2F01"),
                           "%2F", i, "&todate=31%2F12%2F", i, "&sta=1"), f, mode = "wb", quiet = TRUE)

    }

    h = read_html(f) %>%
      html_nodes(xpath = "//a[contains(@href, '&sta=')]") %>%
      html_attr("href")

    h = gsub("(.*)&sta=", "", h[ grepl("sta=\\d+$", h) ]) %>%
      as.integer

    if (!length(h)) {

      h = c()
      cat(": no data")

    } else {

      h = seq(1, max(h), 15)
      cat(":", length(h), "pages\n")

    }

    for (j in rev(h)) {

      cat(ifelse(j %% 100 < 15, which(j == h), "."))

      f = paste0("raw/tk/bill-lists/bills-", i, "-", j, ".html")
      if (!file.exists(f)) {

        download.file(paste0(r, "?qry=%2A&fld_tk_categorie=Kamerstukken&srt=date%3Adesc%3Adate&clusterName=Moties&Type=Kamerstukken&fld_prl_kamerstuk=Moties&fromdate=01%2F01%2F", i, "&todate=31%2F12%2F", i, "&sta=", j), f, mode = "wb", quiet = TRUE)

      }

    }

    cat("\n\n")

  }

  j = list.files("raw/tk/bill-lists", full.names = TRUE)
  b = data_frame()

  for (i in rev(j)) {

    cat("Page", str_pad(which(j == i), 3), str_pad(i, 35, "left"))
    h = read_html(i) %>%
      html_nodes(".search-result-content h3 a") %>%
      html_attr("href")

    if (!length(h)) {

      cat(": no bills found\n")
      next

    } else {

      cat(":", str_pad(length(h), 2), "bill(s)")
      for (k in h) {

        f = gsub("/kamerstukken/(moties/)?detail\\?id=|&(amp;)?did=", "-", k)
        f = paste0("raw/tk/bill-pages/bill", f, ".html")
        if (!file.exists(f)) {
          try(download.file(paste0("http://www.tweedekamer.nl", k), f, mode = "wb", quiet = TRUE),
              silent = TRUE)
        }

        if (file.exists(f) && !file.info(f)$size) {
          file.remove(f)
        }

        if (file.exists(f)) {

          h = read_html(f)

          # motion author
          au = dd(h, "Indiener")

          if (!length(au)) {

            type = "Gewijzigde motie"
            au = dd(h, "Eerste ondertekenaar")
            co = dd(h, "Mede ondertekenaar")

          } else {

            type = "Motie"
            co = dd(h, "Medeindiener")

          }

          co = co[ !co %in% au ]

          if (length(au) != 1) {
            warning(paste("File", f, "has no sponsor information"))
            next
          }

          result = html_nodes(h, ".vote-result .result span") %>% html_text
          result = ifelse(!length(result), NA, result)

          vote = html_nodes(h, ".vote-result .vote-type span") %>% html_text
          vote = ifelse(!length(vote), NA, vote)

          b = rbind(b, data_frame(
            file = gsub("raw/tk/bill-pages/bill-|\\.html", "", f),
            type,
            ref = dd(h, "Nummer", FALSE),
            date = dd(h, "Publicatiedatum", FALSE),
            authors = paste0(gsub("/kamerleden/alle_kamerleden/", "", au), collapse = ";"),
            cosponsors = paste0(gsub("/kamerleden/alle_kamerleden/", "", co), collapse = ";"),
            n_au = length(au),
            n_co = length(co),
            result,
            vote
          ))

        } else {

          warning(paste("failed to download bill", k, "to", f))

        }

      }

    }

    cat("\n")

  }

  b$legislature = "2012-2015" # not collecting older ones (no sponsor links)
  stopifnot(!duplicated(b$ref))

  write.csv(b, "data/bills-tk.csv", row.names = FALSE)

}

b = read.csv("data/bills-tk.csv", stringsAsFactors = FALSE)

# sponsors

dd <- function(h, x) {
  html_node(h, xpath = paste0("//div[@id='passport']/dl/dt[text()='", x,
                              "']/following-sibling::dd[1]"))  %>%
    html_text
}

a = strsplit(b$authors, ";")
a = c(a, strsplit(b$cosponsors, ";"))
a = unique(unlist(a))

s = data_frame()

for (i in rev(a)) {

  cat(str_pad(which(a == i), 3))

  f = paste0("raw/tk/mp-pages/", i, ".html")
  if (!file.exists(f)) {
    download.file(paste0("http://www.tweedekamer.nl/kamerleden/alle_kamerleden/", i), f,
                  mode = "wb", quiet = TRUE)
  }
  h = read_html(f)

  s = rbind(s, data_frame(
    uid = i,
    name = html_node(h, "h1") %>% html_text,
    born = dd(h, "Geboortedatum") %>%
      str_extract("\\d{4}") %>%
      as.integer,
    nyears = dd(h, "Anciënniteit") %>%
      str_extract("\\d+") %>%
      as.integer,
    sex = dd(h, "Geslacht") %>%
      str_trim,
    partyname = dd(h, "Fractie") %>%
      str_trim,
    party = dd(h, "Fractie") %>%
      str_extract("\\(\\w+\\)"),
    url = paste0("http://www.tweedekamer.nl/kamerleden/alle_kamerleden/", i),
    photo = html_node(h, xpath = "//div[@id='passport']/img") %>%
      html_attr("src")
  ))
  cat(":", html_node(h, "title") %>% html_text %>% str_extract("(.*)\\)"), "\n")

  f = paste0("photos-tk/", str_extract(basename(tail(s$photo, 1)), "(.*)jpg"))
  if (!file.exists(f)) {
    download.file(tail(s$photo, 1), f, mode = "wb", quiet = TRUE)
  }
  s$photo[ nrow(s) ] = f

}

s$sex = ifelse(s$sex == "Vrouw", "F", "M")
s$nyears = round(s$nyears / 365)
s$party = gsub("\\(|\\)", "", s$party) %>% toupper
s$partyname = gsub("(.*) \\((.*)", "\\1", s$partyname)

s$party[ s$party == "GRKÖ"] = "GRKO"
# individuals-as-parties, coded as independents (incl. Van Vliet)
s$party[ s$party %in% c("HOUWERS", "KLEIN") | is.na(s$party) ] = "IND"
stopifnot(s$party %in% names(colors))

write.csv(s, "data/sponsors-tk.csv", row.names = FALSE)
