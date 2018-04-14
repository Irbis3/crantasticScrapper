# motions

if (!file.exists("data/bills-ek.csv")) {

  r = "https://www.eerstekamer.nl/moties_3"
  h = read_html(r)

  id1 = html_node(h, xpath = "//input[@name='dlgid'][1]") %>% html_attr("value")
  id2 = html_node(h, xpath = "//input[@name='s01'][1]") %>% html_attr("value")

  id3 = html_nodes(h, "#formm7m option") %>% html_attr("value")
  names(id3) = html_nodes(h, "#formm7m option") %>% html_text %>% substr(1, 4)

  for (i in 2012:2015) {

    cat("Year", i)
    f = paste0("raw/ek/bill-lists/bills-", i, "-0.html")
    if (!file.exists(f)) {

      download.file(paste0(r, "?dlgid=", id1, "&s01=", id2, "&_charset_=UTF-8&m7m=", id3[ as.character(i) ], "&modus=selecteer#p2"), f, mode = "wb", quiet = TRUE)

    }

    h = read_html(f) %>%
      html_nodes(xpath = "//a[text()='verder']") %>%
      html_attr("href")

    while (length(h) > 0) {

      cat(".")

      f = gsub("(.*)start_00j=(\\d+)(.*)", "\\2", h)
      f = paste0("raw/ek/bill-lists/bills-", i, "-", f, ".html")
      if (!file.exists(f)) {
        download.file(paste0("https://www.eerstekamer.nl", h), f, mode = "wb", quiet = TRUE)
      }

      h = read_html(f) %>%
        html_nodes(xpath = "//a[text()='verder']") %>%
        html_attr("href")

    }

    cat("\n")

  }

  j = list.files("raw/ek/bill-lists", full.names = TRUE)
  b = data_frame()

  for (i in rev(j)) {

    cat("Page", str_pad(which(j == i), 3), str_pad(i, 38, "left"))
    h = read_html(i) %>%
      html_nodes("ul.ladder a") %>%
      html_attr("href") %>%
      unique

    if (!length(h)) {

      cat(": no bills found\n")
      next

    } else {

      cat(":", str_pad(length(h), 2), "bill(s)")
      for (k in h) {

        f = gsub("/motie/", "-", k)
        f = paste0("raw/ek/bill-pages/bill", f, ".html")
        if (!file.exists(f)) {
          try(download.file(paste0("http://www.eerstekamer.nl", k), f, mode = "wb", quiet = TRUE),
              silent = TRUE)
        }

        if (file.exists(f) && !file.info(f)$size) {
          file.remove(f)
        }

        if (file.exists(f)) {

          h = read_html(f)

          # motion author
          au = html_nodes(h, xpath = "//div[@class='seriekeuze']//a[contains(@href, '/persoon/')]") %>%
            html_attr("href")

          if (length(au) < 1) {
            warning(paste("File", f, "has no sponsor information"))
            next
          }

          b = rbind(b, data_frame(
            file = gsub("raw/ek/bill-pages/bill-|\\.html", "", f),
            type = "Motie",
            ref = html_node(h, "h1") %>% html_text,
            date = NA, # lazy
            authors = paste0(gsub("/persoon/", "", au[1]), collapse = ";"),
            cosponsors = paste0(gsub("/persoon/", "", au[-1]), collapse = ";"),
            n_au = 1,
            n_co = length(au) - 1,
            result = NA, # lazy
            vote = NA
          ))

        } else {

          warning(paste("failed to download bill", k, "to", f))

        }

      }

    }

    cat("\n")

  }

  b$legislature = "2012-2015" # not collecting older ones (no sponsor links)
  b$type[ grepl("gewijzigde_motie", b$file) ] = "Gewijzigde motie"
  b$ref = gsub("(.*)\\(EK (.*)\\)", "\\2", b$ref)
  stopifnot(!duplicated(b$ref))

  write.csv(b, "data/bills-ek.csv", row.names = FALSE)

}

b = read.csv("data/bills-ek.csv", stringsAsFactors = FALSE)

# sponsors

a = strsplit(b$authors, ";")
a = c(a, strsplit(b$cosponsors, ";"))
a = unique(unlist(a))

s = data_frame()

for (i in rev(a)) {

  cat(str_pad(which(a == i), 3))

  f = paste0("raw/ek/mp-pages/", i, ".html")
  if (!file.exists(f)) {
    download.file(paste0("https://www.eerstekamer.nl/persoon/", i), f,
                  mode = "wb", quiet = TRUE)
  }
  h = read_html(f)

  y = html_nodes(h, xpath = "//div[contains(text(), 'dagen')]") %>%
    html_text %>%
    str_extract("\\d+") %>%
    as.integer

  s = rbind(s, data_frame(
    uid = i,
    name = html_node(h, "h1") %>%
      html_text %>%
      str_replace("(.*)\\((.*)\\)$", "\\1") %>%
      str_trim,
    born = NA, # lazy
    nyears = y[ length(y) ],
    sex = NA, # ?
    partyname = NA, # lazy
    party = html_node(h, "h1") %>%
      html_text %>%
      str_replace("(.*)\\((.*)\\)$", "\\2"),
    url = paste0("https://www.eerstekamer.nl/persoon/", i),
    photo = html_node(h, "img.hoofdbeeld_vlucht") %>%
      html_attr("src") %>%
      str_replace("\\?(.*)", "")
  ))
  cat(":", html_node(h, "title") %>%
        html_text %>%
        str_replace("(.*) -\\s+(.*)", "\\2") %>%
        str_replace_all("\\s+", " "),
      "\n")

  f = paste0("photos-ek/", basename(tail(s$photo, 1)), ".jpg")
  if (!file.exists(f)) {
    download.file(paste0("https://www.eerstekamer.nl", tail(s$photo, 1)), f, mode = "wb", quiet = TRUE)
  }
  s$photo[ nrow(s) ] = f

}

# s$sex = ifelse(s$sex == "Vrouw", "F", "M")
s$nyears = round(s$nyears / 365)
s$party = toupper(s$party)
# s$partyname = gsub("(.*) \\((.*)", "\\1", s$partyname)

s$party[ s$party == "CHRISTENUNIE"] = "CU"
s$party[ s$party == "GROENLINKS"] = "GL"
# De Lange = individual-as-party, coded as independent
# OSF = independent (Onafhankelijke Senaatsfractie)
s$party[ s$party %in% c("FRACTIE-DE LANGE", "OSF") | is.na(s$party) ] = "IND"
stopifnot(s$party %in% names(colors))

write.csv(s, "data/sponsors-ek.csv", row.names = FALSE)
