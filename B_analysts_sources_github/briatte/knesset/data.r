bills = "data/bills.csv"
sponsors = "data/sponsors.csv"

# ==============================================================================
# PARSE BILLS
# ==============================================================================
if (!file.exists(bills)) {

  p = read_html("https://oknesset.org/bill/") %>%
    html_nodes(".pagination a") %>%
    html_text %>%
    as.integer

  b = data_frame()

  cat("Downloading", max(p), "bill pages...\n")
  pb = txtProgressBar(1, max(p), style = 3)
  for (i in 1:max(p)) {

    f = paste0("raw/bill-lists/bills-", i, ".html")
    if (!file.exists(f)) {

      h = GET(paste0("https://oknesset.org/bill/?&page=", i))
      writeLines(content(h, "text", encoding = "UTF-8"), f)

    }

    h = read_html(f)
    b = rbind(b, data_frame(
      url = html_nodes(h, ".item-title a") %>% html_attr("href"),
      title = html_nodes(h, ".item-title a") %>% html_text,
      date = html_nodes(h, ".item-context") %>% html_text,
      status = html_nodes(h, ".item-action") %>% html_text
    ))

    setTxtProgressBar(pb, i)

  }
  cat("\n")

  b$sponsors = NA

  j = sample(b$url)

  cat("Downloading", length(j), "bills...\n")
  pb = txtProgressBar(1, length(j), style = 3)

  for (i in j) {

    f = paste0("raw/bill-pages/bill-", gsub("\\D", "", i), ".html")
    if (!file.exists(f)) {

      h = GET(paste0("https://oknesset.org", i))
      writeLines(content(h, "text", encoding = "UTF-8"), f)

    }

    if (file.info(f)$size == 1) {

      file.remove(f)
      next

    }

    h = read_html(f)

    # date
    d = html_nodes(h, xpath = "//h2[text()='יזום הצעת החוק']/following-sibling::ul/li/a") %>% html_text
    b$date[ gsub("\\D", "", b$url) == gsub("\\D", "", f) ] = paste0(d, collapse = ";")

    # sponsors
    h = html_nodes(h, "div.span12 .party-member-photo a") %>%
      html_attr("href")

    h = h[ grepl("/member/\\d+/", h) ] %>% unique

    b$sponsors[ gsub("\\D", "", b$url) == gsub("\\D", "", f) ] =
      paste0(gsub("/member/(\\d+)/(.*)", "\\1", h), collapse = ";")

    for (k in h) {

      f = paste0("raw/mps/mp-", gsub("/member/(\\d+)/(.*)", "\\1", k), ".html")
      if (!file.exists(f)) {

        h = GET(paste0("https://oknesset.org", k))
        writeLines(content(h, "text", encoding = "UTF-8"), f)

      }

      if (file.info(f)$size == 1) {

        file.remove(f)
        next

      }

    }

    setTxtProgressBar(pb, which(j == i))

  }
  cat("\n")

  write.csv(b, bills, row.names = FALSE)

}

b = read.csv(bills, stringsAsFactors = FALSE)

b$n_au = 1 + str_count(b$sponsors, ";")
b$n_au[ b$sponsors == "" ] = 0

table(b$n_au, exclude = NULL)
table(cut(b$n_au, c(0, 1, 2, Inf), right = FALSE), exclude = NULL)

b$legislature = str_extract(b$date, "\\d{4}")
b$legislature[ b$legislature %in% 2009:2012 ] = 18
b$legislature[ b$legislature %in% 2013:2014 ] = 19
b$legislature[ b$legislature %in% 2015 ] = 20
table(b$legislature, exclude = NULL)

# ==============================================================================
# PARSE SPONSORS
# ==============================================================================
if (!file.exists(sponsors)) {

  s = data_frame()

  for (f in list.files("raw/mps", pattern = "mp-\\d+.html", full.names = TRUE)) {

    # ignore empty page
    if (f == "raw/mps/mp-714.html")
      next

    h = read_html(f)
    u = html_nodes(h, xpath = "//div[@class='social-links']//a[contains(@href, 'knesset.gov.il')]") %>%
      html_attr("href")
    w = html_nodes(h, xpath = "//div[@class='social-links']//a[contains(@href, 'wikipedia')]") %>%
      html_attr("href")

    # download English profile (for English name and birth year)
    f = gsub("\\.html", "-en.html", f)

    if (!file.exists(f))
      download.file(paste0("http://www.knesset.gov.il/mk/eng/mk_eng.asp?mk_individual_id_t=",
                           gsub("\\D", "", f)), f, mode = "wb", quiet = TRUE)

    l = readLines(f, encoding = "UTF-8")

    # English name
    n = str_extract(l, "<TITLE>(.*)</TITLE>")

    # year of birth
    y = str_extract(l, "Birth:</td><td class=EngDataText>(\\d{2}/\\d{2}/)?\\d{4}") %>%
      na.omit

    # seniority
    l = str_extract(l, "eng_hist\\d+_s.htm") %>% na.omit

    s = rbind(s, data_frame(
      uid = gsub("\\D", "", f),
      name_he = html_nodes(h, "h1") %>% html_text,
      name_en = gsub("</?TITLE>|Knesset Member, ", "", na.omit(n)) %>% str_trim,
      sex = "M", # handled later on
      born = ifelse(!length(y), NA, str_extract(y, "\\d{4}")),
      nyears = paste0(gsub("\\D", "", l) %>% unique %>% sort, collapse = ";"),
      url = ifelse(!length(u), NA, u),
      url_wp = ifelse(!length(w), NA, w),
      photo = html_nodes(h, ".span2 .spacer img") %>% html_attr("src")
    ))

  }

  # parties in each legislature
  p = data_frame()

  for (i in 20:18) {

    h = read_html(paste0("https://oknesset.org/parties-members/", i))

    # party blocs
    f = html_nodes(h, xpath = "//h4/..") %>%
      lapply(function(h) {
        data_frame(
          uid = html_nodes(h, ".party-member-info-text h3 a") %>% html_attr("href"),
          party_url = html_node(h, "h4 a") %>% html_attr("href")
        )
      })

    p = rbind(p, cbind(legislature = i, bind_rows(f)))

    # gender
    h = read_html(paste0("http://www.knesset.gov.il/mk/eng/MKIndexByKnesset_eng.asp?knesset=", i, "&view=2")) %>%
      html_nodes(xpath = "//a[contains(@href, 'id_t')]") %>% html_attr("href")

    s$sex[ s$uid %in% gsub("\\D", "", h) ] = "F"

  }

  p$uid = gsub("/member/(\\d+)/(.*)", "\\1", p$uid)

  s = full_join(p, s, by = "uid") %>%
    unique

  s$party_url = gsub("party|/", "", s$party_url) %>% as.integer
  s$party = NA
  s$party[ s$party_url %in% c(9, 24) ] = "BALAD"   # Balad, orange
  s$party[ s$party_url %in% c(8, 22) ] = "HADASH"  # Hadash, red/green
  s$party[ s$party_url %in% c(21) ] = "HATNUAH"    # Hatnuah, dark blue, light blue
  s$party[ s$party_url %in% c(12, 17, 30) ] = "JH" # Jewish Home, dark blue/green, teal
  s$party[ s$party_url %in% c(6, 25) ] = "KADIMA"  # Kadima, dark blue, red
  s$party[ s$party_url %in% c(32) ] = "KULANU"     # Kulanu, light blue, black
  s$party[ s$party_url %in% c(3, 16) ] = "LAB"     # Labour Party, dark blue/red
  s$party[ s$party_url %in% c(11, 20, 28) ] = "MERETZ" # Merezt, green
  s$party[ s$party_url %in% c(1) ] = "NU"              # National Union, dark blue/orange
  s$party[ s$party_url %in% c(7, 18, 36) ] = "SHAS"    # Shas Party, bright green or blue
  s$party[ s$party_url %in% c(10, 23) ] = "UAL"        # UAL-ADP, yellow
  s$party[ s$party_url %in% c(4, 19, 34) ] = "UTJ"     # United Torah Judaism, dark blue
  s$party[ s$party_url %in% c(15, 29) ] = "YA"         # Yesh Atid, dark blue/light blue
  s$party[ s$party_url %in% c(5, 26, 31) ] = "YB"      # Yisrael Beiteinu, teal
  s$party[ s$party_url %in% c(2, 14, 27) ] = "LIKUD"   # Likud, blue
  s$party[ s$party_url %in% c(33) ] = "ZU"             # Zionist Union, dark blue (2014)
  s$party[ s$party_url %in% c(35) ] = "JL"             # Joint List, black (HADASH, UAL, BALAD, Ta'al)
  s$party[ s$party_url %in% c(13) ] = "INDEP"          # Independence (party)
  table(s$party, exclude = NULL)

  # solve duplicates due to transitions, using MP pages to find correct affil.
  s = s[ -which(s$uid == 849 & s$party_url == 3), ] # LAB -> INDEP
  s = s[ -which(s$uid == 717 & s$party_url == 3), ] # LAB -> INDEP
  s = s[ -which(s$uid == 78 & s$party_url == 14), ]  # LIKUD -> YB
  s = s[ -which(s$uid == 790 & s$party_url == 14), ] # LIKUD -> YB
  s = s[ -which(s$uid == 837 & s$party_url == 14), ] # LIKUD -> YB
  s = s[ -which(s$uid == 805 & s$party_url == 14), ] # LIKUD -> YB
  s = s[ -which(s$uid == 859 & s$party_url == 14), ] # LIKUD -> YB
  s = s[ -which(s$uid == 793 & s$party_url == 14), ] # LIKUD -> YB
  s = s[ -which(s$uid == 835 & s$party_url == 14), ] # LIKUD -> YB
  s = s[ -which(s$uid == 832 & s$party_url == 14), ] # LIKUD -> YB
  s = s[ -which(s$uid == 214 & s$party_url == 14), ] # LIKUD -> YB

  # get seniority from full MP listings (l. 10 is the oldest of all sponsors)
  # table(strsplit(s$nyears, ";") %>% unlist %>% as.integer %>% min)
  for (i in 1:nrow(s)) {

    y = strsplit(s$nyears[ i ], ";") %>% unlist %>% as.integer
    y = y[ y < s$legislature[ i ] ]
    y[ y %in% c(10, 14, 16, 17) ] = 3
    y[ y %in% c(11:13, 15, 18) ] = 4
    y[ y == 19 ] = 2
    y[ y == 20 ] = 1
    s$nyears[ i ] = sum(y)

  }

  write.csv(s, sponsors, row.names = FALSE)

}

s = read.csv(sponsors, stringsAsFactors = FALSE) %>%
  filter(!is.na(name_en))

# remove apostrophes from names
s$name_en = gsub("`", "", s$name_en)

# missing years of birth (WP-EN)
s$born[ s$name_en == "Yinon Magal" ] = 1947
s$born[ s$name_en == "Manuel Trajtenberg" ] = 1950
s$born[ s$name_en == "Eitan Broshi" ] = 1950
s$born[ s$name_en == "Ayelet Nahmias-Verbin" ] = 1970
s$born[ s$name_en == "Osama Saadi" ] = 1963
s$born[ s$name_en == "Rachel Adatto" ] = 1947
s$born = as.integer(s$born)

for (i in s$photo %>% na.omit %>% unique) {

  f = paste0("photos/", basename(i))
  if (!file.exists(f))
    download.file(i, f, mode = "wb", quiet = TRUE)

  if (file.exists(f))
    s$photo[ s$photo == i ] = f

}

# switch to Open Knesset URLs (none missing)
s$url = paste0("https://oknesset.org/member/", s$uid)

# subset sponsors with no mandate information
s = subset(s, !is.na(legislature))

# ==============================================================================
# QUALITY CONTROL
# ==============================================================================

# - might be missing: born (int of length 4), constituency (chr),
#   photo (chr, folder/file.ext)
# - never missing: sex (chr, F/M), nyears (int), url (chr, URL),
#   party (chr, mapped to colors)

cat("Missing", sum(is.na(s$born)), "years of birth\n")
stopifnot(is.integer(s$born) & nchar(s$born) == 4 | is.na(s$born))

# cat("Missing", sum(is.na(s$constituency)), "constituencies\n")
# stopifnot(is.character(s$constituency))

cat("Missing", sum(is.na(s$photo)), "photos\n")
stopifnot(is.character(s$photo) & grepl("^photos(_\\w{2})?/(.*)\\.\\w{3}", s$photo) | is.na(s$photo))

stopifnot(!is.na(s$sex) & s$sex %in% c("F", "M"))
stopifnot(!is.na(s$nyears) & is.integer(s$nyears))
stopifnot(!is.na(s$url) & grepl("^http(s)?://(.*)", s$url))
stopifnot(s$party %in% names(colors))
