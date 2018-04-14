# note: all data are scraped from sponsor pages
#
# 1. first, scrape the sponsors, using the complete listing of seat changes;
#    the listing covers legislatures 1-6 and contains all MPs, unlike the A-Z
#    listings, which are not exhaustive (MPs/legislature vary too much)
#
# 2. then, scrape the sponsors' bill initiation pages, which contain the unique
#    bill IDs; this will get every private bill in legislatures 2-6, and the
#    bills need to be downloaded to get the order in which they were signed

root = "http://www.nrsr.sk/web/"
sponsors_index = "data/sponsors-index.csv"
sponsors = "data/sponsors.csv"
bills = "data/bills.csv"

if (!file.exists(sponsors) | !file.exists(bills)) {

  # download sponsor indexes

  if (!file.exists(sponsors_index)) {

    u = paste0(root, "Default.aspx?sid=poslanci%2fzmeny")
    p = GET(u)

    ev = read_html(p) %>%
      html_node(xpath = "//input[@name='__EVENTVALIDATION']") %>%
      html_attr("value")

    vs = read_html(p) %>%
      html_node(xpath = "//input[@name='__VIEWSTATE']") %>%
      html_attr("value")

    cat("Scraping using master sponsors index:",
        "EV", substr(ev, 1, 10), "VS", substr(vs, 1, 10), "\n")

    # loop over all legislatures and pages

    for (i in 1:6) {

      j = 0
      l = 1

      while (j < l) {

        j = j + 1
        cat("Scraping sponsor index for legislature", i,
            "page", sprintf("%2.0f", j))

        # for some weird reason, the only way to download page 1's (without which
        # legislatures 5-6 refuse to download completely) is to lie about the event
        # target

        pinfo = list("__EVENTTARGET" =
                       ifelse(j == 1, "_sectionLayoutContainer$ctl01$dgResult2",
                              "_sectionLayoutContainer$ctl01$_ResultGrid2"),
                     "__EVENTARGUMENT" = paste0("Page$", j),
                     "_sectionLayoutContainer$ctl01$_currentTerm" = i,
                     "__VIEWSTATE" = vs,
                     "__EVENTVALIDATION" = ev,
                     "__VIEWSTATEGENERATOR" = "DB1B4C9A",
                     "__LASTFOCUS" = "",
                     "__SCROLLPOSITIONX" = 0,
                     "__SCROLLPOSITIONY" = 0)

        d = POST(u, body = pinfo, cookies = p$cookies)

        if (read_html(d) %>% html_node("title") %>% html_text == "www.nrsr.sk") {

          cat(": failed\n")
          next

        }

        writeLines(content(d, "text"), paste0("raw/sponsors-", i, "-page-",
                                              sprintf("%02.0f", j), ".html"))

        ev = read_html(d) %>%
          html_node(xpath = "//input[@name='__EVENTVALIDATION']") %>%
          html_attr("value")

        vs = read_html(d) %>%
          html_node(xpath = "//input[@name='__VIEWSTATE']") %>%
          html_attr("value")

        cat(":", "EV", substr(ev, 1, 10), "VS", substr(vs, 1, 10), "\n")

        l = read_html(d) %>%
          html_nodes(xpath = "//a[starts-with(@href, 'java')]") %>%
          html_attr("href")

        l = gsub("(.*)Page\\$(\\d+|Last|First)(.*)", "\\2", l) %>% unique
        l = l[ grepl("\\d", l) ] %>% as.integer %>% max

      }

    }

    # parse sponsors indexes

    p = list.files("raw", pattern = "^sponsors-", full.names = TRUE)
    cat("Parsing", length(p), "sponsor indexes...\n")

    i = txtProgressBar(0, length(p), style = 3)
    s = data_frame()

    for (j in p) {

      setTxtProgressBar(i, which(p == j))

      t = read_html(j) %>%
        html_nodes("#_sectionLayoutContainer_ctl01__ResultGrid2 tr")

      t = t[ html_attr(t, "class") %>%
               sapply(str_detect, "^tab_(.*)alt$") %>%
               which ]

      u = html_node(t, "a") %>%
        html_attr("href") %>%
        gsub("Default.aspx\\?sid=poslanci/poslanec&Poslanec", "", .)

      s = rbind(s, data_frame(
        legislature = gsub("(.*)&CisObdobia=(\\d+)", "\\2", u),
        id = gsub("ID=(\\d+)(.*)", "\\1", u) ,
        name = html_node(t, xpath = "td[2]") %>% html_text %>% str_clean,
        status = html_node(t, xpath = "td[3]") %>% html_text,
        reason = html_node(t, xpath = "td[4]") %>% html_text
      ))

    }

    # extract list of unique sponsor ids and names and compute seniority since l.1

    s = select(s, legislature, id, name) %>%
      unique %>%
      arrange(legislature, id) %>%
      mutate(name = gsub("(.*)\\s\\((.*)", "\\1", name)) %>%
      group_by(name) %>%
      mutate(nyears = 4 * (1:n() - 1))

    # all sponsors uniquely identified by their full names within each legislature
    stopifnot(nrow(s) == paste(s$name, s$legislature, s$id) %>% n_distinct)

    write.csv(arrange(s, legislature, id), sponsors_index, row.names = FALSE)

  }

  s = read.csv(sponsors_index, stringsAsFactors = FALSE)

  # download sponsor pages (rerun to fix network errors)
  # bill initiation pages are all empty for l. 1: skip

  p = with(s[ s$legislature > 1, ],
           paste0(root, "Default.aspx?sid=poslanci/poslanec&PoslanecID=", id,
                  "&CisObdobia=", legislature))

  cat("Finding information on", length(p), "sponsors...\n")

  i = txtProgressBar(0, length(p), style = 3)

  for (j in p) {

    setTxtProgressBar(i, which(p == j))

    # sponsor profile

    f = gsub("(.*)PoslanecID=(\\d+)&CisObdobia=(\\d)", "raw/mp-\\2-\\3.html", j)

    if (!file.exists(f))
      try(download.file(paste0(root, j), f, mode = "wb", quiet = TRUE),
          silent = TRUE)

    if (!file.info(f)$size) {

      cat("Failed to download:", f, "\n")
      file.remove(f)

    }

    # bill initiations

    u = gsub("(.*)PoslanecID=(\\d+)&CisObdobia=(\\d)",
             paste0("Default.aspx?sid=zakony/sslp&PredkladatelID=0",
                    "&PredkladatelPoslanecId=\\2&CisObdobia=\\3"), j)

    f = gsub("(.*)PoslanecID=(\\d+)&CisObdobia=(\\d)", "raw/activity-\\2-\\3.html", j)

    if (!file.exists(f))
      try(download.file(paste0(root, u), f, mode = "wb", quiet = TRUE),
          silent = TRUE)

    if (!file.info(f)$size) {

      cat("Failed to download:", f, "\n")
      file.remove(f)

    }

  }

  cat("\n")

  # parse sponsor details

  p = list.files("raw", pattern = "^mp-", full.names = TRUE)
  cat("\nParsing information on", length(p), "sponsors...\n")

  i = txtProgressBar(0, length(p), style = 3)

  w = data_frame()

  for (j in p) {

    setTxtProgressBar(i, which(p == j))

    t = read_html(j) %>% html_nodes(".mp_personal_data span") %>% html_text
    w = rbind(w, data_frame(
      legislature = gsub("raw/mp-\\d+-(\\d)\\.html", "\\1", j) %>% as.integer,
      id = gsub("raw/mp-(\\d+)(.*)", "\\1", j) %>% as.integer,
      title = t[2] %>% str_trim, first = t[1], last = t[3],
      party = gsub("\\s", "", t[4]), born = str_extract(t[5], "\\d{4}"),
      natl = t[6] %>% str_trim,
      place = t[7] %>% str_trim, county = t[8] %>% str_trim,
      photo = read_html(j) %>% html_node(".mp_foto img") %>% html_attr("src")
    ))

  }

  s = inner_join(s, w, by = c("legislature", "id"))

  # impute sex
  s$sex = ifelse(grepl("a$", s$last), "F", "M")
  s$sex[ s$name %in% c("Bugár, Béla", "Bárdos, Gyula") ] = "M"

  # parse sponsors bill initiation records

  p = paste0("raw/activity-", s$id, "-", s$legislature, ".html")

  # sanity check: all bill initiation records exist
  stopifnot(file.exists(p))

  cat("Parsing bill initiations by", length(p), "sponsors...\n")

  i = txtProgressBar(0, length(p), style = 3)
  a = data_frame()

  for (j in p) {

    setTxtProgressBar(i, which(p == j))

    t = read_html(j) %>%
      html_nodes(xpath = "//table[@id='_sectionLayoutContainer_ctl01_dgResult']/tbody/tr")

    if (length(t) > 0)
      a = rbind(a, data_frame(
        legislature = j,
        id = sapply(t, html_nodes, xpath = "td[1]/a") %>%
          sapply(html_attr, "href"),
        cpt = sapply(t, html_nodes, xpath = "td[2]/a") %>%
          sapply(html_attr, "href"),
        date = sapply(t, html_nodes, xpath = "td[4]") %>%
          sapply(html_text),
        title = sapply(t, html_nodes, xpath = "td[1]") %>%
          sapply(html_text) %>% str_clean,
        status = sapply(t, html_nodes, xpath = "td[3]") %>%
          sapply(html_text),
        authors = sapply(t, html_nodes, xpath = "td[5]") %>%
          sapply(html_text)
      ))

  }

  cat("\n")

  # get legislature id
  a$legislature = gsub("(.*)-(\\d)\\.html", "\\2", a$legislature) %>% as.integer

  # shorten master id
  a$id = gsub("(.*)&MasterID=(\\d+)", "\\2", a$id) %>% as.integer

  # shorten cpt id
  a$cpt = gsub("(.*)&ID=(\\d+)", "\\2", a$cpt) %>% as.integer

  # clean dates
  a$date = a$date = strptime(a$date, "%d. %m. %Y") %>% as.Date

  # remove duplicates and one bill with no sponsors
  a = unique(a) %>%
    filter(nchar(authors) > 5) %>%
    arrange(legislature, id)

  # sanity check: all master IDs are unique
  stopifnot(n_distinct(a$id) == nrow(a))

  # number of sponsors per bill
  a$n_au = 1 + str_count(a$authors, ", ")

  # convert sponsor names to ASCII (keeping diacritics)
  s$uid = paste0(substr(s$first, 1, 1), ". ", s$last) %>%
    iconv(to = "ASCII//TRANSLIT")

  # one duplicate: J. Mikus, l. 3
  group_by(s, legislature, uid) %>%
    mutate(n = n()) %>% filter(n > 1)

  s$uid[ s$name == "Mikuš, Ján" ] = "J. Mikus1"
  s$uid[ s$name == "Mikuš, Jozef" ] = "J. Mikus2"

  # convert bill sponsorships to ASCII (keeping diacritics)
  a$authors = iconv(a$authors, to = "ASCII//TRANSLIT")

  # identify Jozef Mikus in sponsors (14 instances)
  r = grepl("J. Mikus", a$authors) &
    grepl("Jozefa Miku", a$title, ignore.case = TRUE)

  a$authors[ r ] = gsub("J\\. Mikus", "J. Mikus2", a$authors[ r ])

  # identify Jan Mikus in sponsors (0 instance)
  r = grepl("J. Mikus", a$authors) &
    grepl("Jan Miku", a$title, ignore.case = TRUE)

  a$authors[ r ] = gsub("J\\. Mikus", "J. Mikus1", a$authors[ r ])

  # remove ambiguous 'J. Mikus' in sponsors (5 instances)
  r = grepl("J. Mikus", a$authors) &
    !grepl("Mikus(1|2)", a$authors, ignore.case = TRUE)

  a$authors[ r ] = gsub("J\\. Mikus, ", "", a$authors[ r ])

  # bugfixes: two bills with misspelt sponsors
  a$authors = gsub("(.*), J. Ivanc$", "\\1, J. Ivanco", a$authors)
  a$authors = gsub("(.*), S. Kahan$", "\\1, S. Kahanec", a$authors)

  # sanity check: all sponsors detected
  for (j in unique(a$legislature)) {

    stopifnot(unlist(strsplit(a$authors[ a$legislature == j ], ", ")) %in%
                s$uid[ s$legislature == j ])

  }

  # party recodings
  s$party = gsub("–", "-", s$party)
  s$party[ s$party %in% c("SDK", "SDKÚ", "SDKÚ-DS") ] = "SDKU-DS"
  s$party[ s$party == "SMK" ] = "SMK-MKP"
  s$party[ s$party == "ĽS-HZDS" ] = "HZDS"
  s$party[ s$party == "Smer" ] = "SMER-SD"
  s$party[ s$party == "OĽaNO" ] = "OLANO"
  s$party[ s$party == "SDĽ" ] = "SDL"
  s$party[ s$party == "SaS" ] = "SAS"
  s$party[ s$party == "MOST-HÍD" ] = "MOST-HID"

  stopifnot(s$party %in% names(colors))

  # fix names
  s$name = gsub("(.*), (.*)", "\\2 \\1", s$name)

  # export sponsors
  write.csv(s, sponsors, row.names = FALSE)

  # export bills
  write.csv(a, bills, row.names = FALSE)

}

s = read.csv(sponsors, stringsAsFactors = FALSE)
a = read.csv(bills, stringsAsFactors = FALSE)

# download sponsor photos (rerun to fix network errors)

p = s$photo %>% unique

cat("Downloading photos for", length(p), "sponsors...\n")

i = txtProgressBar(0, length(p), style = 3)

for (j in p) {

  setTxtProgressBar(i, which(p == j))

  f = gsub("http://www.nrsr.sk/web/dynamic/PoslanecPhoto.aspx\\?PoslanecID=",
           "photos/", j) %>%
    gsub("&ImageWidth=140", ".jpg", .)

  if (!file.exists(f))
    try(download.file(j, f, mode = "wb", quiet = TRUE),
        silent = TRUE)

  if (!file.info(f)$size) {

    cat("Failed to download:", f, "\n")
    file.remove(f)

  }

}

cat("\n")

s$photo = gsub("http://www.nrsr.sk/web/dynamic/PoslanecPhoto.aspx\\?PoslanecID=|&ImageWidth=140",
               "photos/", gsub("&ImageWidth=140", ".jpg", s$photo))

s$county = gsub("\\s", "_", s$county)

s$url = paste0(root, "Default.aspx?sid=poslanci/poslanec&PoslanecID=", s$id, "&CisObdobia=", s$legislature)

# ==============================================================================
# CHECK CONSTITUENCIES
# ==============================================================================

# county recodings (Wikipedia Slovenčina handles), from 'kraj' to 'sídlo'
s$county = gsub("_kraj$", "", s$county)
s$county[ s$county == "Banskobystrický" ] = "Banská_Bystrica"
s$county[ s$county == "Bratislavský" ] = "Bratislava"
s$county[ s$county == "Košický" ] = "Košice"
s$county[ s$county == "Trnavský" ] = "Trnava"
s$county[ s$county %in% c("Nitriansky", "Nitra") ] = "Nitra"
s$county[ s$county == "Prešovský" ] = "Prešov"
s$county[ s$county %in% c("Trenčiansky", "Trenčín") ] = "Trenčín"
s$county[ s$county == "Žilinský" ] = "Žilina"
s$county[ s$county == "" ] = NA
table(s$county)

cat("Checking constituencies,", sum(is.na(s$county)), "missing...\n")
for (i in na.omit(unique(s$county))) {

  g = GET(paste0("https://", "sk", ".wikipedia.org/wiki/", i)) # meta[ "lang"]

  if (status_code(g) != 200)
    cat("Missing Wikipedia entry:", i, "\n")

  g = xpathSApply(htmlParse(g), "//title", xmlValue)
  g = gsub("(.*) - Wikipédia(.*)", "\\1", g)

  if (gsub("\\s", "_", g) != i)
    cat("Discrepancy:", g, "(WP) !=", i ,"(data)\n")

}

# ============================================================================
# QUALITY CONTROL
# ============================================================================

# - might be missing: born (int of length 4), constituency (chr),
#   photo (chr, folder/file.ext)
# - never missing: sex (chr, F/M), nyears (int), url (chr, URL),
#   party (chr, mapped to colors)

cat("Missing", sum(is.na(s$born)), "years of birth\n")
stopifnot(is.integer(s$born) & nchar(s$born) == 4 | is.na(s$born))

cat("Missing", sum(is.na(s$county)), "constituencies\n")
stopifnot(is.character(s$county))

cat("Missing", sum(is.na(s$photo)), "photos\n")
stopifnot(is.character(s$photo) & grepl("^photos(_\\w{2})?/(.*)\\.\\w{3}", s$photo) | is.na(s$photo))

stopifnot(!is.na(s$sex) & s$sex %in% c("F", "M"))
stopifnot(!is.na(s$nyears) & is.integer(s$nyears))
stopifnot(!is.na(s$url) & grepl("^http(s)?://(.*)", s$url))
stopifnot(s$party %in% names(colors))
