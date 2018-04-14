bills = "data/bills-se.csv"
sponsors = "data/sponsors-se.csv"

if (!file.exists(bills) | !file.exists(sponsors)) {

  k = data_frame()

  # step (1) scrape senators list in every constituency (n = 81)
  # http://senat.cz/senat/volby/hledani/obvody.php?ke_dni=28.10.2015&O=10

  for (i in 1:81) {

    f = paste0("raw/se/constituencies/kod-", i, ".html")
    if (!file.exists(f)) {
      download.file(paste0("http://senat.cz/senat/volby/hledani/o_obvodu.php?kod=", i),
                    f, mode = "wb", quiet = TRUE)
    }
    f = read_html(f)
    k = rbind(k, data_frame(
      kod = i,
      name = html_nodes(f, "h3")[[1]] %>% html_text,
      sen = html_nodes(f, ".tHistory td a") %>% html_text,
      url = html_nodes(f, ".tHistory td a") %>% html_attr("href")
    ))

  }

  k$name = gsub("Sídlo:\\s", "", k$name)

  # step (2) get senator profile pages and list of sponsored bills

  s = data_frame()
  b = data_frame()

  j = unique(k$url) %>% sort
  for (i in rev(j)) {

    f = gsub("(.*)dni=(.*)&lng=cz&par_3=(\\d+)", "raw/se/senator-pages/sen-\\3-\\2.html", i)
    if (!file.exists(f)) {
      download.file(paste0("http://senat.cz", i), f, mode = "wb", quiet = TRUE)
    }
    f = read_html(f)

    name = html_node(f, "title") %>% html_text
    name = gsub("^\\s|\\s?: Senát PČR", "", name)

    info = html_nodes(f, ".basicInfo dd") %>% html_text

    sex = html_nodes(f, ".membershipModule dt") %>%
      html_text %>%
      paste0(collapse = ";")

    photo = html_node(f, ".introductionModule img") %>% html_attr("src")

    cat(sprintf("%3.0f", which(j == i)), name)

    f = gsub("(.*)par_3=(\\d+)", "raw/se/senator-pages/sen-\\2-bills.html", i)
    if (!file.exists(f)) {
      download.file(paste0("http://senat.cz/xqw/xervlet/pssenat/finddoc?navrh=",
                           gsub("(.*)par_3=(\\d+)", "\\2", i)),
                    f, mode = "wb", quiet = TRUE)
    }
    f = read_html(f, encoding = "windows-1250")

    bill = html_nodes(f, xpath = "//div[@class='container']/table//a[contains(@href, 'detail&value=')]") %>%
      html_attr("href")
    bill = gsub("\\D", "", bill)

    b = rbind(b, data_frame(
      session = html_nodes(f, xpath = "//div[@class='container']/table//id_obdobi") %>%
        html_text,
      bill,
      date = html_nodes(f, xpath = "//div[@class='container']/table//datum") %>%
        html_text,
      title = html_nodes(f, xpath = "//div[@class='container']/table//a[contains(@href, 'detail&value=')]") %>%
        html_text
    ))

    s = rbind(s, data_frame(
      uid = gsub("(.*)par_3=(\\d+)", "\\2", i),
      url = i,
      name,
      sex = gsub("\\s", ";", sex),
      constituency = info[1],
      party = gsub("\\sv\\sroce\\s\\d{4}", "", info[2]),
      mandate = gsub("\\s", "", info[3]),
      #bills = ifelse(!length(bill), NA, paste0(bill, collapse = ";"))
      photo
    ))

    cat(":", length(bill), "bill(s)\n")

  }

  # download photos
  for (i in unique(s$photo)) {
    photo = gsub("/images/senatori", "photos_se", i)
    if (!file.exists(photo) | !file.info(photo)$size) {
      try(download.file(paste0("http://senat.cz/", i),
                        photo, mode = "wb", quiet = TRUE), silent = TRUE)
    }
    if (!file.info(photo)$size) {
      file.remove(photo) # will warn if missing
      s$photo[ s$photo == i ] = NA
    } else {
      s$photo[ s$photo == i ] = photo
    }
  }

  # step (3) get all bills and extract sponsors list

  b = unique(b)
  stopifnot(!duplicated(b$bill))
  b$session = as.integer(b$session)

  a = data_frame()

  for (i in b$bill) {

    f = paste0("raw/se/bill-pages/bill-", i, ".html")
    if (!file.exists(f)) {
      download.file(paste0("http://senat.cz/xqw/xervlet/pssenat/historie?action=detail&value=", i),
                    f, mode = "wb", quiet = TRUE)
    }
    f = read_html(f, encoding = "windows-1250")

    a = rbind(a, data_frame(
      bill = i,
      aid = html_nodes(f, ".tableModule p")[[1]] %>%
        html_text %>%
        gsub("Navrhovatel(i|em)\\sj(e|sou)\\s|senátor(ka)?\\s|zástupce\\s((komise|výboru)\\s)?Senátu\\s|\\.$",
             "", .) %>%
        strsplit(., ",\\s") %>%
        unlist
    ))

  }

  # clean names (bills)
  a = unique(a)
  a$aid = iconv(a$aid, to = "ASCII//TRANSLIT")
  a$aid = gsub("sen'atori\\s", "", a$aid)

  # clean names (sponsors)
  s$aid = gsub("(arch|Bc|Doc|Ing|JUDr|Mgr|MUDr|MVDr|PaedDr|PhDr|Prof|RNDr|RSDr)\\.\\s?", "", s$name, ignore.case = TRUE)
  s$aid = gsub("senátori\\s|,\\s+(CSc|DrSc|Ph)\\.|\\sFCMA|,\\s(MBA|MPA)|D\\.$", "", s$aid, ignore.case = TRUE)
  s$aid = iconv(s$aid, to = "ASCII//TRANSLIT") %>%
    str_trim
  stopifnot(unique(a$author) %in% unique(s$aid)) # all found

  # step (4) merge bill details to sponsors list

  b = left_join(a, select(s, uid, aid) %>% unique, by = "aid") %>%
    group_by(bill) %>%
    summarise(authors = paste0(uid, collapse = ";")) %>%
    full_join(b, ., by = "bill") %>%
    arrange(session, bill)

  b$n_au = 1 + str_count(b$authors, ";")
  b$n_au[ is.na(b$authors) | b$authors == "" ] = 0 # just to be safe

  # legislature numbers to match lower chamber (see end of script)
  b$legislature = NA
  b$legislature[ b$session == 1 ] = 2
  b$legislature[ b$session %in% 2:3 ] = 3
  b$legislature[ b$session %in% 4:5 ] = 4
  b$legislature[ b$session %in% 6:7 ] = 5
  b$legislature[ b$session == 8 ] = 6
  b$legislature[ b$session %in% 9:10 ] = 7

  # finalize sponsor details before expanding

  s = select(s, -aid)

  s$sex[ grepl("člen(;|$)", s$sex) ] = "M"
  s$sex[ grepl("členka(;|$)", s$sex) ] = "F"
  s$sex[ !s$sex %in% c("F", "M") ] = "M" # Milan Šimonovský, Josef Novotný

  # ===========================================================================
  # CHECK CONSTITUENCIES
  # ===========================================================================

  # note: simplifying the constituencies to work with Wikipedia handles creates
  # a slight distorsion in the data by fusing 2 similarly-named constituencies:
  # Ostrava-město and Plzeň-město; these handles do not work on WP-CS

  s$constituency = gsub("\\D", "", s$constituency) %>% as.integer
  s$constituency = (select(k, kod, name) %>% unique)$name[ s$constituency ]
  s$constituency = gsub("\\s\\d+", "", s$constituency) # Praha as a single constituency
  s$constituency = gsub("\\s", " ", s$constituency)
  s$constituency[ s$constituency == "Brno-město" ] = "Město Brno"

  for (i in unique(s$constituency)) {

    g = GET(paste0("https://cs.wikipedia.org/wiki/", i))

    if (status_code(g) != 200)
      cat("Missing Wikipedia entry:", i, "\n")

    g = xpathSApply(htmlParse(g), "//title", xmlValue)
    g = gsub("(.*) – Wikipedie(.*)", "\\1", g)

    if (gsub("\\s", "_", g) != i & g != i)
      cat("Discrepancy:", g, "(WP) !=", i ,"(data)\n")

  }

  # step (5) convert sponsor details to session-specific rows

  a = data_frame()
  for (i in 1:nrow(s)) {
    y = str_extract_all(s$mandate[i], "\\d{4}") %>% unlist %>% as.integer
    a = rbind(a, data.frame(s[i, ], year = seq(y[1] + 1, y[2]), stringsAsFactors = FALSE))
  }

  # assign lower house legislature number (senate session indicated in comments)
  a$legislature = NA
  a$legislature[ a$year %in% 1997:1998 ] = 2 # 1
  a$legislature[ a$year %in% 1999:2000 ] = 3 # 2
  a$legislature[ a$year %in% 2001:2002 ] = 3 # 3
  a$legislature[ a$year %in% 2003:2004 ] = 4 # 4
  a$legislature[ a$year %in% 2005:2006 ] = 4 # 5
  a$legislature[ a$year %in% 2007:2008 ] = 5 # 6
  a$legislature[ a$year %in% 2009:2010 ] = 5 # 7
  a$legislature[ a$year %in% 2011:2012 ] = 6 # 8
  a$legislature[ a$year %in% 2013:2014 ] = 7 # 9
  a$legislature[ a$year %in% 2015:2016 ] = 7 # 10

  a = arrange(a, uid, year) %>%
    group_by(uid) %>%
    mutate(nyears = 1:n() - 1) %>% # will indicate 0 on first year in office
    group_by(uid, legislature) %>%
    mutate(nyears = min(nyears)) # accurate enough to identify freshmen

  a = filter(a, !is.na(legislature)) %>% # lose 2017:2018
    select(-year) %>% # lose unique years
    unique # keep one row per session

  # the code above results in just one missing sponsor (uid 272) in s. 9 (l. 7)
  # legislatures are numbered as follows (http://senat.cz/datum/datum.php):

  # 1.  funkční období 18.12.1996 - 15.12.1998 (97-98) -- lower house: 2
  # 2.  funkční období 16.12.1998 - 18.12.2000 (99-00) -- lower house: 3
  # 3.  funkční období 19.12.2000 - 03.12.2002 (01-02) -- lower house: . (id)
  # 4.  funkční období 04.12.2002 - 14.12.2004 (etc.)  -- lower house: 4
  # 5.  funkční období 15.12.2004 - 28.11.2006         -- lower house: .
  # 6.  funkční období 29.11.2006 - 25.11.2008         -- lower house: 5
  # 7.  funkční období 26.11.2008 - 23.11.2010         -- lower house: .
  # 8.  funkční období 24.11.2010 - 20.11.2012         -- lower house: 6
  # 9.  funkční období 21.11.2012 - 18.11.2014         -- lower house: 7 (2013-)
  # 10. funkční období 19.11.2014 - do dnes            -- lower house: .

  a$party[ a$party == "ANO 2011" ] = "ANO2011"
  a$party[ a$party == "ČSSD" ] = "CSSD"
  a$party[ a$party == "KSČM" ] = "KSCM"
  a$party[ a$party == "KDU-ČSL" ] = "KDU"
  a$party[ a$party %in% c("US-DEU", "DEU") ] = "US"
  a$party[ a$party == "PirSZKDU" ] = "PIR"   # n=1
  a$party[ a$party %in% c("SNK", "ED") ] = "SNKED"
  a$party[ a$party == "4KOALICE" ] = "4KOAL" # Čtyřkoalice (KDU-CSL, US-DEU, ODA), n=26
  a$party[ a$party %in% c("Nestran.", "NK") ] = "NK" # Nestraník/Nestraníci ('indep.' movement); n=8 (for 10 mandates)
  a$party[ a$party == "SPO" ] = "SPO" # n=1
  a$party[ a$party == "NEZ" ] = "NEZ" # NEZÁVISLÍ, rightwing independents, n=3 (for 5 mandates)

  # note: candidates with multiple party affiliations ran with the support of
  # these parties, but are unaffiliated otherwise, and so are coded as indep.
  a$party[ a$party == "KDU+NV" ] = "IND"     # n=1
  a$party[ a$party == "KDUČSLSZ" ] = "IND"   # n=3
  a$party[ a$party == "ODS+KČ" ] = "IND"     # n=1
  a$party[ a$party %in% c("STAN", "STANHOPB", "NSK", "SLK") ] = "IND" # n=4 (for 5 mandates)
  a$party[ a$party == "SZ+ČSSD" ] = "IND"    # n=1
  a$party[ a$party == "USDEUODA" ] = "IND"   # n=1 (for 2 mandates)

  # special case: STAN are indep. mayors who ran and collaborated with TOP 09
  a$party[ a$party == "TOP+STAN" ] = "TOP09" # n=3 (for a total of 5 mandates)

  # very small parties with no ParlGov scores (n < 3)
  a$party[ a$party == "HNHRM" ] = "IND"   # Hnutí nezávislých za harmonický rozvoj obcí a měst; n=1 (for 2 mandates)
  a$party[ a$party == "CZ" ] = "IND"      # Cesta změny; n=1 (for 2 mandates)
  a$party[ a$party == "LiRA" ] = "IND"    # Liberálové.CZ (liberal); n=1 (for 2 mandates)
  a$party[ a$party == "Ostravak" ] = "IND" # Ostravak (anti-corruption); n=1
  a$party[ a$party == "SsČR" ] = "IND" # Strana soukromníků České republiky (liberal); n=1
  a$party[ a$party == "SOS" ] = "IND" # Strana pro otevřenou společnost (liberal); n=1 (for 2 mandates)
  a$party[ a$party == "S.cz" ] = "IND" # Severočeši.cz (non-partisan); n=2 (for 4 mandates)
  table(a$party[ !a$party %in% names(colors) ])

  write.csv(a, sponsors, row.names = FALSE)
  write.csv(b, bills, row.names = FALSE)

}

b = read.csv(bills, stringsAsFactors = FALSE)
s = read.csv(sponsors, stringsAsFactors = FALSE)
s$url = paste0("http://senat.cz", s$url)
s$born = NA # missing variable entirely

# =============================================================================
# QUALITY CONTROL
# =============================================================================

# - might be missing: born (int of length 4), constituency (chr),
#   photo (chr, folder/file.ext)
# - never missing: sex (chr, F/M), nyears (int), url (chr, URL),
#   party (chr, mapped to colors)

cat("Missing", sum(is.na(s$born)), "years of birth\n")
stopifnot(is.integer(s$born) & nchar(s$born) == 4 | is.na(s$born))

cat("Missing", sum(is.na(s$constituency)), "constituencies\n")
stopifnot(is.character(s$constituency))

cat("Missing", sum(is.na(s$photo)), "photos\n")
stopifnot(is.character(s$photo) & grepl("^photos(_\\w{2})?/(.*)\\.\\w{3}", s$photo) | is.na(s$photo))

stopifnot(!is.na(s$sex) & s$sex %in% c("F", "M"))
stopifnot(!is.na(s$nyears) & is.integer(s$nyears))
stopifnot(!is.na(s$url) & grepl("^http(s)?://(.*)", s$url))
stopifnot(s$party %in% names(colors))
