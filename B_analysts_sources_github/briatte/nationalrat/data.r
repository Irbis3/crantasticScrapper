root = "http://www.parlament.gv.at"
sponsors = "data/sponsors.csv"
bills = "data/bills.csv"

leg = c("I" = 1, "II" = 2, "III" = 3, "IV" = 4, "V" = 5, "VI" = 6, "VII" = 7,
        "VIII" = 8, "IX" = 9, "X" = 10, "XI" = 11, "XII" = 12, "XIII" = 13,
        "XIV" = 14, "XV" = 15, "XVI" = 16, "XVII" = 17, "XVIII" = 18,
        "XIX" = 19, "XX" = 20, "XXI" = 21, "XXII" = 22, "XXIII" = 23,
        "XXIV" = 24, "XXV" = 25)

# parse bills (selbständige Anträge, closest to definition of private bills;
# some of the sponsors are only reported in the PDF files, so there are some
# missing values in the data, which might explain the sparsity of the graphs

if (!file.exists(bills)) {
  
  b = data_frame()
  for (i in names(leg)[ leg > 18 ]) {
    
    f = paste0("raw/bill-lists/bills-", i, ".html")
    
    if (!file.exists(f))
      download.file(paste0(root, "/PAKT/RGES/index.shtml?AS=ALLE&GBEZ=&AUS=ALLE&requestId=&ALT=&anwenden=Anwenden&LISTE=&NRBR=NR&RGES=A&FR=ALLE&STEP=&listeId=103&GP=", i, "&SUCH=&pageNumber=&VV=&FBEZ=FP_003&xdocumentUri=%2FPAKT%2FRGES%2Findex.shtml&jsMode="), f, mode = "wb", quiet = TRUE)
    
    h = htmlParse(f)
    
    if (i == "XIX") {
      t = readHTMLTable(h, stringsAsFactors = FALSE)[[1]][ -1, 2:3 ]
      names(t) = c("title", "ref")
      t = cbind(date = NA_character_, t)
    } else {
      t = readHTMLTable(h, stringsAsFactors = FALSE)[[1]][ -1, c(-2, -5) ]
      names(t) = c("date", "title", "ref")
    }
    t$url = unique(xpathSApply(h, "//table[@class='tabelle filter']/*/*/a[contains(@href, '/A/')]/@href"))
    
    b = rbind(cbind(legislature = i, t), b)
    
  }
  b$date = strptime(b$date, "%d.%m.%Y")
  b$sponsors = NA
  write.csv(b, bills, row.names = FALSE)
  
}

b = read.csv(bills, stringsAsFactors = FALSE)

# parse sponsor lists (run twice to solve network issues)

u = b$url[ is.na(b$sponsors) ]
for (i in rev(u)) {
  
  cat(sprintf("%4.0f", which(u == i)), i)
  
  f = gsub("/PAKT/VHG/(\\w+)/A/(.*)/index.shtml", "raw/bill-pages/bill-\\1-\\2.html", i)
  if (!file.exists(f))
    download.file(paste0(root, i), f, mode = "wb", quiet = TRUE)
  
  if (!file.info(f)$size) {
    
    cat(": failed\n")
    file.remove(f)
    
  } else {
    
    h = htmlParse(f)
    j = xpathSApply(h, "//div[@class='c_2']//a[contains(@href, 'WWER')]/@href")
    
    b$sponsors[ b$url == i ] = paste0(gsub("\\D", "", j), collapse = ";")
    cat(":", length(j), "sponsor(s)\n")
    
  }
  
}

# roughly a third of all bills are cosponsored

# cat(nrow(b), "bills", sum(grepl(";", b$sponsors)), "sponsored\n")
# print(table(b$legislature, grepl(";", b$sponsors)))

write.csv(b, bills, row.names = FALSE)

b$n_au = 1 + str_count(b$sponsors, ";")
b$legislature = leg[ b$legislature ]

# parse sponsor pages
if (!file.exists(sponsors)) {
  
  # scrape sponsor tables, legislatures XII-XXV
  
  yrs = rep(0, length(leg))
  names(yrs) = names(leg)
  
  s = data_frame()
  
  for (i in names(leg) %>% rev) {
    
    cat(sprintf("%6s", i))
    
    f = paste0("raw/mp-lists/mps-", i, ".html")
    
    if (!file.exists(f)) {
      
      h = htmlParse(paste0("http://www.parlament.gv.at/WWER/NR/ABG/filter.psp?jsMode=EVAL&xdocumentUri=%2FWWER%2FNR%2FABG%2Findex.shtml&GP=", i, "&anwenden=Anwenden&R_WF=FR&FR=ALLE&R_BW=BL&BL=ALLE&W=W&M=M&listeId=4&FBEZ=FW_004")) %>%
        xpathSApply("//a[contains(text(), 'Alle anzeigen')]/@href") %>%
        unique
      
      download.file(paste0("http://www.parlament.gv.at", h), f, mode = "wb", quiet = TRUE)
      
    }
    
    h = htmlParse(f)
    
    # every sponsor row (some with more than one party or constituency)
    t = xpathSApply(h, "//table/tbody/tr")
    
    # this will get all parties and constituencies, creating duplicates
    t = lapply(t, function(h) {
      # name = xpathSApply(h, "td[1]/a", xmlValue) %>% str_clean
      url = xpathSApply(h, "td[1]/a/@href")
      party = xpathSApply(h, "td[2]", xmlValue) %>% str_clean
      partyname = xpathSApply(h, "td[2]/span/@title")
      constituency = xpathSApply(h, "td[4]/span/@title")
      data_frame(url, party, partyname, constituency) # -name
    }) %>% bind_rows
    
    s = rbind(s, cbind(legislature = i, t, stringsAsFactors = FALSE))
    
    cat(":", nrow(t), "sponsors, ")
    
    # length of legislature
    t = xpathSApply(h, "//option[@selected='selected']", xmlValue)[[1]]
    t = str_extract_all(t, "\\d{2}\\.\\d{2}\\.\\d{4}") %>% unlist
    t = strptime(t, "%d.%m.%Y") %>% as.Date
    t = ifelse(length(t) < 2, as.Date(Sys.Date()) - t, diff(t))
    
    yrs[ i ] = round(as.integer(t) / 365)
    cat(yrs[ i ], "year(s) term\n")
    
  }
  
  s$url = paste0(root, s$url)
  s = arrange(s, url, legislature) %>%
    group_by(url) %>%
    mutate(nyears = paste0(unique(legislature), collapse = ";"))
  
  # compute seniority from past mandates
  for (i in nrow(s):1) {
    
    nyears = strsplit(s$nyears[ i ], ";") %>% unlist
    nyears = nyears[ leg[ nyears ] <= leg[ s$legislature[ i ] ] ]
    s$nyears[ i ] = sum(yrs[ nyears ])
    
  }
  
  # scrape sponsors
  
  j = unique(unlist(strsplit(b$sponsors, ";"))) %>% na.omit
  
  k = data_frame()
  
  cat("\nParsing", length(j), "sponsors...\n")
  p = txtProgressBar(max = length(j), style = 3)
  
  for (i in j) {
    
    u = paste0(root, "/WWER/PAD_", i, "/index.shtml")
    # cat(sprintf("%4.0f", which(j == i)), u)
    
    f = paste0("raw/mp-pages/mp-", i, ".html")
    if (!file.exists(f))
      try(download.file(u, f, quiet = TRUE), silent = TRUE)
    
    if (!file.info(f)$size)
      file.remove(f)
    
    h = htmlParse(f)
    
    name = xpathSApply(h, "//h1[@id='inhalt']", xmlValue)
    
    born = xpathSApply(h, "//div[@class='rechteSpalte60']/p[1]", xmlValue)
    born = gsub("Geb\\.: \\d{1,2}\\.\\d{1,2}\\.(\\d{4})(.*)", "\\1", born)
    
    sex = xpathSApply(h, "//div[@class='rechteSpalte60']/ul/li", xmlValue)
    sex = na.omit(str_extract(sex, "Abgeordnete(r)? zum Nationalrat \\("))
    sex = ifelse(grepl("Abgeordneter", unique(sex)), "M", "F")
    stopifnot(length(sex) == 1)
    
    photo = unique(xpathSApply(h, paste0("//div[contains(@class, 'teaserPortraitLarge')]",
                                         "//img[contains(@src, 'WWER')]/@src")))
    
    k = rbind(k, data_frame(id = paste0("id_", i), name, url = u, born, sex,
                            photo = ifelse(is.null(photo), NA, photo)))
    
    setTxtProgressBar(p, which(j == i))
    
  }
  cat("\n")
  
  k$born = as.integer(k$born)
  
  stopifnot(!duplicated(k$id))
  stopifnot(k$url %in% s$url)
  
  write.csv(inner_join(s, k, by = "url"), sponsors, row.names = FALSE)
  
}

s = read.csv(sponsors, stringsAsFactors = FALSE) %>%
  filter(leg[ legislature ] > 18)

# download photos (100% success)
for (i in na.omit(s$photo) %>% unique) {
  
  f = gsub("_WWER_PAD_|\\.jpg", "", gsub("/", "_", i))
  f = paste0("photos/", gsub("_180$|_384$", "", f), ".jpg")
  
  if (!file.exists(f))
    try(download.file(paste0(root, i), f, mode = "wb", quiet = TRUE), silent = TRUE)
  
  if (!file.exists(f) || !file.info(f)$size) {
    
    s$photo[ s$photo == i ] = NA
    
    if (file.exists(f))
      file.remove(f)
    
  } else {
    
    s$photo[ s$photo == i ] = f
    
  }
  
}

# preprocess for WP-DE handles
s$constituency = gsub("\\s", "_", s$constituency)

# find national level (B)
s$kreis = str_extract(s$constituency, "Bundeswahlvorschlag")

# find Landeswahlkreise 1-9
k = "Burgenland|Kärnten|Niederösterreich|Oberösterreich|Salzburg|Steiermark|Tirol|Vorarlberg|Wien"
k = paste0("Landeswahlkreis_", str_extract(s$constituency, k))
s$kreis[ grepl("^\\d_", s$constituency) ] = k[ grepl("^\\d_", s$constituency) ]

# find Regionalwahlkreise 1A, 1B, ..., 9F, 9G
k = "Burgenland_(Nord|Süd)|Klagenfurt|Villach|Kärnten_(Ost|West)|Weinviertel|Waldviertel|Mostviertel|Niederösterreich_(Mitte|Süd)|Wien_Umgebung|Niederösterreich_Süd-Ost|Linz_und_Umgebung|Innviertel|Hausruckviertel|Traunviertel|Mühlviertel|Salzburg_Stadt|Flachgau/Tennengau|Lungau/Pinzgau/Pongau|Graz|Steiermark_(Mitte|Nord(-West)?|Ost|Süd|West)|(Ober|Ost|West)steiermark|Innsbruck(-Land)?|Unterland|Oberland|Osttirol|Vorarlberg_(Nord|Süd)|Wien_(Innen-(Ost|Süd|West)|Nord(-West)?|Süd(-West)?)"
k = paste0("Regionalwahlkreis_", str_extract(s$constituency, k))
s$kreis[ grepl("^\\d\\w_", s$constituency) ] = k[ grepl("^\\d\\w_", s$constituency) ]

# at that stage, the constituency is set to the most precise level at which the
# MP was ever elected during the legislature (best possible approximation)
select(s, constituency, kreis) %>% filter(is.na(kreis) | grepl("_NA$", kreis))

# finalize WP-DE handles
s$kreis[ s$kreis == "Graz" ] = "Regionalwahlkreis_Graz_und_Umgebung"

# ==============================================================================
# CHECK CONSTITUENCIES
# ==============================================================================

cat("Checking constituencies,", sum(is.na(s$kreis)), "missing...\n")
for (i in unique(s$kreis)) {
  
  g = GET(paste0("https://", meta[ "lang"], ".wikipedia.org/wiki/", i))
  
  if (status_code(g) != 200)
    cat("Missing Wikipedia entry:", i, "\n")
  
  g = xpathSApply(htmlParse(g), "//title", xmlValue)
  g = gsub("(.*) – Wikipedia(.*)", "\\1", g)
  
  if (gsub("\\s", "_", g) != i)
    cat("Discrepancy:", g, "(WP) !=", i ,"(data)\n")
  
}

s = select(s, -constituency, constituency = kreis) %>% unique

# party fixes (Austrian politicians like to split just before elections...)

# l. XXV (25, 2013-), parties merged in 2014
s$party[ s$legislature == "XXV" & s$party == "NEOS-LIF, NEOS" ] = "NEOS"

# l. XXV, Marcus Franz, Georg Vetter, Kathrin Nachbaur and Rouven Ertlschweiger,
# all of them were elected with STRONACH in 2013 election and then split to ÖVP
# in June 2015; coded as STRONACH
s$party[ s$legislature == "XXV" & s$party == "STRONACH, ÖVP" ] = "STRONACH"

# l. XXIV, Christoph Hagen and Stefan Markowitz; elected with BZÖ, then split
# and coded as independents, then joined STRONACH"; both coded BZÖ since the
# split occurred just a few months before the 2013 election
s$party[ s$legislature == "XXIV" & s$party == "BZÖ, OK, STRONACH" ] = "BZÖ"

# l. XXIV, Gerhard Huber (independent for a few months only)
s$party[ s$legislature == "XXIV" & s$party == "BZÖ, OK, BZÖ" ] = "BZÖ"

# l. XXIV, Werner Königshofer (independent for a few months only)
s$party[ s$legislature == "XXIV" & s$party == "FPÖ, OK" ] = "FPÖ"

# l. XXIV, Gernot Darmann (BZÖ, then FPÖ a few months before 2013 election)
s$party[ s$legislature == "XXIV" & s$party == "BZÖ, FPÖ" ] = "FPÖ"

# l. XXIII, Karlheinz Klement and Ewald Stadler (independent for a few months only)
s$party[ s$legislature == "XXIII" & s$party == "FPÖ, OK" ] = "FPÖ"

# l. XXII (22, 2002-2006-), split a few months before 2006 election
s$party[ s$legislature == "XXII" & s$party == "F, F-BZÖ" ] = "F"

# l. XX, Peter Rosenstingl (independent for a few months only)
s$party[ s$legislature == "XX" & s$party == "F, OK" ] = "F"

# l. XX, Reinhard Firlinger (L, then F one year after 1996 election)
s$party[ s$legislature == "XX" & s$party == "L,F" ] = "F"

# final simplifications
s$party[ s$party == "F" ] = "FPÖ" # merge F and FPÖ
s$party[ s$party == "L" ] = "LIF" # 3-letter abbr.

stopifnot(!grepl(",", s$party)) # all transitions solved
s = select(s, -partyname) %>% unique

# last, a few MPs still hold multiple constituencies over the same legislature
s = arrange(s, legislature, name, constituency) %>%
  group_by(legislature, name) %>%
  mutate(n = n(), o = n():1)

# selecting the last row (o = 1) selects the most precise level (B < L < R)
filter(s, n >= 1) %>%
  select(legislature, name, constituency, n, o)

s = filter(s, o == 1) %>% select(-n, -o)

stopifnot(!duplicated(paste(s$legislature, s$name)))

# ============================================================================
# QUALITY CONTROL
# ============================================================================

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
