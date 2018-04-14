root     = "http://www.psp.cz/sqw/"
bills    = "data/bills-po.csv"
sponsors = "data/sponsors-po.csv"

# bills

if (!file.exists(bills)) {
  
  l = c()
  # 1 is the first post-independence legislature (no bills details)
  # 7 is the current legislature
  for (i in 2:7) {
    
    f = paste0("raw/po/bill-lists/bills-", i, ".html")
    
    if (!file.exists(f))
      download.file(paste0(root, "tisky.sqw?O=", i, "&PT=K&N=1&F=N&D=1,2,16&RA=1000&TN=3"), f,
                    mode = "wb", quiet = TRUE)
    
    h = htmlParse(f)
    l = c(l, xpathSApply(h, "//a[contains(@href, 'historie.sqw')]/@href"))
    
  }
  l = paste0(root, l, "&sp=1")
  
  b = data.frame()
  for (i in rev(l)) {
    
    cat(sprintf("%3.0f", which(l == i)))
    
    f = paste0("raw/po/bill-pages/bill-", gsub("(.*)o=(\\d)&T=(\\d+)(.*)", "\\2-\\3", i), ".html")
    
    if (!file.exists(f))
      try(download.file(i, f, mode = "wb", quiet = TRUE), silent = TRUE)
    
    if (!file.info(f)$size) {
      
      cat(": failed\n")
      
    } else {
  
      hh = htmlParse(f)
      
      th = xpathSApply(hh, "//a[contains(@href, 'tisky.sqw?vr')]", xmlValue)
      kw = xpathSApply(hh, "//a[contains(@href, 'tisky.sqw?et')]", xmlValue)
      ti = ifelse(grepl("\\?o=7", l), "//h1[2]", "//h1[1]")
      ti = gsub(xpathSApply(hh, "//title", xmlValue), "", xpathSApply(hh, ti, xmlValue))
      dt = str_extract(xpathSApply(hh, "//div[contains(@class, 'section-content')][1]", xmlValue),
                       "[0-9]{1,2}\\.\\s[0-9]{1,2}\\.\\s[0-9]{4}")
      hh = xpathSApply(hh, "//div[contains(@class, 'section-content')]/a[contains(@href, 'id=')]/@href")
      
      cat(":", dt, length(hh), "sponsor(s)\n")
      
      b = rbind(b, data_frame(
        url = i, date = dt, title = ti,
        themes = ifelse(length(th), paste0(th, collapse = ";"), NA),
        keywords = paste0(kw, collapse = ";"),
        authors = paste0(hh, collapse = ";")
      ))
      
    }
    
  }
  
  write.csv(b, bills, row.names = FALSE)
  
}

b = read.csv(bills, stringsAsFactors = FALSE)
a = paste0(root, strsplit(b$authors, ";") %>% unlist %>% unique)

b$date = strptime(b$date, "%d. %m. %Y") %>% as.Date
b$legislature = gsub("(.*)?o=(\\d+)(.*)", "\\2", b$url)
b$n_au = 1 + str_count(b$authors, ";")

# sponsors

if (!file.exists(sponsors)) {
  
  s = data.frame()
  
} else {
  
  s = read.csv(sponsors, stringsAsFactors = FALSE)
    
}

# exclude president/chairs of legislatures 6/7 (no details)
a = a[ !a %in% s$url & !grepl("o=7&id=5462|o=6&id=401", a) ]

if (length(a)) {
  
  for (i in rev(a)) {
    
    cat(sprintf("%4.0f", which(a == i)))

    file = gsub("(.*)o=(\\d)&id=(\\d+)", "raw/po/mp-pages/mp-\\2_\\3.html", i)

    if (!file.exists(file))
      download.file(i, file, mode = "wb", quiet = TRUE)
    
    if (!file.info(file)$size) {
      
      cat(": failed\n")
      file.remove(file)
      
    } else {
      
      h = htmlParse(file, encoding = "iso-8859-2")
      
      cm = gsub("(.*)?o=(\\d+)(.*)", "\\2", i) # current mandate
      om = xpathSApply(h, "//div[@class='page-menu']/ul/li/ul/li/a/@href") # other mandates
      om = gsub("(.*)?o=(\\d+)(.*)", "\\2", om)
      
      nm = xpathSApply(h, "//div[@id='main-content']//h1", xmlValue)
      gd = xpathSApply(h, "//div[@class='figcaption']/p/strong", xmlValue)
      bd = gsub("Narozen(a)?: ", "", gd)
      
      cs = gsub("(.*)Volební kraj: (.*)Zvolen(.*)", "\\2",
                xpathSApply(h, "//div[@class='figcaption']", xmlValue))
      pa = gsub("(.*)Volební kraj: (.*)Zvolen[a]? na kandidátce: (.*)", "\\3",
                xpathSApply(h, "//div[@class='figcaption']", xmlValue))
      
      s = rbind(s, data_frame(
        name = nm, legislature = cm,
        sex = ifelse(grepl("Narozena", gd), "F", "M"),
        born = str_extract(bd, "[0-9]{4}"),
        mandate = paste0(c(om, cm), collapse = ";"),
        constituency = str_clean(cs), party = str_clean(pa), url = i,
        photo = paste0("http://www.psp.cz", 
                       xpathSApply(h, "//div[@class='figure'][1]/a/img/@src"))
      ))
      
      cat(":", tail(s, 1)$name, tail(s, 1)$party, "\n")
      
    }
    
  }
  
  # note: some special characters won't display properly in that file
  write.csv(s, sponsors, row.names = FALSE)
  
}

# ==============================================================================
# CHECK CONSTITUENCIES
# ==============================================================================

s$constituency = paste(s$constituency, "kraj")
s$constituency = gsub("\\s", "_", s$constituency)
s$constituency[ s$constituency == "Hlavní_město_Praha_kraj" ] = "Praha"
s$constituency[ s$constituency == "Vysočina_kraj" ] = "Kraj_Vysočina"

for (i in unique(s$constituency)) {
  
  g = GET(paste0("https://cs.wikipedia.org/wiki/", i))
  
  if (status_code(g) != 200)
    cat("Missing Wikipedia entry:", i, "\n")
  
  g = xpathSApply(htmlParse(g), "//title", xmlValue)
  g = gsub("(.*) – Wikipedie(.*)", "\\1", g)
  
  if (gsub("\\s", "_", g) != i)
    cat("Discrepancy:", g, "(WP) !=", i ,"(data)\n")
  
}

# small bug
s$photo = gsub("http://www.psp.czhttp://www.psp.cz", "http://www.psp.cz", s$photo)

# clean up mandates
s$mandate = sapply(s$mandate, function(x) {
  x = unlist(strsplit(x, ";"))
  x = as.numeric(x[ nchar(x) == 1])
  paste0(sort(x), collapse = ";")
})

# download photos
for (i in unique(s$photo)) {
  photo = gsub("(.*)/(.*)ps/poslanci/i(.*).jpg", "photos_po/\\3_\\2.jpg", i)
  if (!file.exists(photo) | !file.info(photo)$size) {
    try(download.file(i, photo, mode = "wb", quiet = TRUE), silent = TRUE)
    Sys.sleep(1)
  }
  if (!file.exists(photo) | !file.info(photo)$size) {
    file.remove(photo) # will warn if missing
    s$photo[ s$photo == i ] = NA
  } else {
    s$photo[ s$photo == i ] = photo
  }
}

# remove particles
# NOTE: keep "ml." and "st." to distinguish the two Miloslav Kučera in l. 3
s$name = gsub("(.*)\\.\\s|,\\s(.*)", "", s$name)
s$name = gsub("^(Ing\\.genpor|Bc\\.ing|Akad\\.malíř)\\s", "", s$name)
s$name = gsub("\u009d", "ť", s$name, fixed = TRUE)
s$name = gsub("\u009a", "š", s$name, fixed = TRUE)
s$name = gsub("\u008a", "Š", s$name, fixed = TRUE)
s$name = gsub("\u009e", "ž", s$name, fixed = TRUE)
s$name = gsub("\u008e", "Ž", s$name, fixed = TRUE)

# set birth years to numeric
s$born = as.integer(s$born)

# rightwing coalition in legislature 4, also contains a few US-DEU and former independents
s$party[ s$party == "K" ] = "KDU"

# remove accents in party abbreviations to avoid issues when plotting to PDF
s$party = gsub("Č", "C", s$party)

# uppercase (Usvit)
s$party = toupper(s$party)

stopifnot(!is.na(groups[ s$party ]))

# all sponsors identified
stopifnot(all(a %in% s$url))

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
# stopifnot(!is.na(s$nyears) & is.integer(s$nyears)) # computed on the fly
stopifnot(!is.na(s$url) & grepl("^http(s)?://(.*)", s$url))
stopifnot(s$party %in% names(colors))
