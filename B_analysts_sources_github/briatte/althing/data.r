# scrape bills (Lagafrumvörp); leaving resolutions (Þingsályktunartillögur) out
# URL: http://www.althingi.is/thingstorf/thingmalalistar-eftir-thingum/lagafrumvorp/

root = "http://www.althingi.is"
bills = "data/bills.csv"
sponsors = "data/sponsors.csv"

if (!file.exists(bills)) {
  
  b = data_frame()
  for (i in 145:119) { # accepts down to 20 (1907)
    
    cat(sprintf("%3.0f", i))
    
    f = paste0("raw/bill-lists/bills-", i, ".html")
    
    if (!file.exists(f))
      download.file(paste0(root, "/thingstorf/thingmalalistar-eftir-thingum/lagafrumvorp/?lthing=", i), f,
                    quiet = TRUE, mode = "wb")
    
    h = read_html(f) %>% html_nodes("#t_malalisti")
    
    n = html_nodes(h, "td:nth-child(1)") %>% html_text
    
    if (!length(n)) {
      
      cat(": no bills\n")
      
    } else {
      
      b = rbind(b, data_frame(
        session = i,
        ref = n,
        date = html_nodes(h, "td:nth-child(2)") %>% html_text,
        title = html_nodes(h, "td:nth-child(3)") %>% html_text,
        url = html_nodes(h, "td:nth-child(3) a") %>% html_attr("href"),
        author = html_nodes(h, "td:nth-child(4)") %>% html_text,
        authors = html_nodes(h, "td:nth-child(4) a") %>% html_attr("href")
      ))
      
      cat(":", sprintf("%5.0f", nrow(b)), "total bills\n")
      
    }
    
  }

  b$author = str_clean(b$author)
  b$date = as.Date(strptime(b$date, "%d.%m.%Y"))
  b$n_au = NA
  
  write.csv(b, bills, row.names = FALSE)
  
}

# parse bills

b = read.csv(bills, stringsAsFactors = FALSE)

stopifnot(n_distinct(b$authors) == nrow(b))

j = unique(b$authors[ !grepl("herra$", b$author) ]) # skip ministerial bills
a = data_frame()

cat("Parsing", length(j), "bills...\n")
for (i in rev(j)) {
  
  # cat(sprintf("%4.0f", which(j == i)), str_pad(i, 61, "right"))
  
  f = gsub("(.*)lthing=(\\d+)&skjalnr=(\\d+)", "raw/bill-pages/bill-\\2-\\3.html", i)
  
  if (!file.exists(f))
    try(download.file(paste0(root, i), f, quiet = TRUE, mode = "wb"), silent = TRUE)
  
  if (!file.info(f)$size) {
    
    # cat(": failed\n")
    file.remove(f)
    warning(paste("failed to download bill", i))
    
  } else {

    h = read_html(f, encoding = "UTF-8")
    
    bio = html_nodes(h, ".pgmain") %>% html_text %>% strsplit("\\n") %>% unlist
    url = html_nodes(h, ".pgmain a") %>% html_attr("href")
    
    # select sponsors only
    bio = bio[ grepl("^\\d+", bio) ]
    url = url[ grepl("nfaerslunr", url)]
    
    # exclude Jón Sigurðsson, minister who cosponsored two bills, 2003-2007
    bio = bio[ !grepl("nfaerslunr=1123", url) ]
    url = url[ !grepl("nfaerslunr=1123", url) ]
    
    if (length(url)) {
      
      # cat(":", sprintf("%3.0f", length(url)), "sponsor(s)\n")
      
      a = rbind(a, data_frame(authors = b$authors[ b$authors == i ], bio, url))
      b$n_au[ b$authors == i ] = length(url)
      
    } else {
      
      b$n_au[ b$authors == i ] = 0
      # cat(": no sponsors\n")
      
    }
    
  }
    
}

write.csv(b, bills, row.names = FALSE)

b$legislature = NA
b$legislature[ b$session %in% 142:145 ] = "2013-2017" # election in April, bills from s. 142 start in June
b$legislature[ b$session %in% 137:141 ] = "2009-2013" # election in April, bills from s. 137 start in May
b$legislature[ b$session %in% 134:136 ] = "2007-2009" # election on May 12, bills from s. 134 start May 31
b$legislature[ b$session %in% 130:133 ] = "2003-2007" # election on May 10, no bills in s. 129, s. 130 starts in October
b$legislature[ b$session %in% 124:128 ] = "1999-2003" # election on May 8, bills from s. 124 start in June
b$legislature[ b$session %in% 119:123 ] = "1995-1999" # election on April 8, bills from s. 119 start in June

# print(table(b$legislature, b$n_au > 1, exclude = NULL))

# restrict further data collection to selected legislatures
b = filter(b, !is.na(legislature))

# parse sponsors and solve party transitions

stopifnot(a$authors %in% b$authors)

a$bio = str_clean(a$bio)
a$bio = gsub("^\\d+\\.\\s+|\\d+\\.\\sþm.\\s", "", a$bio)
a$bio = gsub(",\\s+", ",", a$bio)

a = inner_join(a, select(b, legislature, authors), by = "authors")

# legislature 1995-1999: Alþýðubandalag (Ab) includes independents (og óháðir)
a$bio = gsub(",Óh$", ",Ab", a$bio)

# legislature 1995-1999: Social-Democratic alliance (A, Ab, JA, SK, Þ), Sf
a$bio = gsub(",(A|Ab|JA|SK|Þ)$", ",Sf", a$bio)

# legislature 1995-1999: Kristín Ástgeirsdóttir, shortly independent
a$bio[ a$url == "/altext/cv/is/?nfaerslunr=388" & 
         a$legislature == "1995-1999" ] = "Kristín Ástgeirsdóttir RV,Sf"

# legislature 1995-1999: Kristinn H. Gunnarsson, shortly independent
a$bio[ a$url == "/altext/cv/is/?nfaerslunr=386" & 
         a$legislature == "1995-1999" ] = "Kristinn H. Gunnarsson VF,Sf"

# legislature 2003-2007: Sigurlín Margrét Sigurðardóttir, shortly independent
a$bio[ a$url == "/altext/cv/is/?nfaerslunr=1041" & 
         a$legislature == "2003-2007" ] = "Sigurlín Margrét Sigurðardóttir SV,Fl"

# legislature 2003-2007: Kristinn H. Gunnarsson moved to Fl just before election
a$bio[ a$url == "/altext/cv/is/?nfaerslunr=386" & 
         a$legislature == "2003-2007" ] = "Kristinn H. Gunnarsson NV,F"

# legislature 2003-2007: Gunnar Örlygsson sponsored more bills as S
a$bio[ a$url == "/altext/cv/is/?nfaerslunr=657" & 
         a$legislature == "2003-2007" ] = "Gunnar Örlygsson SV,S"

# legislature 2003-2007: Valdimar L. Friðriksson sponsored more bills as Sf
a$bio[ a$url == "/altext/cv/is/?nfaerslunr=669" & 
         a$legislature == "2003-2007" ] = "Valdimar L. Friðriksson SV,Sf"

# legislature 2007-2009: Kristinn H. Gunnarsson, shortly independent
a$bio[ a$url == "/altext/cv/is/?nfaerslunr=386" & 
         a$legislature == "2007-2009" ] = "Kristinn H. Gunnarsson VF,Fl"

# legislature 2007-2009: Jón Magnússon sponsored more bills as Fl
a$bio[ a$url == "/altext/cv/is/?nfaerslunr=689" & 
         a$legislature == "2007-2009" ] = "Jón Magnússon RS,Fl"

# legislature 2007-2009: Karl V. Matthíasson sponsored more bills as Sf
a$bio[ a$url == "/altext/cv/is/?nfaerslunr=373" & 
         a$legislature == "2007-2009" ] = "Karl V. Matthíasson NV,Sf"

# legislature 2009-2013: Ásmundur Einar Daðason sponsored more bills as F
a$bio[ a$url == "/altext/cv/is/?nfaerslunr=707" & 
         a$legislature == "2009-2013" ] = "Ásmundur Einar Daðason NV,F"

# legislature 2009-2013: Atli Gíslason sponsored more bills as U
a$bio[ a$url == "/altext/cv/is/?nfaerslunr=675" & 
         a$legislature == "2009-2013" ] = "Atli Gíslason SU,U"

# legislature 2009-2013: Þráinn Bertelsson sponsored more bills as Vg
a$bio[ a$url == "/altext/cv/is/?nfaerslunr=709" & 
         a$legislature == "2009-2013" ] = "Þráinn Bertelsson RN,Vg"

# legislature 2009-2013: Róbert Marshall sponsored more bills as Sf
a$bio[ a$url == "/altext/cv/is/?nfaerslunr=708" & 
         a$legislature == "2009-2013" ] = "Róbert Marshall SU,Sf"

# legislature 2009-2013: Lilja Mósesdóttir sponsored more bills as U
a$bio[ a$url == "/altext/cv/is/?nfaerslunr=711" & 
         a$legislature == "2009-2013" ] = "Lilja Mósesdóttir RS,U"

# legislature 2009-2013: Guðmundur Steingrímsson sponsored more bills (one more) as U
a$bio[ a$url == "/altext/cv/is/?nfaerslunr=704" & 
         a$legislature == "2009-2013" ] = "Guðmundur Steingrímsson NV,U"

# legislature 2009-2013: Borgarahreyfingin (Bhr) became Hreyfingin (Hr)
a$bio = gsub(",Bhr$", ",Hr", a$bio)

# detect sponsors with more than one party affiliation per legislature
d = select(a, legislature, url, bio) %>%
  unique %>%
  group_by(legislature, url, bio) %>%
  arrange(legislature, url) %>%
  group_by(legislature, url) %>%
  mutate(n = n())

# check: single sponsor row per legislature
stopifnot(!nrow(filter(d, n > 1)))

# fix a few rows with missing data
a$bio[ a$bio == "Davíð Oddsson" ] = "Davíð Oddsson RV,S"           # 1995-1999; nfaerslunr=106
a$bio[ a$bio == "Halldór Ásgrímsson" ] = "Halldór Ásgrímsson AL,F" # 1995-1999; nfaerslunr=8

stopifnot(grepl(",", a$bio))

# scrape sponsors

if (!file.exists(sponsors)) {
  
  j = unique(a$url)
  s = data_frame()
  
  for (i in rev(j)) {
    
    cat(sprintf("%3.0f", which(j == i)), str_pad(i, 31, "right"))
    f = gsub("(.*)nfaerslunr=(\\d+)", "raw/mp-pages/mp-\\2.html", i)
    
    if (!file.exists(f))
      download.file(paste0(root, i), f, quiet = TRUE, mode = "wb")
    
    if (!file.info(f)$size) {
      
      file.remove(f)
      cat(": failed\n")
      
    }
    
    h = read_html(f, encoding = "UTF-8")
    
    name = html_node(h, ".article h1") %>% html_text
    photo = html_nodes(h, ".article img")
    born = html_nodes(h, xpath = "//p[starts-with(text(), 'F.') or starts-with(text(), ' F.') or starts-with(text(), 'Fædd')]") %>% html_text
    born = ifelse(!length(born), NA, str_extract(born, "[0-9]{4}"))
    
    if (!length(photo)) {
      
      p = NA
      
    } else {
      
      p = gsub("html$", "jpg", gsub("raw/mp-pages/mp-", "photos/", f))
      
      if (!file.exists(p))
        download.file(paste0(root, photo %>% html_attr("src")), p, quiet = TRUE, mode = "wb")
      
      if (!file.info(p)$size) {
        
        cat(": failed to download photo")
        file.remove(p)
        warning(paste("failed to download photo", photo))
        
      }
      
    }
    
    s = rbind(s, data_frame(url = i, name, born, photo = p))
    cat("\n")
    
  }

  # checked: no overlap in regex
  s$sex = NA
  s$sex[ grepl("sen$|son$", s$name) | grepl("^(Edward|Ellert|Geir|Halldór\\s|Helgi|Kristján|Magnús|Ólöf|Óttarr|Paul|Pétur|Ragnar|Róbert|Tómas|Þór\\s)", s$name) ] = "M"
  s$sex[ grepl("dóttir$", s$name) | grepl("^(Amal|Ásta|Dýrleif|Elín|Jónína|Katrín|Sandra|Þuríður)",
                                          s$name) ] = "F"
  
  # fix duplicates
  s = group_by(s, name) %>%
    mutate(suffix = 1:n(), duplicate = n() > 1) %>%
    mutate(duplicate = ifelse(duplicate, paste0(name, "-", suffix), name))
  
  cat("Solved", sum(s$name != s$duplicate), "duplicate(s)\n")
  s$name = s$duplicate  
  
  # no duplicates should show up
  rownames(s) = s$name

  write.csv(select(s, url, name, sex, born, photo), sponsors, row.names = FALSE)
  
}

s = read.csv(sponsors, stringsAsFactors = FALSE)

stopifnot(a$url %in% s$url)

# get seniority from CV listings
# http://www.althingi.is/altext/cv/is/
cv = data_frame()
for (i in c("A", "%C1", "B", "D", "E", "F", "G", "H", "I", "%CD", "J", "K", 
           "L", "M", "N", "O", "%D3", "P", "R", "S", "T", "U", "V", "W", 
           "%DE", "%D6")) {
  
  f = paste0("raw/mp-lists/cvs-", i, ".html")
  
  if (!file.exists(f))
    download.file(paste0(root, "/altext/cv/is/?cstafur=", i, "&bnuverandi=0"),
                  f, quiet = TRUE, mode = "wb")
  
  h = read_html(f, encoding = "UTF-8")
  
  u = html_nodes(h, xpath = "//a[contains(@href, 'nfaerslunr')]") %>% html_attr("href")
  m = html_nodes(h, xpath = "//a[contains(@href, 'nfaerslunr')]/..") %>% html_text
  m = str_clean(gsub("(.*)(Al|V)þm.", "", m))
  
  cv = rbind(cv, data_frame(url = u, mandate = m))
  
}

stopifnot(list.files("raw/mp-lists") %>% length == 26)

# expand mandate to years
# this part of the code sends an 'is.na applied to NULL' warning
cv$mandate = sapply(cv$mandate, function(y) {
  
  x = as.numeric(unlist(str_extract_all(y, "[0-9]{4}")))
  
  if (length(x) %% 2 == 1 & grepl("síðan", y)) # "since"
    x = c(x, 2015)
  else if (length(x) %% 2 == 1)
    x = c(x, x[ length(x) ])
  
  x = matrix(x, ncol = 2, byrow = TRUE)   # each row is a pseudo-term (some years are unique years)
  x = apply(x, 1, paste0, collapse = "-") # each value is a sequence
  
  x = lapply(x, function(x) {
    
    x = as.numeric(unlist(strsplit(x, "-")))
    x = seq(x[1], x[2])
    
  })
  
  paste0(sort(unique(unlist(x))), collapse = ";") # all years included in mandate(s)
  
})

stopifnot(!is.na(cv$mandate))
stopifnot(s$url %in% cv$url)

# merge sponsor details to seniority
s = left_join(s, cv, by = "url")

# panelize sponsor details
s = left_join(select(a, legislature, url, bio) %>% unique, s, by = "url")

# extract constituency and party
# http://www.althingi.is/vefur/tmtal.html
s$bio = str_extract(s$bio, "\\w{2},\\w+")
s$constituency = gsub("(.*),(.*)", "\\1", s$bio)
s$party = toupper(gsub("(.*),(.*)", "\\2", s$bio))
s$party[ s$party == "U" ] = "IND" # utan flokka

stopifnot(!is.na(groups[ s$party ]))

# ==============================================================================
# CHECK CONSTITUENCIES
# ==============================================================================

# expand constituency names
s$constituency = c(
  "AL" = "Austurlandskjördæmi", # Austurlands(kjördæmis)
  "NA" = "Norðausturkjördæmi",
  "NE" = "Norðurlandskjördæmi_eystra", # Norðurlands(kjördæmis) eystra
  "NV" = "Norðurlandskjördæmi_vestra", # Norðurlands(kjördæmis) vestra
  "RN" = "Reykjaneskjördæmi", # Reyknesinga (Reykjaneskjördæmis)
  "RS" = "Reykjavíkurkjördæmi_suður",  # Reykjavíkurkjördæmi suður
  "RV" = "Ísland", # Reykvíkinga (national approportionment)
  "SL" = "Suðurlandskjördæmi", # Suðurlands(kjördæmis)
  "SU" = "Suðurkjördæmi",
  "SV" = "Suðvesturkjördæmi", # Suðvesturkjöræmi
  "VF" = "Vestfjarðakjördæmi", # Vestfirðinga (Vestfjarðakjördæmis)
  "VL" = "Vesturland")[ s$constituency ] # Vesturlands

stopifnot(!is.na(s$constituency))

cat("Checking constituencies,", sum(is.na(s$constituency)), "missing...\n")
for (i in s$constituency %>% unique %>% na.omit) {
  
  g = GET(paste0("https://is.wikipedia.org/wiki/", i))
  
  if (status_code(g) != 200)
    cat("Missing Wikipedia entry:", i, "\n")
  
  g = xpathSApply(htmlParse(g), "//title", xmlValue)
  g = gsub("(.*) - Wikipedia(.*)", "\\1", g)
  
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

cat("Missing", sum(is.na(s$constituency)), "constituencies\n")
stopifnot(is.character(s$constituency))

cat("Missing", sum(is.na(s$photo)), "photos\n")
stopifnot(is.character(s$photo) & grepl("^photos(_\\w{2})?/(.*)\\.\\w{3}", s$photo) | is.na(s$photo))

stopifnot(!is.na(s$sex) & s$sex %in% c("F", "M"))
# stopifnot(!is.na(s$nyears) & is.integer(s$nyears)) ## computed on the fly
# stopifnot(!is.na(s$url) & grepl("^http(s)?://(.*)", s$url)) ## used as uids
stopifnot(s$party %in% names(colors))

# final check: find any missing sponsors
for (i in b$authors[ !is.na(b$n_au) ])
  stopifnot(b$n_au[ b$authors == i ] == nrow(filter(a, authors == i)))
