bills = "data/bills.csv"
sponsors = "data/sponsors.csv"
root = "http://www.oireachtas.ie/"

#
# download bills
#

bill = "//a[contains(@href, 'viewdoc.asp?DocID=')]" # XPath to bill pages

if (!file.exists(bills)) {

  b = data_frame()
  for (y in 2015:1997) {

    cat(y)
    f = paste0("raw/bill-lists/bills-", y, "-page-1.html")

    if (!file.exists(f))
      download.file(paste0(root, "/viewdoc.asp?DocID=-1&StartDate=1+January+", y, "&CatID=59"),
                    f, mode = "wb", quiet = TRUE)

    h = htmlParse(f)

    # first page links and titles
    l = xpathSApply(h, paste0(bill, "/@href"))
    t = xpathSApply(h, bill, xmlValue)

    # pages
    p = xpathSApply(h, "//a[contains(@href, 'mypage=')]/@href")
    p = p[ !grepl("OrderAscending=0$", p) ]

    for (j in p) {

      cat(".")
      f = paste0("raw/bill-lists/bills-", y, "-page-", 1 + which(p == j), ".html")

      if (!file.exists(f))
        download.file(paste0(root, gsub("\\s", "%20", j)), f, mode = "wb", quiet = TRUE)

      h = htmlParse(f)

      l = c(l, xpathSApply(h, paste0(bill, "/@href")))
      t = c(t, xpathSApply(h, bill, xmlValue))

    }

    d = data_frame(year = y, url = l, title = t)
    d = filter(d, !title %in% c("more ...", " [view more]", "eMail Alerts"), !grepl("DocID=-1", url))

    cat("", nrow(d), "bills\n")
    b = rbind(b, d)

  }

  b = filter(b, !grepl("DocID=4839", url)) # remove list of old bills

  y = b$url
  cat("Parsing", length(y), "bills\n")

  b = data_frame()

  for (i in rev(y)) {

    f = gsub("(.*)?DocID=(\\d+)(.*)", "raw/bill-pages/bill-\\2.html", i)

    if (!file.exists(f))
      try(download.file(paste0(root, gsub("\\s", "%20", i)),
                        f, mode = "wb", quiet = TRUE), silent = TRUE)

    if (!file.exists(f)) {

      cat(str_pad(which(y == i), 4), ": no file\n")
      next

    }

    h = readLines(f, warn = FALSE, encoding = "iso-8859-1")
    h = enc2utf8(h)

    txt = which(grepl("Bill Number", h, useBytes = TRUE))

    if (!length(txt)) {

      cat(str_pad(which(y == i), 4), ": empty\n")
      next

    }

    # in case the info is on multiple lines
    if (!any(grepl("Status", h[ txt ], useBytes = TRUE)))
      txt = txt[1]:which(grepl("Status:", h, useBytes = TRUE))

    h = unlist(strsplit(h[ txt ], "<BR>|<br />"))
    n = h[ grepl("Bill Number", h) ] # ref. number
    t = h[ grepl("Source:", h) ] # source (type)
    a = h[ grepl("Sponsored", h) ] # authors

    if (!length(a)) {

      cat(str_pad(which(y == i), 4), ": no sponsors\n")

    } else {

      b = rbind(b, data_frame(
        url = i,
        ref = gsub("(.*)(\\d+)\\s?of (\\d+)(.*)", "\\3-\\2", n),
        chamber = str_extract(t, "Private|Government"),
        authors = gsub("Sponsored(\\sby)?\\s", "", a)
      ))

    }

  }

  # origin: private members or government
  b$origin = ifelse(grepl("Priv", b$chamber), "PM", "GOV")

  # remove governmental bills + a few private bills
  b = filter(b, chamber != "GOV", !grepl("inister|behalf|Private Sponsor", authors))

  b$authors = gsub("by(&nbsp;|:)|<xmlns:(.*)>|</xmlns:(.)>|\\.", "", b$authors)
  b$authors = gsub("<(FONT|SPAN)(.*)>|</(FONT|SPAN)>|</STRONG>", "", b$authors)
  b$authors = gsub("<e1>|&aacute;", "á", b$authors)
  b$authors = gsub("<f3>|&oacute;", "ó", b$authors)
  b$authors = gsub("<d3>|&Oacute;", "Ó", b$authors)
  b$authors = gsub("<e9>|&eacute;", "é", b$authors)
  b$authors = gsub("<c9>|&Eacute;", "É", b$authors)
  b$authors = gsub("<ed>|&iacute;", "í", b$authors)
  b$authors = gsub("&uacute;", "ú", b$authors)
  b$authors = gsub("&nbsp;", " ", b$authors)
  b$authors = gsub(" & ", " and ", b$authors)
  b$authors = gsub("\\sand\\s|;,?\\s", ", ", b$authors)

  # no mix between Dep and Sen
  table(grepl("Depu", b$authors), grepl("Sena", b$authors))
  b$chamber[ grepl("Depu", b$authors) ] = "da"
  b$chamber[ grepl("Sena", b$authors) ] = "se"

  # will remove bills with no sponsors
  b = filter(b, authors != "") # 21 more problematic cases

  b$authors = gsub("Depu(t)?(y|ies)\\s?|Senator(s)?\\s?|\\(s\\)", "", b$authors)
  b$authors = gsub("\\s+", " ", b$authors) %>% str_trim

  b = filter(b, !grepl("&|<|>", b$authors)) # two messy bills with single authors

  b$authors[ grepl(";", b$authors)] # should be empty

  b$authors = gsub(",(\\sand)?\\s?", ";", b$authors)
  strsplit(b$authors, ";") %>% unlist %>% table

  b$n_au = 1 + str_count(b$authors, ";")
  table(b$n_au)

  # endless list of names mismatches (using MP names as correct spelling)
  b$authors = gsub("Aevril Power", "Averil Power", b$authors)
  b$authors = gsub("Bernard J Durkan", "Bernard Durkan", b$authors)
  b$authors = gsub("Billy Timmins", "Billy Godfrey Timmins", b$authors)
  b$authors = gsub("Burton Joan", "Joan Burton", b$authors)
  b$authors = gsub("byJohn Gormley", "John Gormley", b$authors)
  b$authors = gsub("Caoimhghín ÓCaoláin|Caomhghin Ó Caoláin", "Caoimhghín Ó Caoláin", b$authors)
  b$authors = gsub("Ciarán Cuffe", "Ciaran Cuffe", b$authors)
  b$authors = gsub("Ciaran Lynch", "Ciarán Lynch", b$authors)
  b$authors = gsub("Colm Keaveny", "Colm Keaveney", b$authors)
  b$authors = gsub("Daniel Neville", "Dan Neville", b$authors)
  b$authors = gsub("David G Stanton", "David Stanton", b$authors)
  b$authors = gsub("David Norris", "David Patrick Bernard Norris", b$authors)
  b$authors = gsub("Deirdre de Burca|Déirdre de Búrca", "Deirdre De Búrca", b$authors)
  b$authors = gsub("Donie Cassidy", "Donie (Daniel) Cassidy", b$authors)
  b$authors = gsub("Éamon Gilmore", "Eamon Gilmore", b$authors)
  b$authors = gsub("Eamon Maloney", "Eamonn Maloney", b$authors)
  b$authors = gsub("Fergal Quinn", "Feargal Quinn", b$authors)
  b$authors = gsub("Fergus O Dowd", "Fergus O'Dowd", b$authors)
  b$authors = gsub("Fintan Coogan", "Fintan (Junior) Coogan", b$authors)
  b$authors = gsub("Ivan Bacik", "Ivana Bacik", b$authors)
  b$authors = gsub("John Browne \\(Carlow-Kilkenny\\)", "John Browne-1", b$authors)
  b$authors = gsub("John Hanafin", "John Gerard Hanafin", b$authors)
  b$authors = gsub("Liz Mc Manus", "Liz McManus", b$authors)
  b$authors = gsub("Mary Henry", "Mary E F Henry", b$authors)
  b$authors = gsub("Maurice Cumminns", "Maurice Cummins", b$authors)
  b$authors = gsub("Michael Cummins", "Maurice Cummins", b$authors) # happens once
  b$authors = gsub("Michael DHiggins", "Michael D Higgins", b$authors)
  b$authors = gsub("Michael (D)?Higgins", "Michael D Higgins", b$authors)
  b$authors = gsub("Michéal Martin", "Micheál Martin", b$authors)
  b$authors = gsub("Padraig MacLochlainn", "Pádraig Mac Lochlainn", b$authors)
  b$authors = gsub("Paul Gogarty", "Paul Nicholas Gogarty", b$authors)
  b$authors = gsub("Peadar Tób(i|í)n", "Peadar Tóibín", b$authors)
  b$authors = gsub("Proinsias DeRossa", "Proinsias De Rossa", b$authors)
  b$authors = gsub("Rónan Mullen", "Rónán Mullen", b$authors)
  b$authors = gsub("Ruairi Quinn", "Ruairí Quinn", b$authors)
  b$authors = gsub("Seán Barrett", "Sean Barrett", b$authors)
  b$authors = gsub("Seán Crowe", "Sean Crowe", b$authors)
  b$authors = gsub("Seán Fleming", "Sean Fleming", b$authors)
  b$authors = gsub("Sean Ó Fearghail", "Seán Ó Fearghaíl", b$authors)
  b$authors = gsub("S(eá|éa)n Ryan", "Sean Ryan", b$authors)
  b$authors = gsub("Shane (P N )?Ross", "Shane Peter Nathaniel Ross", b$authors)
  b$authors = gsub("Síle DeValera", "Síle de Valera", b$authors)
  b$authors = gsub("Stephen S Donnelly", "Stephen Donnelly", b$authors)
  b$authors = gsub("Thomas P Broughan", "Tommy Broughan", b$authors)
  b$authors = gsub("Trevor Sargeant", "Trevor Sargent", b$authors)

  # [ S.21, D.28 ], 1997-2002, 2002-2007, 2007-2011, 2011-2016 [ S.24, D.31 ]
  b$year = substr(b$ref, 1, 4) %>% as.integer
  b$legislature = NA
  b$legislature[ b$chamber == "da" & b$year %in% 1997:2001 ] = 28
  b$legislature[ b$chamber == "se" & b$year %in% 1997:2001 ] = 21
  b$legislature[ b$chamber == "da" & b$year %in% 2002:2006 ] = 29
  b$legislature[ b$chamber == "se" & b$year %in% 2002:2006 ] = 22
  b$legislature[ b$chamber == "da" & b$year %in% 2007:2010 ] = 30
  b$legislature[ b$chamber == "se" & b$year %in% 2007:2010 ] = 23
  b$legislature[ b$chamber == "da" & b$year %in% 2011:2015 ] = 31
  b$legislature[ b$chamber == "se" & b$year %in% 2011:2015 ] = 24

  table(b$chamber, b$legislature, exclude = NULL)
  b = filter(b, !is.na(legislature))

  write.csv(b, bills, row.names = FALSE)

}

b = read.csv(bills, stringsAsFactors = FALSE)

#
# sponsors
#

if (!file.exists(sponsors)) {

  s = data_frame()

  # Dáil Éireann, 1997-2016
  for (i in 31:28) {

    f = paste0("raw/mp-lists/mps-d-", i, ".html")

    if (!file.exists(f))
      download.file(paste0("http://www.oireachtas.ie/members-hist/default.asp?housetype=0&HouseNum=",
                           i, "&disp=mem"), f, mode = "wb", quiet = TRUE)

    h = htmlParse(f)

    s = rbind(s, data_frame(
      legislature = i,
      chamber = "D",
      url = xpathSApply(h, "//a[contains(@href, 'MemberID=')]/@href")
    ))

  }

  # Seanad Éireann, 1997-2016
  for (i in 24:21) {

    f = paste0("raw/mp-lists/mps-s-", i, ".html")

    if (!file.exists(f))
      download.file(paste0("http://www.oireachtas.ie/members-hist/default.asp?housetype=1&HouseNum=",
                           i, "&disp=mem"), f, mode = "wb", quiet = TRUE)

    h = htmlParse(f)

    s = rbind(s, data_frame(
      legislature = i,
      chamber = "S",
      url = xpathSApply(h, "//a[contains(@href, 'MemberID=')]/@href")
    ))

  }

  write.csv(s, sponsors, row.names = FALSE)

}

s = read.csv(sponsors, stringsAsFactors = FALSE)

y = str_extract(s$url, "MemberID=\\d+") %>% unique
cat("Parsing", length(y), "sponsors\n")

s = data_frame()

for (i in y) {

  f = paste0(gsub("MemberID=", "raw/mp-pages/mp-", i), ".html")

  if (!file.exists(f))
    download.file(paste0("http://www.oireachtas.ie/members-hist/default.asp?", i),
                  f, mode = "wb", quiet = TRUE)

  if (file.exists(f)) {

    h = htmlParse(f, encoding = "UTF-8")

    photo = xpathSApply(h, "//div[@class='memberdetails']/img/@src")
    name = xpathSApply(h, "//div[@class='memberdetails']/h3", xmlValue)
    born = xpathSApply(h, "//div[@class='memberdetails']/p[1]", xmlValue)
    party1 = xpathSApply(h, "//div[@class='memberdetails']//a[contains(@href, 'Party')]", xmlValue)
    party1 = ifelse(!length(party1), NA, party1)

    mandate = xpathSApply(h, "//li[@class='housenbr']", xmlValue)
    constituency = xpathSApply(h, "//div[@class='memberdetails']//a[contains(@href, 'ConstID')]",
                               xmlValue)
    party = xpathSApply(h, "//li[@class='housenbr']//following-sibling::li[3]", xmlValue)

    if (!length(party))
      party = NA

    s = rbind(s, data_frame(
      uid = gsub("\\D", "", i),
      name, born, mandate, constituency, party1, party,
      photo = ifelse(is.null(photo), NA, photo)
    ))

  }

}

#
# finalize sponsor variables
#

s$born = str_extract(s$born, "\\d{4}") %>% as.integer

# note: chairs do not show up in the networks, so imputation is alright
s$party[ grepl("Office|Cathaoirleach", s$party) ] = NA
s$party[ is.na(s$party) ] = s$party1[ is.na(s$party) ]

s$party = gsub("Party:\\s|\\(2011\\)|\\smembers(.*)", "", s$party)
s$party = str_clean(s$party)

s$sex = s$name
s$sex[ grepl("Mr\\.", s$sex) ] = "M"
s$sex[ grepl("M(r)?s\\.", s$sex) ] = "F"
s$sex[ grepl("(Dermot|Edward|James|Jerry|John|Liam|Leo|Martin|Maurice|Michael|Pat|Rory|Tom)\\s", s$sex) ] = "M"
s$sex[ grepl("(Katherine|Máirín|Marian|Mary)\\s", s$sex) ] = "F"

s$name = gsub("Professor\\s|\\s(\\()?(Deceased|Resigned)(\\))?", "", s$name)
s$name = gsub("(M|D)(r|s|rs)\\.\\s|\\.", "", s$name) %>% str_trim

# duplicates
s$name[ s$uid == 107 ] = "John Browne-1" # matched in sponsors lists
# s$name[ s$uid == 108 ] = "John Browne-2" # matched in sponsors lists

a = strsplit(b$authors, ";") %>% unlist
stopifnot(a[ nchar(a) > 2 ] %in% s$name)

s$legislature = str_extract(s$mandate, "\\d{2}") %>% as.integer
s$chamber = ifelse(grepl("Dáil", s$mandate), "da", "se")
table(s$chamber, s$legislature, exclude = NULL)

s$party = str_clean(s$party)
s$party[ grepl("Ceann Comhairle|Cathaoirleach", s$party) ] = "CHAIR" # chamber chairs, not used
s$party[ grepl("The Workers' Party", s$party) ] = "WP" # not used
s$party[ grepl("Democratic Left", s$party) ] = "DL"
s$party[ grepl("Anti Austerity Alliance", s$party) ] = "AAA"
s$party[ grepl("Fianna Fáil", s$party) ] = "FF"
s$party[ grepl("Fine Gael", s$party) ] = "FG"
s$party[ grepl("Sinn Féin", s$party) ] = "SF"
s$party[ grepl("Labour", s$party) ] = "LAB"
s$party[ grepl("Green Party", s$party) ] = "GP"
s$party[ grepl("Progressive Democrats", s$party) ] = "PD"
s$party[ grepl("Socialist Party", s$party) ] = "SOC"
s$party[ grepl("RENUA Ireland", s$party) ] = "RENUA"

# last two residuals are not used in the networks
s$party[ grepl("Independent|Other|People Before Profit|Unemployed", s$party) ] = "IND"

s$party[ s$uid == 1955 & s$legislature == 14 ] = "FF"
s$party[ s$uid == 2150 & s$legislature == 21 ] = "IND"
table(s$party, s$legislature, exclude = NULL)

#
# sponsor photos
#

cat("Downloading", n_distinct(na.omit(s$photo)), "photos\n")
for (i in na.omit(s$photo)) {

  f = paste0("photos/", basename(i))

  if (!file.exists(f))
    download.file(paste0(root, i), f, mode = "wb", quiet = TRUE)

  if (file.exists(f))
    s$photo[ s$photo == i ] = f
  else
    s$photo[ s$photo == i ] = NA

}

# ==============================================================================
# CHECK CONSTITUENCIES
# ==============================================================================

# note: matches are not perfect, but should work fine for modern legislatures

stopifnot(!is.na(s$constituency))
s$constituency[ s$constituency == "Nominated by the Taoiseach" ] =
  "Nominated members of Seanad Éireann"
s$constituency[ s$constituency == "Dublin North Central" ] = "Dublin North-Central"
s$constituency[ s$constituency == "Dublin Mid West" ] = "Dublin Mid-West"
s$constituency[ s$constituency == "Longford -Westmeath" ] = "Longford-Westmeath"
s$constituency[ s$constituency == "Sligo Leitrim North" ] = "Sligo-Leitrim"
s$constituency[ s$constituency == "Laoighis-Offaly" ] = "Offaly"
s$constituency[ s$constituency == "Roscommon Leitrim South" ] = "Roscommon-South Leitrim"
s$constituency[ s$constituency == "Kerry North Limerick West" ] = "Kerry North-West Limerick"

s$constituency = gsub("\\(|\\)", "", s$constituency)
s$constituency = ifelse(s$chamber == "se", s$constituency,
                        paste(s$constituency, "(Dáil Éireann constituency)"))
s$constituency = gsub("-", "–", s$constituency)
s$constituency = gsub("\\s", "_", s$constituency)

cat("Checking constituencies,", sum(is.na(s$constituency)), "missing...\n")
for (i in s$constituency %>% unique %>% na.omit) {

  g = GET(paste0("https://en.wikipedia.org/wiki/", i))

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
