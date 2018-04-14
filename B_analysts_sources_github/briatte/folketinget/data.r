root = "http://www.ft.dk"
legislature = c("2004-05" = 1, "2005-06" = 2, "2006-07" = 2,
                "2007-08" = 3, "2008-09" = 3, "2009-10" = 3, "2010-11" = 3,
                "2011-12" = 4, "2012-13" = 4, "2013-14" = 4, "2014-15" = 4,
                "2015-16" = 5)

sponsors = "data/sponsors.csv"

# also accepts "Forslag_til_vedtagelse" (resolutions)
for (type in c("Beslutningsforslag", "Lovforslag")) {

  name = ifelse(type == "Beslutningsforslag", "motions",
                ifelse(type == "Forslag_til_vedtagelse", "resolutions",
                       "bills"))

  bills = paste0("data/", name, ".csv")
  if (!file.exists(bills)) {

    data = data.frame()

    for (i in rev(sort(c(paste0(2015:2004, 1), paste0(c(2004, 2007, 2010, 2014), 2))))) {

      file = paste0("raw/bill-lists/", name, i, ".html")

      if (!file.exists(file))
        download.file(paste0("http://www.ft.dk/Dokumenter/Vis_efter_type/", type, ".aspx?session=", i, "&ministerArea=-1&proposer=&caseStatus=-1&startDate=&endDate=&dateRelatedActivity=&sortColumn=&sortOrder=&startRecord=&numberOfRecords=500&totalNumberOfRecords=#dok"),
                      file, mode = "wb", quiet = TRUE)

      h = htmlParse(file)

      urls = xpathSApply(h, "//tr[contains(@onmouseover, 'docListing')]/@onclick")
      urls = gsub("document.location=\\('|'\\);$", "", urls)

      if (length(urls)) {

        file = gsub("raw/bill-lists/", "data/", gsub("html$", "csv", file))

        links = data.frame()

        cat("Year", substr(i, 1, 4), "session", substr(i, 5, 5), ":",
            sprintf("%3.0f", length(urls)), gsub("s$", "(s)", name), "\n")

        for (j in urls) {

          doc = paste0("raw/bill-pages/", gsub("s$", "-", name), i, "-",
                       gsub("(.*)/(.*)/index\\.htm", "\\2", j), ".html")

          if (!file.exists(doc))
            try(download.file(paste0(root, j), doc, mode = "wb", quiet = TRUE), silent = TRUE)

          if (!file.info(doc)$size) {

            cat("failed:", paste0(root, j), "\n")
            file.remove(doc)

          } else {

            h = htmlParse(doc)

            t = str_clean(xpathSApply(h, "//h1[1]", xmlValue))
            l = paste0(str_clean(xpathSApply(h, "//a[contains(@href, 'findMedlem')]/@href")), collapse = ";")
            h = str_clean(xpathSApply(h, "//p", xmlValue))
            a = gsub("Af (.*)", "\\1", h[ grepl("^Af ", h) ])
            s = gsub("Resumé (.*)", "\\1", h[ grepl("Resumé", h) ])
            v = h[ grepl("^Vedtagetd", h) ]
            v = paste0(v, h[ grepl("^Forkastet", h) ], collapse = ". ")

            h = data_frame(title = t,
                           uid = gsub("/samling/(\\d+)/(vedtagelse|beslutningsforslag|lovforslag)/(L|B|V)(\\d+)/index.htm", "\\1-\\3\\4", j),
                           ministry = gsub("Ministerområde (.*)", "\\1", h[ grepl("Ministerområde", h) ]),
                           year = gsub("Samling: ([0-9-]+)(.*)", "\\1", h[ grepl("Samling", h) ]),
                           status = gsub("(.*)Status: (.*)", "\\2", h[ grepl("Samling", h) ]),
                           authors = ifelse(length(a), a, NA),
                           links = gsub("/Folketinget/findMedlem/|\\.aspx", "", l),
                           vote = ifelse(length(v), v, NA),
                           summary = ifelse(length(s), s, NA), url = j)

            links = rbind(links, h)

          }

        }

        data = rbind(data, links)

      }

    }

    cat(nrow(data), name, "saved\n")
    write.csv(data, bills, row.names = FALSE)

  }

}

d = bind_rows(lapply(dir("data", pattern = "^(bills|motions|resolutions)\\.csv", full.names = TRUE),
                     read.csv, stringsAsFactors = FALSE))

# drop one faulty bill (by a minister)
d = d[ !d$links == "jorn-neergaard-larsen", ]

d$year[ nchar(d$year) > 7 ] = NA

# fix a few missing values
d$year[ is.na(d$year) & substr(d$url, 10, 14) == "20061" ] = "2006-07"
d$year[ is.na(d$year) & substr(d$url, 10, 14) == "20081" ] = "2008-09"
d$year[ is.na(d$year) & substr(d$url, 10, 14) == "20121" ] = "2012-13"
d$year[ is.na(d$year) & substr(d$url, 10, 14) == "20131" ] = "2013-14"

d$legislature = legislature[ d$year ]

d$type = ifelse(grepl("-B", d$uid), "motion",
                ifelse(grepl("-V", d$uid), "resolution", "bill"))

# print(table(d$type, d$legislature, exclude = NULL))
# print(table(d$year, d$legislature, exclude = NULL))
cat("Scraped", nrow(d), "documents")

# categorize bills

d$n_au = 1 + str_count(d$links, ";")
d = subset(d, !grepl("minister", d$authors, ignore.case = TRUE))

cat(":", nrow(d), "MP bills", sum(d$n_au > 1), "cosponsored bills\n")

d$theme = d$ministry
d$theme[ d$ministry %in% c("Erhvervs- og Vækstministeriet", "Handels- og Udviklingsministeriet", "Økonomi- og Erhvervsministeriet",  "Økonomi- og Indenrigsministeriet", "Finansministeriet") ] = "Economy"
d$theme[ d$ministry == "Beskæftigelsesministeriet" ] = "Employment"
d$theme[ d$ministry == "Europaministeriet" ] = "EU"
d$theme[ d$ministry == "Forsvarsministeriet" ] = "Defence"
d$theme[ d$ministry == "Indenrigs- og Socialministeriet" ] = "Interior,Social Affairs"
d$theme[ d$ministry == "Indenrigs- og Sundhedsministeriet" ] = "Health"
d$theme[ d$ministry == "Ministeriet for Sundhed og Forebyggelse" ] = "Interior,Health"
d$theme[ d$ministry == "Justitsministeriet" ] = "Justice"
d$theme[ d$ministry == "Kirkeministeriet" ] = "Church"
d$theme[ d$ministry == "Klima- og Energiministeriet" ] = "Climate,Energy"
d$theme[ d$ministry == "Klima-, Energi- og Bygningsministeriet" ] = "Climate,Energy,Building"
d$theme[ d$ministry == "Miljøministeriet" ] = "Environment"
d$theme[ d$ministry == "Kulturministeriet" ] = "Culture" # culture
d$theme[ d$ministry == "Ministeriet for Børn og Undervisning" ] = "Children,Education" # culture
d$theme[ d$ministry == "Ministeriet for Børn, Ligestilling, Integration og Sociale Forhold" ] = "Children,Gender Equality,Integration,Social Affairs" # culture
d$theme[ d$ministry == "Ministeriet for By, Bolig og Landdistrikter" ] = "Housing,Land" # culture
d$theme[ d$ministry == "Ministeriet for Familie- og Forbrugeranliggender" ] = "Family,Consumer Affairs"
d$theme[ d$ministry == "Ministeriet for Flygtninge, Indvandrere og Integration" ] = "Refugees,Immigration,Integration"
d$theme[ d$ministry == "Ministeriet for Fødevarer, Landbrug og Fiskeri" ] = "Food,Agriculture,Fisheries"
d$theme[ d$ministry == "Ministeriet for Forskning, Innovation og Videregående Uddannelser" ] = "Science,Innovation,Higher Education"
d$theme[ d$ministry == "Ministeriet for Videnskab, Teknologi og Udvikling" ] = "Science,Technology,Innovation"
d$theme[ d$ministry == "Ministeriet for Ligestilling" ] = "Gender Equality"
d$theme[ d$ministry == "Ministeriet for Ligestilling og Kirke" ] = "Church,Gender Equality"
d$theme[ d$ministry %in% c("Ministerområde", "Statsministeriet", "Folketinget") ] = "Institutional"
d$theme[ d$ministry == "Skatteministeriet" ] = "Taxation"
d$theme[ d$ministry == "Socialministeriet" ] = "Social Affairs"
d$theme[ d$ministry == "Social- og Integrationsministeriet" ] = "Social Security,Integration"
d$theme[ d$ministry == "Social-, børne- og integrationsministeriet" ] = "Social Affairs,Children,Integration"
d$theme[ d$ministry %in% c("Transportministeriet", "Trafikministeriet") ] = "Transport" # and roads
d$theme[ d$ministry %in% c("Transport- og Energiministeriet", "Trafik- og Energiministeriet") ] = "Transport,Energy"
d$theme[ d$ministry == "Uddannelses- og Forskningsministeriet" ] = "Education,Science"
d$theme[ d$ministry == "Udenrigsministeriet" ] = "Foreign Affairs"
d$theme[ d$ministry == "Undervisningsministeriet" ] = "Education"
d$theme[ d$ministry == "Velfærdsministeriet" ] = "Welfare"
d$theme[ d$ministry == "Ministeriet for Udviklingsbistand" ] = "Development"
d$theme[ d$ministry == "Handels- og europaministeriet" ] = "Trade,EU"
# print(table(unlist(strsplit(d$theme, ","))))

# sponsors

u = unique(na.omit(unlist(strsplit(d$links, ";"))))
u = gsub("\\s", "%20", paste0("/Folketinget/findMedlem/", u, ".aspx"))
cat("Parsing", length(u), "sponsors...\n")

s = data.frame()

# # get all MPs from the current legislature:
# h = htmlParse("http://www.ft.dk/Folketinget/searchResults.aspx?letter=ALLE&pageSize=500&pageNr=")
# xpathSApply(h, "//a[contains(@href, '/findMedlem/')]/@href")

# skipping over faulty page
for (k in rev(u[ u != "/Folketinget/findMedlem/RVUFEL.aspx" ])) {

  # cat(sprintf("%3.0f", which(u == k)))
  file = gsub("aspx$", "html", gsub("/Folketinget/findMedlem/", "raw/mp-pages/mp-", k))

  if (!file.exists(file))
    try(download.file(paste0(root, k), file, mode = "wb", quiet = TRUE), silent = TRUE)

  if (!file.info(file)$size) {

    cat(": failed", paste0(root, k), "\n")
    file.remove(file)

  } else {

    h = htmlParse(file)
    # cat(":", file, ":",
    #     gsub("Folketinget - ", "",
    #          str_clean(xpathSApply(h, "//title", xmlValue))), "\n")

    constit = xpathSApply(h, "//strong[text()='Medlemsperiode']/..", xmlValue)
    constit = gsub("(.*)\\si\\s(.*)(\\s(Amts|Stor)kreds)(.*)", "\\2\\3", constit)

    img = xpathSApply(h, "//img[contains(@src, '/media/')]/@src")
    s = rbind(s, data_frame(
      file,
      name = xpathSApply(h, "//meta[@name='Fullname']/@content"),
      func = xpathSApply(h, "//meta[@name='Function']/@content"),
      party = xpathSApply(h, "//meta[@name='Party']/@content"),
      mandate = xpathSApply(h, "//meta[@name='MfPeriod']/@content"),
      constituency = ifelse(length(constit), constit, NA),
      job = xpathSApply(h, "//div[contains(@class, 'person')]/p[2]/strong", xmlValue),
      photo = ifelse(length(img), img, NA),
      url = k,
      bio = xpathSApply(h, "//div[contains(@class, 'tabContent')]/p[1]", xmlValue)
    ))

  }

}

# remove two strictly ministerial sponsors
s = subset(s, func != "exmin")

# ==============================================================================
# CHECK CONSTITUENCIES
# ==============================================================================

# special constituencies
s$constituency[ grepl("Grønland", s$constituency) ] = "Grønland"
s$constituency[ grepl("Færøerne", s$constituency) ] = "Færøerne"

# convert to Wikipedia Dansk handles
s$constituency = gsub("\\s", "_", s$constituency)

cat("Checking constituencies,", sum(is.na(s$constituency)), "missing...\n")
for (i in na.omit(unique(s$constituency))) {

  g = GET(paste0("https://", meta[ "lang"], ".wikipedia.org/wiki/", i))

  if (status_code(g) != 200)
    cat("Missing Wikipedia entry:", i, "\n")

  g = xpathSApply(htmlParse(g), "//title", xmlValue)
  g = gsub("(.*) - Wikipedia(.*)", "\\1", g)

  if (gsub("\\s", "_", g) != i)
    cat("Discrepancy:", g, "(WP) !=", i ,"(data)\n")

}

s$mandate = sapply(s$mandate, function(x) {
  x = str_extract_all(x, "[0-9]{4}")
  paste0(unique(unlist(x)), collapse = ";")
})

s$mandate[ s$mandate == "" ] = "2011;2012;2013;2014"

s$sex = str_extract(s$bio, "(D|d)atter af|(S|s)øn af")
s$sex = ifelse(grepl("(D|d)atter af", s$sex), "F", "M")
s$sex[ !grepl("(D|d)atter af|(S|s)øn af", s$bio) ] = NA

# fill in a few missing values
s$sex[ is.na(s$sex) &
         grepl("^(Anne|Annika|Dorrit|Erika|Eva\\s|Fatma|Helge|Ida|Karin|Linda|Lise|Lykke|Maria|Marlene|Mette|Mie|Sanne|Özlem Sara|Pia|Sofia|Stine|Susanne|Trine)", s$name) ] = "F"
s$sex[ is.na(s$sex) &
         grepl("^(Dan\\s|Erling|Eyvind|Hans|Ja(c|k)ob|Jens|Jeppe|Jesper|Johs\\.|Jør(n\\s|gen)|Kamal|Kuupik|Nick|Niels|Nikolaj|Pelle|Per\\s|Peter|Rasmus|Søren|Thomas|Uffe)", s$name) ] = "M"

s$born = str_extract(s$bio, "født [0-9\\.]+ [a-z\\.]+ \\d{4}")
s$born = sapply(str_extract_all(s$born, "[0-9]{4}"), length)
s$born[ s$born != 1 ] = 0
s$born[ s$born == 1 ] = str_extract(s$bio[ s$born == 1 ], "[0-9]{4}")
s$born[ s$born == 0 ] = NA
s$born = as.integer(s$born)

s$party[ is.na(s$party) | s$party %in% c("", "Indep") ] = "Independent"
s$party[ s$party == "Ny Alliance" ] = "Liberal Alliance"

write.csv(s, sponsors, row.names = FALSE)

# download photos
for (i in 1:nrow(s)) {
  
  if (is.na(s$photo[ i ]))
    next
  
  photo = gsub("/Folketinget/findMedlem/(.*)\\.aspx", "photos/\\1.jpg", s$url[ i ])

  if (!file.exists(photo)) {
    
    h = try(GET(paste0(root, "/Folketinget/findMedlem/", gsub("\\s", "%20", s$photo[ i ]))))
    
    if (h$headers$`content-type` == "image/jpeg")
      writeBin(content(h, "raw"), photo)
    
  }
  
  if (file.exists(photo))
    s$photo[ i ] = photo
  else
    s$photo[ i ] = NA
  
}

s$url = gsub("/Folketinget/findMedlem/|\\.aspx", "", s$url)
s$url = gsub("\\s", "%20", s$url)
rownames(s) = s$url

# translating to abbreviations (for consistency)
s$party[ s$party == "Enhedslisten" ] = "E"
s$party[ s$party == "Socialistisk Folkeparti" ] = "SFP"
s$party[ s$party == "Socialdemokratiet" ] = "SD"
s$party[ s$party == "Radikale Venstre" ] = "RV"
s$party[ s$party == "Kristendemokraterne" ] = "KD"
s$party[ s$party == "Liberal Alliance" ] = "LA"
s$party[ s$party == "Venstre" ] = "V"
s$party[ s$party == "Det Konservative Folkeparti" ] = "KFP"
s$party[ s$party == "Dansk Folkeparti" ] = "DFP"
s$party[ s$party == "Inuit Ataqatigiit" ] = "IA"
s$party[ s$party == "Siumut" ] = "S"
s$party[ s$party == "Sambandsflokkurin" ] = "SF"
s$party[ s$party == "Javnaðarflokkurin" ] = "JF"
s$party[ s$party == "Alternativet" ] = "IND" # Green centre-left party by Uffe Elbæk
s$party[ s$party == "Tjóðveldi" ] = "IND" # Faroese party, Høgni Hoydal
s$party[ s$party == "Independent" ] = "IND"
stopifnot(!is.na(groups[ s$party ])) # no chance this fails

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
# stopifnot(!is.na(s$url) & grepl("^http(s)?://(.*)", s$url)) # used as uids
stopifnot(s$party %in% names(colors))
