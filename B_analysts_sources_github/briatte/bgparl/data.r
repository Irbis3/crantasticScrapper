# http://stackoverflow.com/a/6364905/635806

simpleCap <- function(x) {
  s = strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep = "", collapse = " ")
}

# bills

data = "data/bills.csv"
if (!file.exists(data)) {
  
  root = "http://www.parliament.bg"
  file = "raw/bill-lists/bills.html"
  
  if (!file.exists(file))
    download.file("http://www.parliament.bg/bg/bills", file, mode = "wb", quiet = TRUE)
  
  h = htmlParse(file, encoding = "UTF-8")
  h = xpathSApply(h, "//a[contains(@href, 'bills/period')]/@href")
  
  b = data.frame()
  k = c()
  for (i in h) {
    
    mth = gsub("(.*)(\\d{4})", "\\2", i)
    cat(str_pad(mth, 7, "right"), "... ")
    file = paste0("raw/bill-lists/bills-", mth, ".html")
    
    if (!file.exists(file))
      download.file(paste0(root, i), file, mode = "wb", quiet = TRUE)
    
    h = htmlParse(file)
    h = xpathSApply(h, "//a[contains(@href, 'bills/ID')]/@href")
    
    for (j in rev(h)) {
      
      #     cat(sprintf("%3.0f", which(h == j)))
      file = paste0("raw/bill-pages/bill-", gsub("\\D", "", j), ".html")
      
      if (!file.exists(file))
        download.file(paste0(root, j), file, mode = "wb", quiet = TRUE)
      
      hh = htmlParse(file)
      jj = xpathSApply(hh, "//ul[@class='frontList'][1]/li/a/@href")
      
      refs = xpathSApply(hh, "//td[@class='h1']/following-sibling::td", xmlValue)
      
      b = rbind(b, data_frame(
        uid = as.character(gsub("\\D", "", j)),
        ref = refs[2],
        date = as.Date(strptime(refs[3], "%d/%m/%Y")),
        session = refs[4],
        title = str_clean(refs[1]),
        authors = paste0(gsub("\\D", "", jj), collapse = ";"),
        committee = paste0(gsub("\\D", "",
                                xpathSApply(hh, "//td[@class='h1'][7]/following-sibling::td/ul/li/a/@href")),
                           collapse = ";")
      ))
      
      if (any(grepl("/MP", jj)))
        k = c(k, jj)
      
    }
    
    cat(sprintf("%2.0f", length(h)), "bill(s)\n")
    
  }
  
  b$authors[ b$authors == "" ] = "GOV"
  
  legislature = NA
  legislature[ is.na(legislature) & b$date < as.Date("2005-07-11") ] = "2001-2005" # l. 39
  legislature[ is.na(legislature) & b$date < as.Date("2009-07-14") ] = "2005-2009" # l. 40
  legislature[ is.na(legislature) & b$date < as.Date("2013-05-09") ] = "2009-2013" # l. 41
  legislature[ is.na(legislature) & b$date < as.Date("2014-10-05") ] = "2013-2014" # l. 42 (ended Aug.)
  legislature[ is.na(legislature) & b$date > as.Date("2014-10-04") ] = "2014-2018" # l. 43 (election 5 Oct.)
  table(legislature, b$authors != "GOV", exclude = NULL)
  
  write.csv(cbind(legislature, b), data, row.names = FALSE)

  # sponsors (none found in the bills for legislature 39, 2001-2005)
    
  data = "data/sponsors.csv"
  if (!file.exists(data)) {
    
    k = unique(k)
    s = data_frame()
    for (i in rev(k)) {

      cat(sprintf("%4.0f", which(k == i)), str_pad(i, 12, "right"))

      # Bulgarian (seniority)
      file = paste0("raw/mp-pages/mp-", gsub("\\D", "", i), "-bg.html")
      
      if (!file.exists(file))
        download.file(paste0(root, i), file, mode = "wb", quiet = TRUE)
      
      h = htmlParse(file)
      mandates = xpathSApply(h, "//div[@class='MPinfo']/ul/li[contains(text(), 'НС:')]", xmlValue)
      mandates = as.numeric(unlist(str_extract_all(mandates, "[0-9]+")))
      
      legislature = xpathSApply(h, "//a[contains(@href, '/bg/MP/members/')]", xmlValue)
      legislature = substr(legislature, 1, 2) %>% unique %>% as.integer
      stopifnot(mandates < legislature)

      if (!length(mandates))
        mandates = ""

      # English (rest of details)
      i = gsub("/bg/", "/en/", i)
      file = paste0("raw/mp-pages/mp-", gsub("\\D", "", i), ".html")
      
      if (!file.exists(file))
        download.file(paste0(root, i), file, mode = "wb", quiet = TRUE)
      
      h = htmlParse(file)
      
      nfo = xpathSApply(h, "//div[@class='MPinfo']/ul/li", xmlValue)
      born = nfo[ grepl("Date of birth|Дата на раждане", nfo) ]
      job = gsub("Profession|Професия|: |;$|N\\.A\\.", "", nfo[ grepl("Profession|Професия", nfo) ])
      
      s = rbind(s, data_frame(
        legislature,
        name = xpathSApply(h, "//img[contains(@src, 'Assembly')]/@alt"),
        born = str_extract(born, "[0-9]{4}"),
        born_bg = as.numeric(grepl("Bulgaria|България", born)), # born in Bulgaria (0/1)
        mandates = paste0(mandates, collapse = ";"),
        nyears = 4 * length(mandates[ mandates != "" ]),
        party = gsub("Political force: |Избран\\(а\\) с политическа сила: |;$", "",
                     nfo[ grepl("Political force|политическа сила", nfo) ]),
        constituency = gsub("Constituency: |Изборен район: |;$", "",
                            nfo[ grepl("Constituency|Изборен район", nfo) ]),
        job = ifelse(length(job), job, NA),
        url = gsub("/en/", "", i)
        # photo = xpathSApply(h, "//img[contains(@src, 'Assembly')]/@src")
      ))
      
      cat(":", tail(s, 1)$name, "\n")
      
    }
    
    write.csv(s, data, row.names = FALSE)
    
  }
  
}

m = read.csv("data/bills.csv", stringsAsFactors = FALSE)
m = filter(m, authors != "GOV")
m$n_au = 1 + str_count(m$authors, ";")

s = read.csv("data/sponsors.csv", stringsAsFactors = FALSE)

s$url = gsub("\\D", "", s$url)
s$photo = paste0("photos/", s$url, ".png") # all photos found

# download photos
for (i in unique(s$url)) {
  
  photo = paste0("photos/", i, ".png")
  if (!file.exists(photo))
    try(download.file(paste0("http://www.parliament.bg/images/Assembly/", i, ".png"),
                      photo, mode = "wb", quiet = TRUE), silent = TRUE)
  
  if (!file.info(photo)$size) {
    
    file.remove(photo)
    s$photo[ s$url == i ] = NA
    
  }
  
}

# ==============================================================================
# CHECK CONSTITUENCIES
# ==============================================================================

# convert constituencies to Wikipedia handles
s$constituency = gsub("\\d|-| (GRAD|OKRAG|OBLAST)", "", s$constituency)
s$constituency = sapply(tolower(s$constituency), simpleCap)
s$constituency = paste(s$constituency, "Province")
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

# name fixes (Google translations with first name checks)
s$name = str_clean(s$name)
s$name[ s$url == "171" ] = "KOSTADIN STOYANOV PASKALEV"
s$name[ s$url == "131" ] = "ATANAS PETROV ATANASOV"
s$name[ s$url == "57" ] = "IVO PŬRVANOV ATANASOV"
s$name[ s$url == "819" ] = "PETAR DIMITROV POPOV"
s$name[ s$url == "815" ] = "HRISTO KIRILOV POPOV"
s$name[ s$url == "24" ] = "BOYKO STEFANOV VELIKOV"
s$name[ s$url == "32" ] = "MIHAIL RAĬKOV MIKOV"
s$name[ s$url == "190" ] = "VOLER NIKOLOV SIDEROV"
s$name[ s$url == "1" ] = "LUBEN ANDONOV KORNEZOV"
s$name[ s$url == "79" ] = "ANGEL PETROV NAĬDENOV"
s$name[ s$url == "790" ] = "DENITSA IVAĬLOVA DIMITROVA"
s$name[ s$url == "792" ] = "ILKO DIMITROV DIMITROV"
s$name[ s$url == "209" ] = "STELA DIMITROVA ANGELOVA-BANKOVA"
s$name[ s$url == "114" ] = "ELEONORA NIKOLAEVA NIKOLOVA"

s$sex = NA
s$sex[ str_sub(s$name, -2) %in% c("EV", "OV") ] = "M"
s$sex[ str_sub(s$name, -2) == "VA" ] = "F"
s$sex[ grepl("^(A(K)?HMED|ANDREY|ANGEL|ARIF|ATANAS|AYDOAN|AYHAN|BELGIN|BORIS(LAV)?|BOYKO|BYUNYAMIN|DAUT|DELYAN|DESISLAVA|DIMCHO|DIMITAR|DOBROMIR|DURHAN|EMIL|ERDINCH|ERDZHAN|GEORGI|GYUNAY|GYUNER|HAMID|HASAN|HRISTO|IVAN|IVAYLO|JORDAN|JUNAL|KAMEN|KASIM|KIRIL|KRASIMIR|KRISTIAN|LYUBEN|LYUBOMIR|LYUTVI|MARIO|MEHMED|MIHAIL|MITHAT|MUSTAFA|NAYDEN|NEDZHMI|NESRIN|NEVIN|NIKOLA(Y)?|PAVEL|PETAR|PLAMEN|RADOSLAV|RAMADAN|REMZI|RU(M|P)EN|RUSHEN|SEMIR|SHABANALI|SHENDOAN|STANISLAV|STEFAN|STOYAN|TCHETIN|TODOR|TSVETAN|VALENTIN|VA(S)?SIL|VESELIN|VLADIMIR|YANKO|Y(O)?UNAL|YUKSEL|YUSEIN)\\s", s$name) ] = "M"
s$sex[ s$name == "REYHAH" ] = "M"
s$sex[ grepl("^(ANASTASIA|BOYKA|DANIELA|FATHME|GALINA|KRASIMIRA|KRASTANKA|ILIYA|IRENA|MARGARITA|MARIA(NA)?|NIGYAR|PETYA|SALIHA|TATYANA|TEODORA|TSETSKA|VANYA|VYARA)\\s", s$name) ] = "F"

table(s$sex, exclude = NULL)
stopifnot(!is.na(s$sex))

s$name = sapply(tolower(s$name), simpleCap)

s$party[ grepl("Ataka|Attack|Атака", s$party) ] = "A"
s$party[ grepl("Movement for Rights and Freedoms|Движение за права и свободи", s$party) ] = "DPS" 
s$party[ grepl("Coalition for Bulgaria|Коалиция за България|BSP Leftist Bulgaria", s$party) ] = "KB"
s$party[ grepl("GERB|ГЕРБ", s$party) ] = "GERB"
s$party[ grepl("Democrats for Strong Bulgaria|Демократи за Силна България", s$party) ] = "DSB"
s$party[ grepl("National Movement Simeon the Second|Национално движение Симеон Втори", s$party) ] = "NMS"
s$party[ grepl("Order, Lawfulness Justice|Ред, законност и справедливост", s$party) ] = "RZS"
s$party[ grepl("Blue Coalition|Синята коалиция", s$party) ] = "SK"
s$party[ grepl("United Democratic Forces|Обединени Демократични Сили", s$party) ] = "ODS"
s$party[ grepl("Bulgarian People's Union|Български Народен Съюз", s$party) ] = "BNS"
s$party[ grepl("BULGARIA WITHOUT CENSORSHIP|България без цензура", s$party) ] = "BBZ" # 2014 election
s$party[ grepl("REFORMIST BLOC|Реформаторски блок", s$party) ] = "RB" # 2014 election
s$party[ grepl("PATRIOTIC FRONT|Патриотичен фронт", s$party) ] = "RB" # 2014 election

# English party names
table(s$party, exclude = NULL)
stopifnot(!is.na(groups[ s$party ]))

# all sponsors recognized
stopifnot(all(unique(unlist(strsplit(m$authors, ";"))) %in% s$url))

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
# stopifnot(!is.na(s$url) & grepl("^http(s)?://(.*)", s$url)) # used as uids
stopifnot(s$party %in% names(colors))
