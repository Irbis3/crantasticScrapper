bills = "data/bills.csv"
sponsors = "data/sponsors.csv"

# scrape bills

if (!file.exists(bills)) {

  root = "http://www3.lrs.lt/pls/inter3/"
  b = data_frame()

  # avoid hitting search engine limit at 10,000 documents
  for (y in 2015:1994) {

    h = htmlParse(paste0("http://www3.lrs.lt/pls/inter3/dokpaieska.rezult_l?p_nr=&p_nuo=", y, "-01-01&p_iki=", y, "-12-31&p_org=1&p_drus=2&p_kalb_id=1&p_title=&p_text=&p_pub=&p_met=&p_lnr=&p_denr=&p_es=0&p_tid=&p_tkid=&p_t=0&p_tr1=2&p_tr2=2&p_gal=&p_rus=1"))

    p = unique(xpathSApply(h, "//a[contains(@href, 'dokpaieska.rezult') and contains(@title, 'Į galą')]/@href"))
    n = gsub("(.*)&p_no=(\\d+)", "\\2", p) # last page
    p = gsub("(.*)&p_no=(\\d+)", "\\1", p) # root with session

    for (i in as.numeric(n):1) {

      cat(y, sprintf("%3.0f", i))
      file = paste0("raw/bill-lists/bills-", y, "-", i, ".html")

      if (!file.exists(file))
        try(download.file(paste0(root, p, "&p_no=", i), file, quiet = TRUE, mode = "wb"))

      if (!file.info(file)$size) {

        cat(": failed\n")
        file.remove(file)

      } else {

        h = htmlParse(file)

        # bill URLs
        urls = xpathSApply(h, "//table[@class='basicnoborder']/tr//a[contains(@href, 'dokpaieska')][1]/@href")
        uids = gsub("(.*)p_id=(\\d+)&(.*)", "\\2", urls)
        text = xpathSApply(h, "//table[@class='basicnoborder']/tr/td[2]", xmlValue)
        date = xpathSApply(h, "//table[@class='basicnoborder']/tr/td[3]", xmlValue)

        text = lapply(text, function(x) {
          x = unlist(strsplit(x, "\\n"))
          data_frame(text = x[1], authors = x[2])
        })
        text = bind_rows(text)

        # cosponsor URLs
        co  = xpathSApply(h, "//table[@class='basicnoborder']/tr//a[contains(@href, 'p_daug')]/@href")
        coid = gsub("(.*)p_id=(\\d+)&(.*)", "\\2", co)

        cosponsors = rep(NA, length(uids))
        cosponsors[ uids %in% coid ] = co

        for (j in co) {

          cat(".")
          file = paste0("raw/bill-pages/bill-", coid[ which(co == j) ], ".html")

          if (!file.exists(file))
            try(download.file(paste0(root, j), file, quiet = TRUE, mode = "wb"))

          if (!file.info(file)$size) {

            cat(": failed (details)\n")
            file.remove(file)

          } else {

            h = htmlParse(file)
            h = xpathSApply(h, "//ul/li", xmlValue)
            h = gsub("(.*)(Seimas|frakcija), |Darbo grupė, ", "", h)
            h = h[ !grepl("(komisija|komitetas|Seimas)$", h) &
                     !h %in% c("Darbo grupė", "Mišri Seimo narių grupė",
                               "2014-01-14 Pateikė - Darbo grupė") ]

            stopifnot(!grepl("Darbo", h))

            if (length(h))
              cosponsors[ cosponsors == j ] = paste0(h, collapse = ", ")
            else
              cosponsors[ cosponsors == j ] = NA

          }

        }

        b = rbind(b, cbind(
          data_frame(year = y, uids = as.character(uids), urls = as.character(urls), date),
          text, cosponsors,
          stringsAsFactors = FALSE
        ))
        cat("\n")

      }

    }

  }

  # clean up

  # standardize commas
  b$authors = gsub("\\s?(,|;)\\s?", ", ", b$authors)
  # superfluous text, including bill references
  b$authors = gsub("(Parengė|Pateikė|Priėmė|Tvirtino):\\s|Seimo nar(ė|ys)\\s|,\\s>>$", "", b$authors)
  b$authors = gsub("Projektas\\s(.*?)-\\d{2}|Sveikatos|Švietimo|\\((.*)\\)", "", b$authors)
  # committees and party factions as sponsors
  b$authors = gsub("(\\w|\\s|-)+\\s((pa)?komi(sija|tet(a|i)s)|(delegac|frakc|minister)ija)", "", b$authors)
  b$authors = gsub("(Darbo|(Mišri\\s)?Seimo\\snarių)\\sgrupė|Frakcija \"(.*?)\"", "", b$authors)
  # institutional sponsors
  b$authors = gsub("Seimo\\sPirmininko(.*?)skubos|Klaipėdos\\smiesto\\svaldyba|STT\\sišvada", "", b$authors)
  b$authors = gsub("Lietuvos\\sRespublikos\\s(Prezidentas|Seimas|Vyriausybė)", "", b$authors)
  b$authors = gsub("Lietuvos\\sRespublikos\\s(.*?)\\s(departamentas|(komis|minister)ija)(,\\s)?", "",
                   b$authors)
  # various bits of text
  b$authors = gsub("soc\\.reik\\.|\\sraštas\\sdėl\\sparašo\\satšaukimo(\\.pdf)?", "", b$authors)
  b$authors = gsub("Nr\\.\\s(.*?)pakeitimo\\s|VU\\sSenato(.*?)išrašas\\s|ĮSTATYMO\\sPROJEKTAS", "",
                   b$authors)
  # misspelt sponsor names
  b$authors = gsub("V.Būtėnas", "V.Butėnas", b$authors)
  # remove extra commas and spaces
  b$authors = sapply(b$authors, function(x) {
    x = strsplit(x, ",\\s?") %>% unlist
    paste0(x[ nchar(x) > 2 ], collapse = ", ")
  })

  # cosponsors
  b$cosponsors = gsub("Švietimo|(\\w|\\s|-)+\\skomi(sija|tetas)", "", b$cosponsors)
  b$cosponsors = gsub("(Darbo|(Mišri\\s)?Seimo\\snarių)\\sgrupė|Frakcija \"(.*?)\"", "", b$cosponsors)
  # misspelt sponsor names
  b$cosponsors = gsub("Vladas\\sBūtėnas", "Vladas Butėnas", b$cosponsors)
  b$cosponsors = gsub("Vladas\\sBūtėnas", "Vladas Butėnas", b$cosponsors)
  # remove extra commas and spaces
  b$cosponsors = sapply(b$cosponsors, function(x) {
    x = strsplit(x, ",\\s?") %>% unlist
    paste0(x[ nchar(x) > 2 ], collapse = ", ")
  })

  b$legislature = NA
  b$legislature[ as.Date(b$date) > as.Date("2012-10-14") ] = "2012-2016" # l. 11
  b$legislature[ is.na(b$legislature) & as.Date(b$date) > as.Date("2008-10-12") ] = "2008-2012" # l. 10
  b$legislature[ is.na(b$legislature) & as.Date(b$date) > as.Date("2004-10-10") ] = "2004-2008" # l. 9
  b$legislature[ is.na(b$legislature) & as.Date(b$date) > as.Date("2000-08-10") ] = "2000-2004" # l. 8
  b$legislature[ is.na(b$legislature) & as.Date(b$date) > as.Date("1996-10-20") ] = "1996-2000" # l. 7
  b$legislature[ is.na(b$legislature) & as.Date(b$date) > as.Date("1992-10-25") ] = "1992-1996" # l. 6
  table(b$legislature, exclude = NULL)

  write.csv(b, bills, row.names = FALSE)

}

b = read.csv(bills, stringsAsFactors = FALSE)

# scrape sponsors by going through historical pages
# index: http://www3.lrs.lt/pls/inter/w5_show?p_r=3804&p_k=1

# note: sponsors need to be uniquely named x.y where x is their first initial
# and y is is their family name; this is how they are named in the bills data
# when they appear as first author of a bill

if (!file.exists(sponsors)) {

  cat("Getting sponsors from 1992-1996 (l. 6)\n")
  f = "raw/mp-lists/mps-6.html"
  if (!file.exists(f))
    download.file("http://www3.lrs.lt/docs3/kad2/w5_lrs.seimo_nariu_sarasas-p_kade_id=2&p_kalb_id=1&p_int_tv_id=784.htm", f, mode = "wb", quiet = TRUE)

  h = htmlParse(f)

  u = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]/@href")
  n = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]", xmlValue)
  f = sapply(n, function(x) {
    x = unlist(strsplit(x, " "))
    paste0(x[ grepl("[A-Z]{2,}", x) ], collapse = " ")
  })
  n = str_replace(n, paste0(" ", f), "")

  d = data_frame(legislature = "1992-1996", url = u, name = paste(n, f),
                 id = paste0(substr(n, 1, 1), ".", f))

  cat("Getting sponsors from 1996-2000 (l. 7)\n")
  f = "raw/mp-lists/mps-7.html"
  if (!file.exists(f))
    download.file("http://www3.lrs.lt/seimu_istorija/w3_lrs.seimo_nariu_sarasas-p_kade_id=3&p_kalb_id=1&p_int_tv_id=784.htm", f, mode = "wb", quiet = TRUE)

  h = htmlParse(f)

  u = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]/@href")
  n = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]", xmlValue)
  f = sapply(n, function(x) {
    x = unlist(strsplit(x, " "))
    paste0(x[ grepl("[A-Z]{2,}", x) ], collapse = " ")
  })
  n = str_replace(n, paste0(" ", f), "")

  d = rbind(d, data_frame(legislature = "1996-2000", url = u, name = paste(n, f),
                          id = paste0(substr(n, 1, 1), ".", f)))

  cat("Getting sponsors from 2000-2004 (l. 8)\n")
  f = "raw/mp-lists/mps-8.html"
  if (!file.exists(f))
    download.file("http://www3.lrs.lt/docs3/kad4/w3_lrs.seimo_nariu_sarasas-p_kade_id=4&p_kalb_id=1&p_int_tv_id=784.htm", f, mode = "wb", quiet = TRUE)

  h = htmlParse(f)

  u = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]/@href")
  n = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]", xmlValue)
  f = sapply(n, function(x) {
    x = unlist(strsplit(x, " "))
    paste0(x[ grepl("[A-Z]{2,}", x) ], collapse = " ")
  })
  n = str_replace(n, paste0(" ", f), "")

  d = rbind(d, data_frame(legislature = "2000-2004", url = u, name = paste(n, f),
                          id = paste0(substr(n, 1, 1), ".", f)))

  cat("Getting sponsors from 2004-2008 (l. 9)\n")
  f = "raw/mp-lists/mps-9.html"
  if (!file.exists(f))
    download.file("http://www3.lrs.lt/docs3/kad5/w5_istorija.show5-p_r=786&p_k=1.html", f, mode = "wb", quiet = TRUE)

  h = htmlParse(f)

  u = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]/@href")
  n = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]", xmlValue)
  f = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]/strong", xmlValue)
  n = str_replace(n, f, "")

  d = rbind(d, data_frame(legislature = "2004-2008", url = u, name = paste(n, f),
                          id = paste0(substr(n, 1, 1), ".", f)))

  cat("Getting sponsors from 2008-2012 (l. 10)\n")
  f = "raw/mp-lists/mps-10.html"
  if (!file.exists(f))
    download.file("http://www3.lrs.lt/docs3/kad6/w6_istorija.show6-p_r=6113&p_k=1.html", f, mode = "wb", quiet = TRUE)

  h = htmlParse(f)

  u = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]/@href")
  n = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]", xmlValue)
  f = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]/strong", xmlValue)
  n = str_replace(n, f, "")

  d = rbind(d, data_frame(legislature = "2008-2012", url = u, name = paste(n, f),
                          id = paste0(substr(n, 1, 1), ".", f)))

  cat("Getting sponsors from 2012-2016 (l. 11)\n")
  f = "raw/mp-lists/mps-11.html"
  if (!file.exists(f))
    download.file("http://www3.lrs.lt/pls/inter/w5_show?p_r=8801&p_k=1", f, mode = "wb", quiet = TRUE)

  h = htmlParse(f)

  u = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]/@href")
  n = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]", xmlValue)
  f = sapply(n, function(x) {
    x = unlist(strsplit(x, " "))
    paste0(x[ grepl("[A-Z]{2,}", x) ], collapse = " ")
  })
  n = str_replace(n, paste0(" ", f), "")

  d = rbind(d, data_frame(legislature = "2012-2016", url = u, name = paste(n, f),
                          id = paste0(substr(n, 1, 1), ".", f)))

  d = unique(d)

  # ensure UIDs are unique
  rownames(d) = paste0(d$legislature, str_replace(d$url, "(.*)p_asm_id=(\\d+)(.*)?", "-\\2"))

  d$url[ d$legislature == "2012-2016" ] = paste0(
    "http://www3.lrs.lt/pls/inter/", d$url[ d$legislature == "2012-2016" ])
  d$url[ d$legislature == "2008-2012" ] = paste0(
    "http://www3.lrs.lt/docs3/kad6/", d$url[ d$legislature == "2008-2012" ])
  d$url[ d$legislature == "2004-2008" ] = paste0(
    "http://www3.lrs.lt/docs3/kad5/", d$url[ d$legislature == "2004-2008" ])
  d$url[ d$legislature == "2000-2004" ] = paste0(
    "http://www3.lrs.lt/docs3/kad4/", d$url[ d$legislature == "2000-2004" ])
  d$url[ d$legislature == "1996-2000" ] = paste0(
    "http://www3.lrs.lt/seimu_istorija/", d$url[ d$legislature == "1996-2000" ])
  d$url[ d$legislature == "1992-1996" ] = paste0(
    "http://www3.lrs.lt/docs3/kad2/", d$url[ d$legislature == "1992-1996" ])
  stopifnot(!duplicated(d$url))

  # parse sponsors

  s = data_frame()

  for (i in nrow(d):1) {

    cat(sprintf("%4.0f", i))
    file = paste0("raw/mp-pages/mp-", rownames(d)[ i ], ".html")

    if (!file.exists(file))
      try(download.file(d$url[ i ], file, quiet = TRUE, mode = "wb"), silent = TRUE)

    if (!file.exists(file) || !file.info(file)$size) {

      cat(": failed", d$url[ i ], "\n")

      if (file.exists(file)) {
        file.remove(file)
      }

    } else {

      cat(":", file)

      h = htmlParse(readLines(file, warn = FALSE), asText = TRUE)
      name = xpathSApply(h, "//meta[@name='description']/@content")

      if (is.null(name)) {

        # scraper for 1992-1996 to 2000-2004
        name = xpathSApply(h, "//h2", xmlValue)
        name = ifelse(!length(name), xpathSApply(h, "//h3", xmlValue), name) # 1992-1996
        name = gsub("\\n1992-1996 m. kadencijos Seimo nar(ys|ė) ", "", name)

        details = xpathSApply(h, "//b[contains(text(), 'Seimo nar')]/..", xmlValue)
        sex = ifelse(grepl("Seimo narė", details[ grepl("^Seimo", details) ]), "F", "M")
        stopifnot(sex %in% c("F", "M"))

        born = xpathSApply(h, "//p[contains(text(), 'Gimė') or contains(text(), 'gimė')]/..", xmlValue) %>%
          str_extract("(G|g)imė\\s(-\\s)?([Ol0-9]+)")
        born = gsub("(G|g)imė\\s(-\\s)?", "", born) %>% gsub("l", "1", .) %>% gsub("O", "0", .)

        if (!length(born)) {

          born = xpathSApply(h, "//div[contains(text(), 'Gimė')]/..", xmlValue)
          born = gsub("(.*)Gimė\\s(\\d{4})(.*)", "\\2", born)

        }

        constituency = gsub("Išrinkta(s)?\\s+|\\s\\((.*)", "",
                            str_extract(details[ grepl("Išrinkta", details) ],
                                        "Išrinkta(s)?\\s(.*?)(,|\\d+)"))

        party = gsub("iškėlė\\s|\\sBiuro(.*)", "",
                     str_extract(details[ grepl("iškėlė\\s|\\sBiuro(.*)", details) ],
                                 "iškėlė\\s(.*)"))

        photo = xpathSApply(h, "//img[contains(@tppabs, 'seimo_nariu')]/@tppabs") %>%
          as.character

        s = rbind(s, data_frame(file, name, sex,
                                born = ifelse(!length(born), NA, as.integer(born)),
                                constituency = ifelse(!length(constituency), NA, constituency),
                                party = ifelse(!length(party), NA, party),
                                mandates = NA,
                                photo = ifelse(!length(photo), NA, photo),
                                url = d$url[ i ]))

      } else {

        # scraper for 2004-2008, 2008-2012 and 2012-2016
        details = xpathSApply(h, "//table[@class='MsoTableGrid']//td", xmlValue)

        if (!length(details)) {
          details = xpathSApply(h, "//td[@width='410']/..", xmlValue)
        }

        if (length(details)) {

          details = str_clean(details)
          if (any(grepl("^Gimimo data(:)?$", details))) {

            born = details[ which(grepl("Gimimo data", details)) + 1 ]
            born = ifelse(length(born), as.integer(substr(born, 1, 4)), NA)

          } else {

            born = details[ which(grepl("Gimimo data", details)) ]
            born = ifelse(length(born), as.integer(str_extract(born, "\\d{4}")), NA)

          }

        } else {

          born = xpathSApply(h, "//p[contains(text(), 'Gimė')]", xmlValue)
          born = ifelse(length(born), born, xpathSApply(h, "//p[contains(text(), 'Gimė')]", xmlValue))
          born = ifelse(length(born), str_extract(born, "\\d{4}") %>% as.integer, NA) # many missing

        }

        if (is.na(born)) {

          born = xpathSApply(h, "//div[contains(text(), 'Gimė')]/..", xmlValue)
          born = gsub("(.*)Gimė\\s(\\d{4})(.*)", "\\2", born)
          born = ifelse(length(born), as.integer(born), NA)

        }

        details = xpathSApply(h, "//b[contains(text(), 'Seimo nar')]/..", xmlValue)
        sex = ifelse(grepl("Seimo\\snarė", details), "F", "M")

        constituency = gsub("Išrinkta(s)?\\s+|\\s\\((.*)", "",
                            str_extract(details, "Išrinkta(s)?\\s(.*?)(,|\\d+)"))

        party = gsub("iškėlė\\s|\\sBiuro(.*)", "",
                     str_extract(details, "iškėlė\\s(.*)"))

        if (is.na(party)) {

          party = xpathSApply(h, "//b[contains(text(), 'Seimo frakcijose')]/following-sibling::ul[1]/li/a", xmlValue)
          party = unique(party)
          party = ifelse(length(party) > 1, NA, party) # four missing, fixed later

        }

        photo = xpathSApply(h, "//img[contains(@src, 'seimo_nariu')]/@src")

        mdts = xpathSApply(h, "//table[@summary='Istorija']//a/..", xmlValue)

        s = rbind(s, data_frame(file, name, sex, born, constituency, party,
                                mandates = ifelse(length(mdts), mdts, NA),
                                photo, url = d$url[ i ]))

      }

      cat(":", name, "\n")

    }

  }

  s = merge(d[, c("legislature", "url", "id") ], s, by = "url")

  s$party = str_trim(s$party)
  s$party[ grepl("Išsikėlė pats|Mišri Seimo narių grupė", s$party) ] = "IND" # or mixed group
  s$party[ grepl("(V|v)alstiečių", s$party) ] = "LVZS" # ex-LVLS + Naujosios demokratijos partija, NDP
  s$party[ grepl("Tvarka|Pakso|Liberalų demokratų partija", s$party) ] = "TT"
  s$party[ grepl("Liberalų ir centro", s$party) ] = "LICS"
  s$party[ grepl("Tautos", s$party) ] = "TPP"
  s$party[ grepl("liberalų sąjūdis|Liberalų  sąjūdžio", s$party) ] = "LS" # LRLS
  s$party[ grepl("liberalų sąjunga", s$party) ] = "LLIS"
  s$party[ grepl("^Sąjūdžio koalicija|^Lietuvos sąjūdis", s$party) ] = "SK"
  s$party[ grepl("^Tėvynės", s$party) ] = "TS-LKD" # TS-LK + LKDP after 2008
  s$party[ grepl("Lietuvos Krikščionių demokratų partijos", s$party) ] = "LKDP"
  s$party[ grepl("socialdemokratų", s$party) ] = "LSDP"
  s$party[ grepl("Lietuvos demokratinė darbo", s$party) ] = "LDDP"
  s$party[ grepl("lenkų", s$party) ] = "LLRA"
  s$party[ grepl("Darbo", s$party) ] = "DP"
  s$party[ grepl("Drąsos", s$party) ] = "DK"
  s$party[ grepl("socialliberalai|Brazausko", s$party) ] = "NS" # + Brazauskas/Paulauskas coalition
  s$party[ grepl("(K|k)rikščionių demokratų (parti|sąjunga)", s$party) ] = "KDS"

  # n = 1, 2000
  s$party[ grepl("Lietuvos laisvės sąjunga", s$party) ] = "LLAS"

  # n = 1, 2000
  s$party[ grepl("Nuosaikiųjų konservatorių sąjunga", s$party) ] = "NKS"

  # n = 1, 1996, and n = 2, 2000
  s$party[ grepl("^Naujosios demokratijos partija", s$party) ] = "NDP"

  # n = 2, 1992, n = 14, 1996, and n = 2, 2000
  s$party[ grepl("^Lietuvos centro ", s$party) ] = "LCS"

  # various nationalist coalitions or single candidates
  # n = 2, 1992, and n = 1, 1996
  s$party[ grepl("^Lietuvos politinių kalinių", s$party) ] = "LPKTS"
  # n = 1 in 1992, 1996, 2000
  s$party[ grepl("Jaunoji Lietuva|Jaunosios Lietuvos", s$party) ] = "JL"
  # n = 3, 1992, and n = 2, 1996
  s$party[ grepl("tautininkų sąjunga|tautininkų ir Lietuvos demokratų", s$party) ] = "LTS"

  # missing values
  s$party[ s$legislature == "2008-2012" & s$id == "M.VARAŠKA" ] = "IND"  # Mantas VARAŠKA
  s$party[ s$legislature == "2008-2012" & s$id == "V.VALKIŪNAS" ] = "IND"  # Valdemaras VALKIŪNAS
  s$party[ s$legislature == "2008-2012" & s$id == "J.STANEVIČIUS" ] = "IND"  # Jonas STANEVIČIUS
  s$party[ s$legislature == "2008-2012" & s$id == "A.BURBA" ] = "LICS" # Andrius BURBA
  s$party[ s$id == "V.EINORIS" ] = "LDDP" # Vytautas EINORIS (WP-ET)
  s$party[ s$legislature == "2000-2004" & s$id == "V.EINORIS" ] = "LSDP" # Vytautas EINORIS (WP-ET)
  s$party[ s$id == "K.SKREBYS" ] = "IND" # Kęstutis SKREBYS (single Ind. party member)
  s$party[ s$id == "N.AMBRAZAITYTĖ" ] = "IND" # Nijolė AMBRAZAITYTĖ (single LRPCH party member)
  s$party[ s$legislature == "1992-1996" & s$id == "T.LIDEIKIS" ] = "SK" # Tautvydas LIDEIKIS
  s$party[ s$legislature == "1992-1996" & s$id == "J.BASTYS" ] = "LDDP" # Juozas BASTYS
  s$party[ s$legislature == "1992-1996" & s$id == "J.BULAVAS" ] = "LDDP" # Juozas BULAVAS
  s$party[ s$legislature == "1992-1996" & s$id == "J.PALECKIS" ] = "LDDP" # Justas Vincas PALECKIS
  s$party[ s$legislature == "2008-2012" & s$id == "J.KONDROTAS" ] = "DP" # Jonas KONDROTAS
  s$party[ s$legislature == "1996-2000" & s$id == "A.BUTKEVIČIUS" ] = "TT"
  s$party[ s$legislature == "1996-2000" & s$id == "A.MEDALINSKAS" ] = "IND" # mixed
  s$party[ s$legislature == "2008-2012" & s$id == "A.BURBA" ] = "LCS"
  s$party[ s$legislature == "1996-2000" & s$id == "D.PAUKŠTĖ" ] = "LSDP"
  s$party[ s$legislature == "2008-2012" & s$id == "J.STANEVIČIUS" ] = "IND" # mixed TTP/KP
  s$party[ s$legislature == "2000-2004" & s$id == "J.VESELKA" ] = "IND"
  s$party[ s$legislature == "2004-2008" & s$id == "J.VESELKA" ] = "IND" # mixed
  s$party[ s$legislature == "2000-2004" & s$id == "K.GLAVECKAS" ] = "LICS"
  s$party[ s$legislature == "2004-2008" & s$id == "P.GRAŽULIS" ] = "IND" # mixed
  s$party[ s$legislature == "2008-2012" & s$id == "N.PUTEIKIS" ] = "TS-LKD"
  s$party[ s$legislature == "1996-2000" & s$id == "R.KARBAUSKIS" ] = "IND" # Jungtine f.
  s$party[ s$legislature == "2000-2004" & s$id == "V.USPASKICH" ] = "NS"
  s$party[ s$legislature == "1996-2000" & s$id == "V.MARTIŠAUSKAS" ] = "IND" # mixed
  s$party[ s$legislature == "1996-2000" & s$id == "V.ŠMIGELSKAS" ] = "LCS"
  s$party[ s$legislature == "1996-2000" & s$id == "V.VELIKONIS" ] = "LDDP"
  s$party[ s$legislature == "1992-1996" & s$id == "A.VAIŽMUŽIS" ] = "IND"

  # one big nationwide constituency, plus many small SMDs
  # many constituencies are missing, as with birth years
  s$constituency = gsub("\\s-(.*)|,$", "", s$constituency)

  # missing values (proportional list members)
  s$constituency[ is.na(s$constituency) ] = "pagal sąrašą"

  s$mandates = gsub("\u0097", "-", s$mandates)
  s$mandates = gsub("Buvo išrinkta(s)? į ", "", s$mandates)
  s$mandates = gsub(" Seimą.", ";", s$mandates)
  s$mandates = gsub("(\\sAukščiausiąją\\sTarybą\\s-\\sAtkuriamąjį)?;$", "", s$mandates)

  # concatenate current legislature and mandates found in pages of 2012-2016 sponsors
  s = group_by(s, legislature, name) %>%
    summarise(mandates = paste0(na.omit(c(legislature, mandates)), collapse = ";")) %>%
    left_join(select(s, -mandates), by = c("legislature", "name"))

  # aggregate all mandates by sponsor (includes duplicates and future ones)
  s = arrange(s, legislature, name) %>%
    group_by(name) %>%
    mutate(mandates = paste0(mandates, collapse = ";"))

  # convert to years (will remove duplicates but still include fuure years)
  # note: years before 1992 (reconstituent parliament) are excluded to avoid the
  # situation where only a few MPs from 2012-2016 get seniority measures while
  # all others from the earliest legislature (1992-1996) get none
  s$mandates = sapply(s$mandates, function(x) {
    x = unlist(strsplit(x, ";")) # each element is a term
    x = lapply(x, function(y) {
      y = unlist(strsplit(y, "-")) # each element is a year
      y = as.numeric(y)
      seq(min(y), max(y)) # could also be y[1], y[2]
    })
    x = sort(unique(unlist(x)))
    paste0(x[ x >= 1992 & x <= 2015 ], collapse = ";") # left and right-censored
  })

  write.csv(s, sponsors, row.names = FALSE)

}

s = read.csv(sponsors, stringsAsFactors = FALSE)

# sole duplicate MP case, both MPs have same party, sex and seniority
s$id[ s$legislature == "1996-2000" & s$name == "Audrius BUTKEVIČIUS" ] = "Au.BUTKEVIČIUS"

# ensure first authors are unique
stopifnot(!duplicated(paste0(s$legislature, "-", s$id)))

# last fixes to sponsor names
s$name[ s$name == "Viktorija ČMILYTĖ NIELSEN" ] = "Viktorija ČMILYTĖ-NIELSEN"

# photos, all .jpg -- rerun entire script to solve network errors
for (i in rev(unique(s$photo %>% na.omit))) {

  #cat(sprintf("%4.0f", which(unique(s$photo) == i)))
  file = gsub("http://www(3)?.lrs.lt/(n|home)/seimo_nariu_nuotraukos/(\\d+)(-\\d{4})?/", "photos/\\3_", i)

  if (!file.exists(file))
    try(download.file(i, file, mode = "wb", quiet = TRUE))

  if (!file.info(file)$size) {

    #cat(": failed\n")
    file.remove(file)
    s$photo[ s$photo == i ] = NA

  } else {

    #cat("\n")
    s$photo[ s$photo == i ] = file

  }

}

s$constituency = str_trim(s$constituency)

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
