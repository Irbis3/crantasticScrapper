sponsors = "data/sponsors.csv"
bills = "data/bills.csv"

# MP bills (slow, over 100,000 JSON files to parse)
if (!file.exists(bills)) {
  
  m = data_frame()
  years = c("2014-2017", "2010-2013", "2006-2009", "2002-2005", "1998-2001", "1990-1997", "1980-1989")
  
  for (i in years) {
    
    f = paste0("data/mot-", i, ".json.zip")
    
    if (!file.exists(f))
      download.file(paste0("http://data.riksdagen.se/dataset/dokument/mot-", i, ".json.zip"),
                    f, mode = "wb")
    
    unzip(f, exdir = "raw")
    f = list.files("raw", pattern = "json$", full.names = TRUE)

    cat(i, ": parsing", sprintf("%6.0f", length(f)), "bills...")
    for (j in f) {
      
      d = fromJSON(readLines(j, warn = FALSE), flatten = TRUE)$dokumentstatus
      
      m = rbind(m, data_frame(
        uid = d$dokument$hangar_id,
        doc = d$dokument$dok_id,
        date = as.Date(d$dokument$datum),
        authors = paste0(d$dokintressent$intressent$intressent_id %>% unique,
                         collapse = ";")
      ))
      
    }
    
    cat("", sprintf("%6.0f", nrow(m)), "total bills\n")
    file.remove(f) # save disk space (1.5+ GB files in total)
    
  }
  
  write.csv(m, bills, row.names = FALSE)
  
}

m = read.csv(bills, stringsAsFactors = FALSE)
m = filter(m, authors != "")
m$n_au = 1 + str_count(m$authors, ";")

table(substr(m$doc, 1, 2)) # session years

# parliamentary sessions, 1988-2018 (years before/after excluded from dataset)
# see <http://data.riksdagen.se/sv/sa-funkar-dokument-id>
m$legislature = substr(m$doc, 1, 2)
# Sep 1988: 88/9, 89/0, 90/1
m$legislature[ m$legislature %in% c("GC", "GD", "GE") ] = "1988-1991"
# Sep 1991: 91/2, 92/3, 93/4
m$legislature[ m$legislature %in% c("GF", "GG", "GH") ] = "1991-1994"
# Sep 1994: 94/5, 95/6, 96/7, 97/8
m$legislature[ m$legislature %in% c("GI", "GJ", "GK", "GL") ] = "1994-1998"
# Sep 1998: 98/9, 99/0, 00/1, 01/2
m$legislature[ m$legislature %in% c("GM", "GN", "GO", "GP") ] = "1998-2002"
# Sep 2002: 02/3, 03/4, 04/5, 05/6
m$legislature[ m$legislature %in% c("GQ", "GR", "GS", "GT") ] = "2002-2006"
# Sep 2006: 06/7, 07/8, 08/9, 09/0
m$legislature[ m$legislature %in% c("GU", "GV", "GW", "GX") ] = "2006-2010"
# Sep 2010: 10/1, 11/2, 12/3, 13/4
m$legislature[ m$legislature %in% c("GY", "GZ", "H0", "H1") ] = "2010-2014"
# Sep 2014: 14/5, 15/6, 16/7, 17/8
m$legislature[ m$legislature %in% c("H2", "H3", "H4", "H5") ] = "2014-2018"

table(m$legislature, exclude = NULL)
m = filter(m, nchar(legislature) == 9)

r = unlist(strsplit(m$authors, ";"))

cat("Found", nrow(m), "bills",
    sum(m$n_au > 1), "cosponsored",
    n_distinct(r), "sponsors\n")

# sponsors

if (!file.exists(sponsors)) {
  
  s = data_frame() # initialize
  
} else {
  
  s = read.csv(sponsors, stringsAsFactors = FALSE)
  s = subset(s, grepl("\\d", url)) # avoid scraper bug
  
  r = r[ !r %in% gsub("\\D", "", s$url) ] # only new sponsors
  
}

# empty pages
r = unique(r[ !r %in% c("0584306280501", "0338066970018", "0844235199717", "0000000000000") ])

if (length(r)) {
  
  cat("Scraping", length(r), "missing sponsor(s)\n")
  
  for (i in rev(r)) {
    
    cat(sprintf("%4.0f", which(r == i)), str_pad(i, 14, "right"))
    
    f = paste0("raw/mp-pages/mp-", i, ".xml")
    if (!file.exists(f))
      try(download.file(paste0("http://data.riksdagen.se/personlista/?iid=", i),
                        f, quiet = TRUE, mode = "wb"), silent = TRUE)
    
    if (!file.info(f)$size) {
      
      cat(": failed\n")
      file.remove(f)
      
    } else {
      
      h = xmlParse(f)
      
      name = paste(xpathSApply(h, "//tilltalsnamn", xmlValue), xpathSApply(h, "//efternamn", xmlValue))
      
      from = substr(xpathSApply(h, "//uppdrag[organ_kod='kam']/from", xmlValue), 1, 4)
      to = substr(xpathSApply(h, "//uppdrag[organ_kod='kam']/tom", xmlValue), 1, 4)
      mdts = apply(cbind(from, to), 1, paste0, collapse = "-")
      mdts = lapply(mdts, function(x) {
        y = as.numeric(unlist(strsplit(x, "-")))
        seq(y[1], y[2])
      })
      mdts = paste0(sort(unique(unlist(mdts))), collapse = ";")

      job = xpathSApply(h, "//uppgift[kod='en' and typ='titlar']/uppgift", xmlValue)
      
      if (length(name)) {
        
        s = rbind(s, data_frame(
          name,
          born = xpathSApply(h, "//fodd_ar", xmlValue),
          sex = xpathSApply(h, "//kon", xmlValue),
          party = xpathSApply(h, "//parti", xmlValue),
          county = xpathSApply(h, "//valkrets", xmlValue),
          status = xpathSApply(h, "//status[1]", xmlValue),
          mandate = mdts,
          job = ifelse(is.null(job), NA, job),
          url = paste0("http://data.riksdagen.se/personlista/?iid=", i, "&utformat=html"),
          photo = xpathSApply(h, "//bild_url_80", xmlValue)
        ))
        
        cat(":", tail(s, 1)$name, "\n")
        
      } else {
        
        cat(": empty\n")
        
      }
      
    }
    
  }
  
  s$job[ s$job == "" ] = NA
  
  write.csv(s, sponsors, row.names = FALSE)

}

s = read.csv(sponsors, stringsAsFactors = FALSE)

s$url = gsub("\\D", "", s$url)

# download photos (eliminating known fails for speed)
for (i in unique(s$url[ !s$url %in% 
                        c("0316531680905", "0957300271615", "0685433188501", "0623459368604", 
                          "0707426886411", "0261479148307", "0778776381700", "0898090064906", 
                          "0308563888104", "0255522895108", "0362315104401", "0918472842401", 
                          "0968100864203", "0161742347007", "0675602051200", "0908744384705", 
                          "0740062037201", "0836242026100", "0316358669112", "0934740593109", 
                          "0844198084908", "094134653807", "0209004049600", "0931691314703", 
                          "049679140507", "076794009304", "0320679739714", "0648066719018", 
                          "078688254917", "0122743258808", "0809421239311", "0125847791412", 
                          "0671453355104", "042327212304", "0887553712102", "0204145974808", 
                          "0713431429202", "0208480761018", "0905188835300", "0160374603998", 
                          "0778276090510", "0504312377900", "0991145731807", "0647201845206", 
                          "0637482737005", "0474619812103", "0817420987900", "0970500642209", 
                          "0839411947603", "0800081986104", "0704703749401", "0688288022003", 
                          "0713869819004", "0720445004501", "0805469037201", "0725936371809", 
                          "0503986647505", "036360942301", "0112898942202", "017025148404", 
                          "085018966209", "075784887694", "043283522692", "0117814135893", 
                          "0369065037402", "0736538502404", "0252645348808", "0906916983202", 
                          "0969147846718", "0729734540102", "0319345085107", "0447050635303", 
                          "0791916121004", "0608089286702", "0304188758405", "0552300917200", 
                          "0465588741605", "0207544889903", "0751528529603", "0602452646106", 
                          "046137638006", "0156214148697", "0493166969401", "0590096877813", 
                          "0173299755202", "0179597134903", "0490704966896", "0108961111006", 
                          "0823175828602", "0456298137719", "0853269477202", "0455312624903", 
                          "0525247903009", "0404836900409", "0469915585804", "0543481046604", 
                          "0439547246805", "0882262949208", "0554072553108", "0822327872306", 
                          "0943925396910", "0913695315308", "0200943027702", "0744066990009", 
                          "0188510214005", "0370147780007", "0167049231801", "0658618520605", 
                          "0992508932009", "0126076270500", "0326516425507", "034408872004", 
                          "0430662617406", "0442579593600", "0433320766008", "0870568167702", 
                          "038177988704", "0976559189807", "0252987797901", "0123485846301", 
                          "0314386385815", "0396368639900", "0196459185206", "0925516323103", 
                          "0234578153505", "0740487617408", "0890087624410", "0746759738009", 
                          "0157778896604", "0686532742102", "0987605799006", "0609514779691", 
                          "0758610200105", "051635760208", "0320093489709", "0807888102003", 
                          "0307955518208", "021208424208", "0126791491904", "0630022377501", 
                          "0583060288605", "0119158487501", "0555321980101", "0401511670506", 
                          "0402521607405", "0565457468009", "0477654100492", "0361346660811", 
                          "0558681738602", "0424419397804", "0773185460005", "0194731575606", 
                          "0243998374609", "0514532571208", "0691717205205", "0362377089307", 
                          "047696971609") ])) {
  
  photo = paste0("photos/", i, ".jpg")
  
  if (!file.exists(photo)) {
    
    try(download.file(paste0("http://data.riksdagen.se/filarkiv/bilder/ledamot/", i, "_80.jpg"),
                      photo, mode = "wb", quiet = TRUE), silent = TRUE)
    
  }
  
  # empty photos are 791 bytes
  if (!file.exists(photo) | file.info(photo)$size < 1000) {
    
    file.remove(photo) # will warn if missing
    s$photo[ s$url == i ] = NA
    
  } else {
    
    s$photo[ s$url == i ] = photo
    
  }
  
}

s$photo[ !grepl("photos/", s$photo) ] = NA # exception URLs to missing
s$sex = ifelse(s$sex == "kvinna", "F", "M")

# ==============================================================================
# CHECK CONSTITUENCIES
# ==============================================================================

# note: constituency is very often missing in legislatures before 1994 due
# to a revision in the national apportionment of fixed constituency seats
s$county = ifelse(s$county == "", NA, s$county)
s$county[ grepl("Götalands", s$county) ] = paste(s$county[ grepl("Götalands", s$county) ], "valkrets")
s$county[ grepl("Skåne", s$county) ] = paste(s$county[ grepl("Skåne", s$county) ], "valkrets")
s$county = gsub("\\s", "_", s$county)
table(s$county, exclude = NULL)

cat("Checking constituencies,", sum(is.na(s$county)), "missing...\n")
for (i in na.omit(unique(s$county))) {
  
  g = GET(paste0("https://sv.wikipedia.org/wiki/", i))
  
  if (status_code(g) != 200)
    cat("Missing Wikipedia entry:", i, "\n")
  
  g = xpathSApply(htmlParse(g), "//title", xmlValue)
  g = gsub("(.*) – Wikipedia(.*)", "\\1", g)
  
  if (gsub("\\s", "_", g) != i)
    cat("Discrepancy:", g, "(WP) !=", i ,"(data)\n")
  
}

s$party[ s$party == "L" ] = "FP" # new name, Nov. 2015: Liberalerna
s$party[ s$party == "-" ] = "IND"
stopifnot(!is.na(groups[ s$party ]))

# solve duplicates
s = arrange(s, name, born) %>%
  group_by(name) %>%
  mutate(n = n(), o = 1:n()) %>%
  group_by() %>%
  mutate(name = ifelse(n == 1, name, paste0(name, "-", o))) %>%
  select(-n, -o) %>%
  data.frame(row.names = gsub("\\D", "", s$url))

stopifnot(!duplicated(s$url))
stopifnot(!duplicated(s$name))
cat("Found", nrow(s), "MPs\n")

s$url = paste0("http://data.riksdagen.se/personlista/?iid=", s$url)

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
# stopifnot(!is.na(s$nyears) & is.integer(s$nyears)) # computed on the fly
stopifnot(!is.na(s$url) & grepl("^http(s)?://(.*)", s$url))
stopifnot(s$party %in% names(colors))
