library("rplos")
library("rcrossref")
library("dplyr")
library("httr")
library("jsonlite")
library("XML")
library("alm")

# load code
source("cr_code.R")
source("zzz.R")

# 4950 is the F1000Research member ID
# 2258 is the Pensoft member ID
# cr_members(query = "f1000")
out <- cr_members(member_ids = 4950, works = TRUE, limit = 50)$data
dois <- out$DOI

# get crossmark data
mdout <- apply(out, 1, function(x) cross_mark(x['DOI']))

# get figure and other media links from article xml
figs_media <- lapply(dois, function(x){
  tmp <- get_xml(make_url(x))
  if(is.na(tmp)){
    list(
      figs=NA,
      media=NA
    )
  } else {
    list(
      figs=extract_figs(tmp),
      media=extract_media(tmp)
    )
  }
})

# alm data
## no data for figshare
# alm_dat(dois)

# combine crossref, crossmark, and figure/media links
# F1000 isn't giving links to full text yet...
# cr_full_links("10.12688/f1000research.3817.1")

# combine all data
alldat <- Map(make_entry,
    apply(out, 1, function(x) as.list(x[c('DOI','URL')])),
    lapply(mdout, function(x){
      if(all(is.na(x))){
        list(cm_updates=NA, cm_assertions=NA)
      } else {
        list(target_doi=pick(x$updated_by$doi), assertions=no_na(x$assertions$href))
      }
    }),
    figs_media
)

jsonlite::toJSON(alldat, auto_unbox = TRUE, pretty = TRUE)
jsonlite::toJSON(alldat, auto_unbox = TRUE)
