out <- cr_members(member_ids = 340, works = TRUE, limit = 50)$data
dois <- out$DOI

# get crossmark data
mdout <- lapply(dois, cross_mark)

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
## no alm data
# alm_pensoft(dois[1:5])

# combine crossref, crossmark, and figure/media links
# F1000 isn't giving links to full text yet...
# cr_full_links("10.12688/f1000research.3817.1")
alldat <- Map(make_entry,
              apply(out, 1, function(x) as.list(x[c('DOI','URL')])),
              mdout,
              figs_media
)

jsonlite::toJSON(alldat, auto_unbox = TRUE, pretty = TRUE)
jsonlite::toJSON(alldat, auto_unbox = TRUE)
