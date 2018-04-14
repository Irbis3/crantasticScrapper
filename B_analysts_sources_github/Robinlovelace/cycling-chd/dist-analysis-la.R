la_dists = readr::read_csv("data/distance-bands-travel-to-work--nomis-table-qs702ew.csv")
la_dists = la_dists[grepl(pattern = "E", la_dists$`geography code`),]
tail(la_dists)
agdists = colSums(la_dists[6:13])
agdists_props = agdists / sum(agdists)
agdists_props[1] # result 1: 20.4% of those regularly commuting to work go < 2km
sum(agdists_props[c(1,2)]) # 43.0% go travel go < 5 km
sum(agdists_props[c(1,2,3)]) # 64.4% go travel go < 10 km

