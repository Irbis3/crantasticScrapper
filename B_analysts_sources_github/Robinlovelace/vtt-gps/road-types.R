# road type

source("R/load-data.R")

# 1 common road types in the study area

b = bb(i1)

# create all highways - enter ?extract_osm_objects
h_secondary = extract_osm_objects(key = 'highway', value = "secondary",
                        bbox = b)
h = extract_osm_objects(key = 'highway', value = "secondary",
                        bbox = b)

# save 1 road type ???
summary(h)

plot(h)
plot(i1, add = T, col = "red")

bi1 = stplanr::buff_geo(i1, width = 50)

# see if bi1 (buffer of i1) look right
plot(bi1, add = T, col = "green")

# chop out all road network in the buff
ri1 = gIntersection(bi1, h)

# add information back to track data - needs in metres
itea$main_road[1] = gLength(ri1)

plot(ri1, add = T)
plot(ri1)
class(ri1)
summary(ri1)

