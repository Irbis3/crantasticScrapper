library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(stringr)
library(scales)
library(RColorBrewer)
library(Hmisc)

###--------------------------------------------------
### Set up the Maps.
### Map code borrows heavily from work by Bob Rudis:
### https://github.com/hrbrmstr/rd3albers
###--------------------------------------------------

theme_set(theme_minimal())

## Make a "figures" subdirectory if one doesn't exist
ifelse(!dir.exists(file.path("figures")),
       dir.create(file.path("figures")),
       FALSE)

theme_map <- function(base_size=9, base_family="") {
    require(grid)
    theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.margin=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
          )
}

###--------------------------------------------------
### We'll ynderlay the use maps with US county
### boundaries. It makes the map easier to see in
### sparse regions.
###--------------------------------------------------

###--------------------------------------------------
### US County Boundaries
###--------------------------------------------------

us.counties <- readOGR(dsn="data/gz_2010_us_050_00_5m.json",
                       layer="OGRGeoJSON")

# Convert it to Albers equal area projection
us.counties.aea <- spTransform(us.counties,
                               CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

us.counties.aea@data$id <- rownames(us.counties.aea@data)

# Extract, then rotate, shrink & move alaska (and reset projection)
# need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
alaska <- us.counties.aea[us.counties.aea$STATE=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us.counties.aea)

# extract, then rotate & shift hawaii
hawaii <- us.counties.aea[us.counties.aea$STATE=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(us.counties.aea)

# remove old states and put new ones back in; note the different order
# we're also removing puerto rico in this example but you can move it
# between texas and florida via similar methods to the ones we just used
us.counties.aea <- us.counties.aea[!us.counties.aea$STATE %in% c("02", "15", "72"),]
us.counties.aea <- rbind(us.counties.aea, alaska, hawaii)


###--------------------------------------------------
### Merge some census county-level data
### with county map data. Not strictly necessary,
### but I might use it later.
###--------------------------------------------------

state.data <- read.csv("data/census/state-data-statabs-2012.csv", header=TRUE)

county.names <- read.csv("data/census/fips-by-state.csv", header=TRUE)

county.data <- read.csv("data/census/DataSet.txt", header=TRUE)
county.data$id <- as.character(county.data$fips)
ind <- county.data$fips<10000
county.data$id[ind] <- paste("0", county.data$id[ind], sep="")
county.data$id[county.data$id=="00"] <- "0"

ind <- match(county.data$fips, county.names$fips)
county.data$name <- county.names$name[ind]
county.data$state <- county.names$state[ind]

ind <- match(state.data$fips, county.data$fips)
county.data$state[ind] <- state.data$State.Abbr

## Add state names as levels of county name, so states have FIPS too
levels(county.data$name) <- c(levels(county.data$name), levels(state.data$State))
county.data$name[ind] <- state.data$State


### Add census region. Don't call the variable "region" because that's
### already reserved by the map object
ind <- match(county.data$state, state.data$State.Abbr)
county.data$census.region <- state.data$Region[ind]

county.data$pop.dens <- with(county.data, PST045214/LND110210)
county.data$pop.dens <- cut2(county.data$pop.dens,
                             cuts = c(0, 10, 100, 1000, 10000))

county.data$pct.black <- cut2(county.data$RHI225213,
                              cuts = c(0, 2, 5, 10, 15, 25, 50))


co.map <- fortify(us.counties.aea, region="GEO_ID")
co.map$id <- str_replace(co.map$id, "0500000US", "")

co.map <- merge(co.map, county.data, by="id")




###--------------------------------------------------
### US Federal Lands data
### Details: http://nationalmap.gov/small_scale/mld/fedlanp.html
### Shapefile: ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/SmallScale/Data/Boundaries/fedlanp010g.shp_nt00966.tar.gz
###--------------------------------------------------

### Download and uncompress the file.

### quite a big file
data <- readOGR(dsn="data/fedlanp010g.shp",
                       layer="fedlanp010g")

# Convert it to Albers equal area projection
data.aea <- spTransform(data, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

data.aea@data$id <- rownames(data.aea@data)

# Extract, then rotate, shrink & move alaska (and reset projection)
# need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
alaska <- data.aea[data.aea$STATE_FIPS=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(data.aea)

# extract, then rotate & shift hawaii
hawaii <- data.aea[data.aea$STATE_FIPS=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(data.aea)

# remove old states and put new ones back in; note the different order
# we're also removing puerto rico in this example but you can move it
# between texas and florida via similar methods to the ones we just used
data.aea <- data.aea[!data.aea$STATE_FIPS %in% c("02", "15", "72"),]
data.aea <- rbind(data.aea, alaska, hawaii)

land.df <- data.aea@data
keep.me <- colnames(land.df) %nin% c("URL")
land.df <- land.df[,keep.me]

###--------------------------------------------------
### Recode Land Use into smaller categories.
###--------------------------------------------------
library(car)

## Break this up into pieces just becasue it's so long
land.df$Use <- recode(land.df$FEATURE1,
                      "c('AF Guard', 'Air Force', 'Army', 'Army Guard', 'Coast Guard', 'Marine Corps', 'Navy', 'Test Site') = 'Military'")

land.df$Use <- recode(land.df$Use,
                      "c('National Battlefield', 'National Cemetary', 'National Cemetery', 'National Historic Park', 'National Memorial', 'National Historic Site', 'National Military Park', 'National Historic Reserve', 'National Monument', 'Land') = 'Other'")

land.df$Use <- recode(land.df$Use,
                      "c('National Conservation Area', 'National Forest', 'National Fish Hatchery', 'National Lakeshore', 'National Park', 'Coordinated Area', 'National Grassland', 'National Game Preserve', 'National Natural Landmark', 'National Parkway', 'National Preserve', 'National Recreation Area', 'National Scenic Area', 'National Seashore', 'National Wildlife Refuge', 'National Wilderness Area', 'Waterfowl Production Area', 'Wildlife Management Area', 'Wild and Scenic River') = 'National Park, Preserve, or Wilderness Area'")

land.df$Use <- recode(land.df$Use,
                      "c('National Reserve', 'National Laboratory', 'Research Natural Area', 'MWAA', 'Indian Reservation') = 'Other'",
                      levels = c("National Park, Preserve, or Wilderness Area",
                                              "Public Domain Land",
                                              "Military",
                                              "Lake",
                                              "Other"))

### Make it into a data frame
fed.map <- fortify(data.aea)

ind <- match(fed.map$id, land.df$id)
fed.map <- cbind(fed.map, land.df[ind,])


###--------------------------------------------------
### Make our big map
###--------------------------------------------------

p <- ggplot(fed.map)

### The geom_polygon provides the county boundary layer. Note fill=NA
p1 <- p + geom_polygon(data=co.map, aes(x=long, y=lat, group=group),
             color="gray70",
             fill=NA,
             size=0.03)

### Then we layer the land-use map on top
p2 <- p1 + geom_map(data = fed.map,
             map = fed.map,
             aes(map_id = id,
                 x=long,
                 y=lat,
                 group=group,
                 fill=Use),
             color="white",
             size = 0.01)

### And get it ready for presentation
p3 <- p2 + coord_equal() +
    theme_map() +
    theme(legend.position = "top",
          plot.title = element_text(face="bold")) +
    scale_fill_manual(values = my.colors()[c(1, 2, 5, 3, 4)],
                      labels = c("National Park, Preserve\nor Wilderness Area",
                                 "Public Domain\nLand",
                                 "Military Use",
                                 "Lake",
                                 "Other")) +
    labs(fill="Main Purpose\nor Type") +
    ggtitle("Land Owned or Administered by the US Federal Government")

### PNG and PDF
ggsave("figures/federal-lands.png",
       p3,
       height=8,
       width=12,
       dpi=300)

pdf(file="figures/federal-lands.pdf", height = 9, width = 10)
print(p3)
credit()
dev.off()

### --------------------------------------------------
### One-feature maps
### --------------------------------------------------

### A convenience function to map one land-use feature at a time.
### data = the land use map data
### var = The variable you want to select from
### feature = The category of this variable you want to map
### name = used to constructing the filename
### col = the color you want to use for the fill
### co.data = the county boundary map data

feature.map <- function(data=fed.map,
                        var="FEATURE1",
                        feature="Public Domain Land",
                        name="public-domain",
                        col = my.colors()[2],
                        co.data=co.map) {

    fname <- paste0("figures/federal-lands-", name, ".png")

    ## note eval(parse(text=var)) to pass the column name
    ## to subset() properly
    p <- ggplot(subset(fed.map, eval(parse(text=var))==feature))

    p1 <- p + geom_polygon(data=co.data, aes(x=long, y=lat, group=group),
             color="gray50",
             fill=NA,
             size=0.03)

    p2 <- p1 + geom_map(data = subset(fed.map, eval(parse(text=var))==feature),
                        map = subset(fed.map, eval(parse(text=var))==feature),
                        aes(map_id = id,
                            x=long,
                            y=lat,
                            group=group,
                            fill=Use),
                        color="white",
                        size = 0.01) +
        coord_equal() +
        theme_map() +
        theme(plot.title = element_text(face="bold", size=14)) +
        guides(fill = FALSE) +
        scale_fill_manual(values = col) +
        ggtitle(paste0("\n", feature))

    print(p2)
    ggsave(fname,
       p2,
       height=8,
       width=12,
       dpi=300)
}


feature.map(feature="Public Domain Land",
            name="public-domain")

feature.map(feature="National Forest",
            name="national-forest",
            col="springgreen4")

feature.map(feature="National Park",
            name="national-park",
            col="chartreuse3")

feature.map(feature="Military",
            var="Use",
            name="military",
            col="firebrick")

feature.map(feature="National Grassland",
            name="national-grassland",
            col="darkgoldenrod")

feature.map(feature="National Wilderness Area",
            name="national-wilderness",
            col="cornflowerblue")

feature.map(feature="National Wildlife Refuge",
            name="national-wildlife",
            col="coral4")

feature.map(feature="National Wildlife Refuge",
            name="national-wildlife",
            col="coral4")



###--------------------------------------------------
### Primary Management Agency
###--------------------------------------------------

fed.map$Administrator <- recode(fed.map$ADMIN1,
                                "c('GSA', 'HHS', 'DOT', 'NASA', 'MWAA', 'DOL', 'DOJ', 'DOC', 'BIA', 'USDA', 'VA') = 'Other'; 'BLM' = 'Bureau of Land Management'; 'BOR' = 'Bureau of Reclamation'; 'DOD' = 'Dept of Defense'; 'DOE' = 'Dept of Energy'; 'NPS' = 'Park Service'; 'FS' = 'Forest Service'; 'FWS' = 'Fish and Wildlife Service'; 'TVA' = 'Tenn. Valley Authority'",
                                levels = c("Bureau of Land Management", "Forest Service",
                                           "Dept of Defense", "Park Service", "Bureau of Reclamation",
                                           "Dept of Energy", "Fish and Wildlife Service",
                                           "Tenn. Valley Authority", "Other"))



p <- ggplot(fed.map)

### The geom_polygon provides the county boundary layer. Note fill=NA
p1 <- p + geom_polygon(data=co.map, aes(x=long, y=lat, group=group),
             color="gray70",
             fill=NA,
             size=0.03)

### Then we layer the land-use map on top
p2 <- p1 + geom_map(data = fed.map,
             map = fed.map,
             aes(map_id = id,
                 x=long,
                 y=lat,
                 group=group,
                 fill=Administrator),
             color="white",
             size = 0.01)

### And get it ready for presentation
p3 <- p2 + coord_equal() +
    theme_map() +
    theme(legend.position = "top",
          plot.title = element_text(face="bold")) +
    labs(fill="Primary\nAdministrator") +
    ggtitle("Land Owned or Administered by the US Federal Government")

### PNG and PDF
ggsave("figures/federal-lands-admin.png",
       p3,
       height=8,
       width=12,
       dpi=300)

pdf(file="figures/federal-lands-admin.pdf", height = 9, width = 10)
print(p3)
credit()
dev.off()
