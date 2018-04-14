library(stringr)
library(foreign)
library(magrittr)
cen <- read.csv("centroids.csv")
cen$zone <- NA_integer_
setUTMZone <- function(df, x1,y1,x2,y2, zone) {
  df$zone[(df$X > x1 & df$X < x2 & df$Y > y1 & df$Y < y2)] <- zone
  return(df)
}

#Bounds for each zone from http://spatialreference.org/ref/epsg/26716/
cen <- setUTMZone(cen, -120.0000, 27.0000, -114.0000, 78.3300, 11)
cen <- setUTMZone(cen, -114.0000, 24.8300, -108.0000, 79.2500, 12)
cen <- setUTMZone(cen, -108.0000, 17.8300, -102.0000, 80.1000, 13)
cen <- setUTMZone(cen, -102.0000, 15.5000, -96.0000, 81.0000, 14)
cen <- setUTMZone(cen, -96.0000, 13.5700, -90.0000, 82.0000, 15)
cen <- setUTMZone(cen, -90.0000, 9.1000, -84.0000, 82.5000, 16)

cen[which(cen$CVEMUNI== 3003 | cen$CVEMUNI == 3008),]$zone <- 12
cen[which(cen$CVEMUNI== 30099),]

write.csv(cen, "centroids_zones.csv", row.names = FALSE)

copyFile <- function(prjFile, file){
  file.copy(file.path('..', 'prj', prjFile),
            file.path('..', 'zip', dirname(file), 
                      str_c(str_split_fixed(basename(file), "\\.", 2)[[1]], ".prj")),
            overwrite = TRUE
  )
}

files <- list.files(file.path('..', 'zip'), recursive = TRUE, pattern = "*.dbf")
for(file in files){
  if(str_detect(file, "MANZANAS") & !str_detect(file, "2168/MANZANAS/U0990001A.dbf")) {
    print(file)
    dbf = read.dbf(file.path('..', 'zip', file), as.is = TRUE)
    colName = names(dbf)[which(names(dbf) == "CLVMNZ" | names(dbf) == "CLVAGB")]
    
    mun <- dbf[["CLVMNZ"]][1] %>% 
      str_sub(1, 5) %>%
      as.numeric
    #U1440001M.shp
    if(str_detect(file, "937/MANZANAS/I0200084m.dbf")){
      mun  <- 18020
    }
    #print(as.character(cen$zone[which(mun == cen$CVEMUNI)])[[1]])
    switch(as.character(cen$zone[which(mun == cen$CVEMUNI)])[[1]],
           "11"=copyFile("26711.prj", file),
           "12"=copyFile("26712.prj", file),
           "13"=copyFile("26713.prj", file),
           "14"=copyFile("26714.prj", file),
           "15"=copyFile("26715.prj", file),
           "16"=copyFile("26716.prj", file))
  }
}