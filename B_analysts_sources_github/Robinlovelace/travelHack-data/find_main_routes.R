# Get top lines
library(sf)
library(tmap)
tmap_mode("view")


l.all = readRDS("../travelHack/data/PCT/l_WY.Rds")
l.all = st_as_sf(l.all)
l.all = l.all[,c("is_two_way","dist","id",
"msoa1","msoa2","all", "light_rail","train","bus", "taxi","motorbike","car_driver",              
"car_passenger","bicycle","foot","other", "geo_label1","geo_label2",              
"dist_fast","dist_quiet","time_fast",
"time_quiet","cirquity","distq_f",
"avslope","avslope_q","clc",
"geometry")]

#Get the majoir routes with many people
l <- l.all[l.all$all > 50,]

#get bounds
bounds <- readRDS("../travelHack/data/PCT/local_authority.Rds")
bounds <- bounds
l <- st_transform(l, 27700)
l <- l[bounds,]

#Remove very short (walking routes)
l <- l[l$dist > 1,]


l$pmotor <- round((l$car_driver + l$car_passenger + l$taxi + l$motorbike) / l$all * 100,0)
l$pactive <- round((l$bicycle + l$foot) / l$all * 100,0)
l$ptrans <- round((l$bus + l$train + l$light_rail) / l$all * 100,0)

#Filter out high transit or walking
l <- l[l$ptrans < 30,]
l <- l[l$pactive < 30,]

#filer to high number of traverls
l <- l[l$all > 100,]

saveRDS(l,"../travelHack/data/listofroutes.Rds")

qtm(l)
