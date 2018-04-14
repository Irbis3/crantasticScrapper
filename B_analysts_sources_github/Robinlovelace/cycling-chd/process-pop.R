#################################################
####### Run before process-minap.R ##############
####### Process population data for MSOAs #######
#################################################


# Load data
fpath <- "data/Population Data/Cleaned Raw Data/"
pop_i <- pop_m <- pop_f <- vector(mode = "list", 11)
male_raw <- female_raw <- male_all <- female_all <- vector(mode = "list", 11)

y = 2003 # initiate counter for testing

#' @examples
#' cap_at_93(90)
#' cap_at_93(93)
#' cap_at_93(100)
cap_at_93 <- function(x){
  if(x > 93)
    93 else
      x
}


for(y in 2003:2013){
  i = y - 2003 # counter
  j = i + 1

  fname_m <- paste0(fpath, "male_pop_", y, "_msoa.csv")
  fname_f <- paste0(fpath, "female_pop_", y, "_msoa.csv")
  male_yr <- read.csv(fname_m)
  female_yr <- read.csv(fname_f)

  # Some of 2012 and 2013 codes are LAs as well (how data presented) so need removing
  male_yr$geography <- substr(male_yr$msoa_code,1,3) # Take first 3 characters of MSOA code to decipher what type of geography
  male_yr <- male_yr[male_yr$geography == "E02",] # Subset non-msoas out of data
  male_yr$geography <- NULL # Delete variable

  female_yr$geography <- substr(female_yr$msoa_code,1,3) # Take first 3 characters of MSOA code to decipher what type of geography
  female_yr <- female_yr[female_yr$geography=="E02",] # Subset non-msoas out of data
  female_yr$geography <- NULL # Delete variable

  male_all[[j]] <- data.frame(male_yr[1])
  female_all[[j]] <- data.frame(female_yr[1])

  male_all[[j]]$mpop_0_9 <- apply(male_yr[,(3 + i):(i + 12)], 1, sum)
  male_all[[j]]$mpop_10_19 <- apply(male_yr[,(13 + i):(i + 22)], 1, sum)
  male_all[[j]]$mpop_20_26 <- apply(male_yr[,(23 + i):(i + 29)], 1, sum)
  male_all[[j]]$mpop_27_36 <- apply(male_yr[,(30 + i):(i + 39)], 1, sum)
  male_all[[j]]$mpop_37_46 <- apply(male_yr[,(40 + i):(i + 49)], 1, sum)
  male_all[[j]]$mpop_47_56 <- apply(male_yr[,(50 + i):(i + 59)], 1, sum)
  male_all[[j]]$mpop_57_66 <- apply(male_yr[,(60 + i):(i + 69)], 1, sum)
  male_all[[j]]$mpop_67_76 <- apply(male_yr[,(70 + i):(i + 79)], 1, sum)
  male_all[[j]]$mpop_77_86 <- apply(male_yr[(80 + i):cap_at_93(i + 90)], 1, sum)
  male_all[[j]]$mpop_87plus <- apply(male_yr[cap_at_93(91 + i):cap_at_93(i + 93)], 1, sum)

  female_all[[j]]$fpop_0_9 <- apply(female_yr[,(3 + i):(i + 12)], 1, sum)
  female_all[[j]]$fpop_10_19 <- apply(female_yr[,(13 + i):(i + 22)], 1, sum)
  female_all[[j]]$fpop_20_26 <- apply(female_yr[,(23 + i):(i + 29)], 1, sum)
  female_all[[j]]$fpop_27_36 <- apply(female_yr[,(30 + i):(i + 39)], 1, sum)
  female_all[[j]]$fpop_37_46 <- apply(female_yr[,(40 + i):(i + 49)], 1, sum)
  female_all[[j]]$fpop_47_56 <- apply(female_yr[,(50 + i):(i + 59)], 1, sum)
  female_all[[j]]$fpop_57_66 <- apply(female_yr[,(60 + i):(i + 69)], 1, sum)
  female_all[[j]]$fpop_67_76 <- apply(female_yr[,(70 + i):(i + 79)], 1, sum)
  female_all[[j]]$fpop_77_86 <- apply(female_yr[(80 + i):cap_at_93(i + 90)], 1, sum)
  female_all[[j]]$fpop_87plus <- apply(female_yr[cap_at_93(91 + i):cap_at_93(i + 93)], 1, sum)

  print(y)

  pop_m[[j]] <- reshape(male_all[[j]],
                        varying = c("mpop_0_9", "mpop_10_19", "mpop_20_26", "mpop_27_36", "mpop_37_46",
                                    "mpop_47_56", "mpop_57_66", "mpop_67_76", "mpop_77_86", "mpop_87plus"),
                        v.names = "population",
                        timevar = "age_band",
                        times = c("0-9", "10-19","20-26","27-36","37-46","47-56","57-66","67-76","77-86", "87+"),
                        #new.row.names = 1:60392,
                        direction = "long")
  pop_m[[j]]$id <- NULL
  pop_m[[j]]$year <- y
  pop_m[[j]]$sex <- "Male"

  pop_f[[j]] <- reshape(female_all[[j]],
                        varying = c("fpop_0_9", "fpop_10_19", "fpop_20_26", "fpop_27_36", "fpop_37_46",
                                    "fpop_47_56", "fpop_57_66", "fpop_67_76", "fpop_77_86", "fpop_87plus"),
                        v.names = "population",
                        timevar = "age_band",
                        times = c("0-9", "10-19","20-26","27-36","37-46","47-56","57-66","67-76","77-86", "87+"),
                        #new.row.names = 1:60392,
                        direction = "long")
  pop_f[[j]]$id <- NULL
  pop_f[[j]]$year <- y
  pop_f[[j]]$sex <- "Female"

  pop_i[[j]] <- rbind(pop_m[[j]], pop_f[[j]])

}

# Join together
pop_03_13 <- dplyr::bind_rows(pop_i)

save(pop_03_13, file = "data/pop_03_13.RData")
rm(list=ls())
gc()
