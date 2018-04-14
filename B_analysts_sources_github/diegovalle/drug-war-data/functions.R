dateOccur <- function(df, assume=FALSE){
  try({
    df$date_occur<- with(df,
                         as.Date(str_c(year_occur,
                                       month_occur,
                                       day_occur,
                                       sep = "-"),
                                 "%Y-%m-%d"))
    
    if(assume) {
      ## Deaths without a valid date of occurance are assumed to
      ## have happened when they were registered
      df$date_occur[which(is.na(df$date_occur))] <-
        with(df[which(is.na(df$date_occur)),],
             as.Date(str_c(year_occur,
                           month_occur,
                           day_reg,
                           sep = "-") ,
                     "%Y-%m-%d")) 
      ## Day and month of registration
      df$date_occur[which(is.na(df$date_occur))] <-
        with(df[which(is.na(df$date_occur)),],
             as.Date(str_c(year_occur,
                           month_reg,
                           day_reg,
                           sep = "-"),
                     "%Y-%m-%d"))
      ## Now with year of registration
      df$date_occur[which(is.na(df$date_occur))] <-
        with(df[which(is.na(df$date_occur)),],
             as.Date(str_c(year_reg,
                           month_reg,
                           day_reg,
                           sep = "-"),
                     "%Y-%m-%d"))
      ## Invalidly registered dates (e.g February 31) are assumed as having
      ## happened on the 15th of each month
      df$date_occur[which(is.na(df$date_occur))] <-
        with(df[which(is.na(df$date_occur)),],
             as.Date(str_c(year_reg,
                           month_reg,
                           "15",
                           sep = "-"),
                     "%Y-%m-%d"))
    }
    
  })
  return(df$date_occur)
}

dateReg <- function(df, assume=FALSE) {
  try({df$date_reg <- with(df,
                           as.Date(str_c(year_reg,
                                         month_reg,
                                         day_reg,
                                         sep = "-"),
                                   "%Y-%m-%d"))
       
       
       if(assume) {     
         ##Invalid dates (e.g February 31) are assumed as
         ## having happened on the 15th of each month
         df$date_reg[which(is.na(df$date_reg))] <-
           with(df[which(is.na(df$date_reg)),],
                as.Date(str_c(year_reg,
                              month_reg,
                              "15",
                              sep = "-"),
                        "%Y-%m-%d"))
       }
  })
  return(df$date_reg)
}

extractStateCode <- function(id){
  id <- str_pad(id, 5, "left", pad = "0")
  as.numeric(str_sub(id, 1, 2))
}

extractMunCode <- function(id){
  id <- str_pad(id, 5, "left", pad = "0")
  as.numeric(str_sub(id, 3))
}

generateId <- function(state_code, mun_code) {
  as.numeric(gsub(" ", "0",
                str_c(format(state_code, width = 2),
                      format(mun_code, width = 3))))
}

stateToAbbrev <- function(v) {
  car::recode(v, "
              0 = NA;
              1 = 'Ags';
              2 = 'BC';
              3 = 'BCS';
              4 = 'Camp';
              5 = 'Coah';
              6 = 'Col';
              7 = 'Chis';
              8 = 'Chih';
              9 = 'DF';
              10 = 'Dgo';
              11 ='Gto';
              12 ='Gro';
              13 ='Hgo';
              14 ='Jal';
              15 ='Mex';
              16 ='Mich';
              17 ='Mor';
              18 ='Nay';
              19 ='NL';
              20 ='Oax';
              21 ='Pue';
              22 ='Qro';
              23 ='QR';
              24 ='SLP';
              25 ='Sin';
              26 ='Son';
              27 ='Tab';
              28 ='Tamps';
              29 ='Tlax';
              30 ='Ver';
              31 ='Yuc';
              32 = 'Zac';
              33 = 'USA';
              34 = 'LATAM';
              35 = 'Other';")
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title stateToName 
##' @param v 
##' @return vector
##' @author diego
##' @export
stateToName <- function(v) {
  car::recode(v, "1 = 'Aguascalientes';
              2 = 'Baja California';
              3 = 'Baja California Sur';
              4 = 'Campeche';
              5 = 'Coahuila';
              6 = 'Colima';
              7 = 'Chiapas';
              8 = 'Chihuahua';
              9 = 'Distrito Federal';
              10 = 'Durango';
              11 ='Guanajuato';
              12 ='Guerrero';
              13 ='Hidalgo';
              14 ='Jalisco';
              15 ='México';
              16 ='Michoacán';
              17 ='Morelos';
              18 ='Nayarit';
              19 ='Nuevo León';
              20 ='Oaxaca';
              21 ='Puebla';
              22 ='Querétaro';
              23 ='Quintana Roo';
              24 ='San Luis Potosí';
              25 ='Sinaloa';
              26 ='Sonora';
              27 ='Tabasco';
              28 ='Tamaulipas';
              29 ='Tlaxcala';
              30 ='Veracruz';
              31 ='Yucatán';
              32 = 'Zacatecas'")
}
#injury.intent$date_occur_death <- dateOccur(injury.intent)
numberOfDays  <- function(x) {
  ym <- as.yearmon(x)
  as.numeric(as.Date(ym, frac = 1) - as.Date(ym) + 1)
}
