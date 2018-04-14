abbrev <- data_frame(abbrev = c("BC", "SON", "CHIH",
                                "BCS", "SIN", "DGO", "COAH", "NL","TAM",
                                "NAY","ZAC", "AGS", "SLP",
                                "JAL",  "GTO", "QRO", "HGO","VER",
                                "MICH","COL","MEX", "TLAX",
                                "GRO", "MOR","DF", "PUE", "YUC" ,
                                "OAX", "TAB", "CAMP", "QROO",
                                "CHPS"),
                     state = c("BAJA CALIFORNIA", "SONORA", "CHIHUAHUA",
                               "BAJA CALIFORNIA SUR", "SINALOA", "DURANGO", "COAHUILA", "NUEVO LEON", "TAMAULIPAS",
                               "NAYARIT", "ZACATECAS", "AGUASCALIENTES", "SAN LUIS POTOSI",
                               "JALISCO", "GUANAJUATO", "QUERETARO", "HIDALGO", "VERACRUZ",
                               "MICHOACAN","COLIMA", "MEXICO","TLAXCALA",
                               "GUERRERO", "MORELOS", "DISTRITO FEDERAL", "PUEBLA", "YUCATAN" ,
                               "OAXACA", "TABASCO", "CAMPECHE", "QUINTANA ROO",
                               "CHIAPAS"))

rnpe_c <- fromJSON('data/rnped_comun.json')
rnpe_c <- as.data.frame(rnpe_c$aaData, stringsAsFactors = FALSE)
rnpe_c$date <- as.Date(rnpe_c$fuerocomun_desapfecha, "%d/%m/%Y")
rnpe_c$tipo <- "comun"
rnpe_c$fuerocomun_desapentidad %<>% str_replace_all(" DE ZARAGOZA|ESTADO DE ", "")
write.csv(rnpe_c, "data/rnped_comun.csv", row.names=FALSE)

rnpe_f <- fromJSON('data/rnped_federal.json')
rnpe_f <- as.data.frame(rnpe_f$aaData, stringsAsFactors = FALSE)

rnpe_f <- rnpe_f %>%
  mutate(fuerofederal_ultimapais = iconv(fuerofederal_ultimapais, to="ASCII//TRANSLIT"),
         fuerofederal_ultimaentidad = iconv(fuerofederal_ultimaentidad, to="ASCII//TRANSLIT"),
         fuerofederal_ultimampio = iconv(fuerofederal_ultimampio, to="ASCII//TRANSLIT"))
rnpe_f$date <- as.Date(rnpe_f$fuerofederal_ultimafecha, "%d/%m/%Y")
rnpe_f$tipo <- "federal"
write.csv(rnpe_f, "data/rnped_federal.csv", row.names=FALSE)



rnpe_c <- left_join(rnpe_c, abbrev, by = c("fuerocomun_desapentidad" = "state"))
rnpe_f <- left_join(rnpe_f, abbrev, by = c("fuerofederal_ultimaentidad" = "state"))