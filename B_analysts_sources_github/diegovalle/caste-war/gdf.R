
writeGDF <- function(fileName, df, freq, last_names) {
  write("nodedef> name VARCHAR", fileName)
  write.table(last_names, fileName, append = TRUE, row.names=FALSE, col.names=FALSE)
  write("edgedef> node1 VARCHAR, node2 VARCHAR, Weight DOUBLE", fileName, append = TRUE)
  write.table(freq, fileName, append = TRUE, 
              row.names=FALSE, col.names=FALSE, sep = ",")
}

createGDF <- function(df, fileName) {
  #df[ ,c("paterno", "materno")] <- sortApellidos(df)
  
  df$paterno <- str_replace_all(df$paterno, "\\s","_")
  df$materno <- str_replace_all(df$materno, "\\s","_")
  last_names <- unique(c(as.character(df$paterno), as.character(df$materno)))
  
  
  
  freq <- df %>%
    group_by(paterno, materno) %>%
    summarise(Weight = n()) %>%
    arrange(desc(Weight))
  
  writeGDF(fileName, df, freq, last_names)
  return(TRUE)
}

createGDF(filter(all, partido == "PAN"), file.path("gephi", "pan.gdf"))
createGDF(filter(filter(all, partido == "PRD"), paterno != "" | materno != "."), 
          file.path("gephi", "prd.gdf"))
createGDF(filter(all, partido == "PRD"), file.path("gephi", "prd.gdf"))
freq <- filter(all, partido == "PRI") %>%
  group_by(paterno, materno) %>%
  summarise(Weight = n()) %>%
  arrange(desc(Weight)) %>%
  filter(Weight > 2)
createGDF(semi_join(filter(all, partido == "PRI"), freq), 
          file.path("gephi", "pri10.gdf"))
createGDF(filter(all, entidad == "YUCATAN"), 
          file.path("gephi", "pan-pri-prd-yucatan.gdf"))
createGDF(filter(all, entidad == "QUINTANA ROO"), 
          file.path("gephi", "pan-pri-prd-qroo.gdf"))
createGDF(filter(all, entidad == "CAMPECHE"),
          file.path("gephi", "pan-pri-prd-camp.gdf"))

createGDF(filter(all, entidad == "CHIAPAS"), 
          file.path("gephi", "pan-pri-prd-chis.gdf"))

