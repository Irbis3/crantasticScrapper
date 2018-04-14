## Theme for maps


theme_bare <-theme(axis.line=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   panel.background=element_blank(),
                   panel.border=element_blank(),
                   panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank(),
                   plot.background=element_blank())

## Draw a small multiples chart of the membership numbers of the three main parties by state

drawParty <- function(all, state_centroids) {
  bystate <- all %>%
    group_by(state_code, partido) %>%
    summarise(count = n())
  ggplot(inner_join(bystate, state_centroids), aes(long, lat)) +
    geom_polygon(data = states.ff, aes(long, lat, group=group),
                 color = "black",fill = "transparent", size = .2) +
    geom_point(aes(size = count, fill = partido), 
               shape = 21, color = "black") +
    scale_size_area(max_size = 12) +
    scale_fill_manual("party",
                       breaks = c("PAN", "PRI", "PRD"), 
                       values = c("#2b8cbe", "#fed300", "#de2d26")) +
    facet_wrap(~partido, ncol = 2) +
    coord_map("albers", lat0 = bb[ 2 , 1 ] , lat1 = bb[ 2 , 2 ] ) +
    theme_bw() + 
    ggtitle("Political Party Membership in Mexico") +
    guides(fill = guide_legend(override.aes = list(size = 6)))+
    theme(legend.key = element_rect( fill = NA)) +
    theme_bare
}


cleanModularity <- function(party, modularity.data){
  
  #browser()
  map = data.table( modularity.paterno= modularity.data$Modularity.Class, 
                    paterno= modularity.data$Id)
  DT  <- data.table(party)
  
  setkey(DT,paterno)  
  setkey(map,paterno)
  DT <- merge(DT, map, all.x = TRUE)
  
  map = data.table( modularity.materno= modularity.data$Modularity.Class, 
                    materno= modularity.data$Id)
  setkey(DT,materno)  
  setkey(map,materno)  
  #DT[,list(DT, map)]
  DT <- merge(DT, map, all.x = TRUE)
  party <- data.frame(DT)
  #   party <- data_frame(state_code = party$state_code,
  #                       paterno = party$paterno,
  #                       materno = party$materno)
  #   party$modularity.paterno <- map[as.character(party$paterno)]
  #   party$modularity.materno <- map[as.character(party$materno)]
  
  
  t1 <- party[,c("state_code", "modularity.paterno")]
  t2 <- party[,c("state_code", "modularity.materno")]
  names(t1) <- c("state_code", "modularity")
  names(t2) <- c("state_code", "modularity")
  
  modularity.data <- as.data.frame(rbindlist(list(t1, t2)))
  
  bystate <- modularity.data %>%
    group_by(state_code, modularity) %>%
    summarise(count = n())
}

drawSingleModularity <- function(party, modularity.data, color, group, title) {
  
  bystate <- cleanModularity(party, 
                             modularity.data)
  
  ggplot(filter(full_join(bystate, state_centroids), modularity %in% group), aes(long, lat)) +
    geom_polygon(data = states.ff, aes(long, lat, group=group),
                 color = "black",fill = "transparent", size = .3) +
    geom_point(aes(size = count), fill = color, 
               shape = 21, color = "black") +
    scale_size_area("single\nlast name\ncount", max_size = 12) +
    coord_map("albers", lat0 = bb[ 2 , 1 ] , lat1 = bb[ 2 , 2 ] ) +
    theme_bw()+ 
    ggtitle(title) + 
    theme_bare+
    scale_shape(solid=FALSE)
}

drawModularity <- function(party, modularity.data, colors, groups, title) {
  
  bystate <- cleanModularity(party, modularity.data)
  
  ggplot(na.omit(full_join(bystate, state_centroids)), aes(long, lat)) +
    geom_polygon(data = states.ff, aes(long, lat, group=group),
                 color = "black",fill = "transparent", size = .1) +
    geom_point(aes(size = count, fill = as.factor(modularity)), 
               shape = 21, color = "black") +
    scale_size_continuous("single\nlast name\ncount", range = c(1.2,12)) +
    scale_fill_manual("modularity", breaks = groups, 
                       values = colors) +
    facet_wrap(~modularity, ncol = 3) +
    coord_map("albers", lat0 = bb[ 2 , 1 ] , lat1 = bb[ 2 , 2 ] ) +
    theme_bw() + 
    ggtitle(title) +
    guides(fill = guide_legend(override.aes = list(size = 6)))+
    theme(legend.key = element_rect( fill = NA)) +
    theme_bare +
    scale_shape(solid=FALSE)
}


## Chart of party membership
drawParty(all, state_centroids)
ggsave(file.path("charts", "parties.svg"), dpi = 100, width = 9.60, height = 7)


## Charts of modularity
pan.mod <- read.csv(file.path("gephi", "pan-modularity.csv"))
pan.groups <- 0:7
pan.colors <- c("#06f141", "#0641f1", 
                "#06f1f1", "#f1b606", "#f106b6", 
                "#7cf106", "#7c06f1", "#f10606")
drawModularity(filter(all, partido == "PAN"), pan.mod, pan.colors, pan.groups, 
               "Common Last Names, by Modularity (PAN)")
ggsave(file.path("charts", "sm_pan.svg"), dpi = 100, width = 9.60, height = 7)
drawSingleModularity(filter(all, partido == "PAN"), pan.mod, pan.colors[6], pan.groups[6], 
                     "Common Last Names, Yucatan Group (PAN)")
ggsave(file.path("charts", "yuc_pan.svg"), dpi = 100, width = 9.60, height = 7)
drawSingleModularity(filter(all, partido == "PAN"), pan.mod, pan.colors[8], pan.groups[8], 
                     "Common Last Names, Yucatan Group (PAN)")
ggsave(file.path("charts", "ver_pan.svg"), dpi = 100, width = 9.60, height = 7)


prd.mod <- unique(read.csv(file.path("gephi", "prd-modularity.csv")))
prd.groups <- 0:7
prd.colors <- c("#04b8b8", "#0431b8", 
                "#5e04b8", "#f1b606", "#b8048b", 
                "#04b831", "#b88b04", "#b80404")
drawModularity(filter(all, partido == "PRD")[, c("state_code", "paterno", "materno")], 
               prd.mod, prd.colors, prd.groups, 
               "Common Last Names, by Modularity (PRD)")
ggsave(file.path("charts", "sm_prd.svg"), dpi = 100, width = 9.60, height = 7)
drawSingleModularity(filter(all, partido == "PRD"), prd.mod, prd.colors[5], prd.groups[5], 
                     "Common Last Names, Yucatan Group (PRD)")
ggsave(file.path("charts", "yuc_prd.svg"), dpi = 100, width = 9.60, height = 7)

pri.mod <- read.csv(file.path("gephi", "pri-modularity.csv"))
pri.groups <- 0:11
pri.colors <- c("#5b5be8", "#e8a15b", 
                "#5e04b8", "#f1b606", "#5b5be8", 
                "#04b831", "#5be8a1", "#e85b5b",
                "#a15be8", "#5be8e8", "#e85ba1", "black")
drawModularity(filter(all, partido == "PRI"), pri.mod, pri.colors, pri.groups, 
               "Common Last Names, by Modularity (PRI)")
ggsave(file.path("charts", "sm_pri.svg"), dpi = 100, width = 9.60, height = 7)
drawSingleModularity(filter(all, partido == "PRI"), 
                     pri.mod, pri.colors[7], pri.groups[7], 
                     "Common Last Names, Yucatan Group (PRI)")
ggsave(file.path("charts", "yuc_pri.svg"), dpi = 100, width = 9.60, height = 7)
drawSingleModularity(filter(all, partido == "PRI"), 
                     pri.mod, pri.colors[11], pri.groups[11], 
                     "Common Last Names, Puebla Group (PRI)")
ggsave(file.path("charts", "pipope_pri.svg"), dpi = 100, width = 9.60, height = 7)
