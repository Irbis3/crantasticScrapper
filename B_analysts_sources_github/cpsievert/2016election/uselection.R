# devtools::install_github("Data4Democracy/election-transparency", subdir = "r-packages/uselections")

library(uselections)
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(crosstalk)
library(htmltools)

# download area and population info from Census
if (!file.exists("LND01.xls")) {
  download.file("http://www2.census.gov/prod2/statcomp/usac/excel/LND01.xls", "LND01.xls")
}
if (!file.exists("POP01.xls")) {
  download.file("http://www2.census.gov/prod2/statcomp/usac/excel/POP01.xls", "POP01.xls")
}

# according to metadata, this is Land Area in 2010 and resident population in 2010
us_county_area <- read_excel("LND01.xls") %>%
  transmute(County = as.character(as.integer(STCOU)),
            Area = LND110210D)

us_county_population <- read_excel("POP01.xls") %>%
  transmute(County = as.character(as.integer(STCOU)),
            Population = POP010210D)

d <- PresidentialElectionResults2016 %>%
  mutate(
    # Leading 0's seems to be removed in Census codes
    County = sub("^0", "", County),
    ID = tolower(paste0(CountyName, ",", StateName))
  ) %>%
  left_join(us_county_area) %>%
  left_join(us_county_population) %>%
  mutate(
    clinton = clinton / totalvotes,
    trump = trump / totalvotes,
    johnson = johnson / totalvotes,
    stein = stein / totalvotes,
    # hickory, missouri has more votes that people?
    turnout = pmin(totalvotes / Population, 1)
  ) %>%
  select(ID, clinton:stein, turnout, Population, Area)

dmelt <- d %>%
  gather(variable, value, -ID, -turnout, -Population, -Area) %>%
  mutate(variable = factor(variable, levels = c("trump", "clinton", "johnson", "stein")))

sd1 <- SharedData$new(dmelt, ~ID, group = "A")

p1 <- ggplot(dmelt, aes(x = log(Population / Area), y = value)) + 
  geom_point(aes(text = ID),# color = turnout), 
             alpha = 0.2, data = sd1) + 
  geom_smooth(se = FALSE) + 
  facet_wrap(~variable, scales = "free") + 
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        strip.text = element_text(size = 16),
        legend.position = "none")

gg1 <- ggplotly(p1, tooltip = "text", dynamicTicks = T, 
                height = 650, width = 900) %>%
  add_annotations("log(Population / Area)", font = list(size = 20),
                  x = 0.51, y = -0.07, ax = 10, ay = -50,
                  xref = "paper", yref = "paper", showarrow = FALSE) %>%
  layout(dragmode = "zoom", margin = list(l = 55, b = 50))

# htmlwidgets::saveWidget(toWebGL(gg1), "votes.html")

d2 <- map_data('county') %>%
  mutate(ID = paste0(subregion, ",", region)) %>% 
  left_join(d)

# vars to show in the tooltip
vars <- c("ID", "trump", "clinton", "johnson", "stein")
tmp <- Map(function(x, y) paste0(x, ": ", format(y, digits = 2)), vars, d2[vars])
paster <- function(x, y) paste(x, y, sep = ",\n")
d2$txt <- Reduce(paster, tmp)

sd2 <- SharedData$new(d2, ~ID, group = "A")

p2 <- ggplot(sd2, aes(x = long, y = lat, group = group, text = txt)) + 
  geom_polygon(aes(fill = turnout)) + 
  coord_map() + labs(x = NULL, y = NULL) +
  ggthemes::theme_map()

gg2 <- ggplotly(
  p2, tooltip = c("text", "fill"), dynamicTicks = T, height = 400, 
  width = 400 * with(d2, diff(range(long)) / diff(range(lat)))
) %>% layout(dragmode = "zoom")

# htmlwidgets::saveWidget(gg2, "map.html")

#browsable(tagList(gg1, gg2))
html <- tags$div(
  style = "display: flex; flex-wrap: wrap",
  tags$div(gg1, style = "width: 50%"),
  tags$div(gg2, style = "width: 50%")
)

res <- html_print(html)

# TODO: can this be done in a standalone fashion?
file.copy(
  dir(dirname(res), full.names = TRUE), 
  "docs", overwrite = T, recursive = T
)

file.rename("docs/index.html", "docs/votes-map.html")
