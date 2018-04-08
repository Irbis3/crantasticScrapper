#--- NHL colors function --#
#'@title NHL colors
#'@description Function returns a vector of character strings from a selected NHL team's color palette. The team options are c("ducks", "coyotes", "bruins", "sabres", "flames", "hurricanes", "blackhawks", "avalanche", "blue_jackets", "stars", "red_wings", "oilers", "panthers", "kings", "wild", "canadiens", "predators", "devils", "islanders", "rangers", "senators", "flyers", "penguins", "sharks", "blues", "lightning", "maple_leafs", "canucks", "golden_knights", "capitals", "jets").
#'@author Charles Crabtree \email{ccrabtr@umich.edu}
#'@param set Character string denoting an NHL team's color palette.
#'@return Vector of character strings from the selected NHL team's color palette.
#'@examples
#'\dontrun{
#'# Anaheim Ducks colors
#'(pal = nhl.colors("ducks"))
#'pie(rep(1, length(pal)), labels = sprintf("%d (%s)", seq_along(pal), pal), col = pal)
#'
#'# Colorado Avalanche colors
#'(pal = nhl.colors("avalanche"))
#'pie(rep(1, length(pal)), labels = sprintf("%d (%s)", seq_along(pal), pal), col = pal)
#'}
#'@export
#' @importFrom stats hclust dist
#' @importFrom graphics par plot rect text

nhl.colors <- function(set=c("ducks", "coyotes", "bruins", "sabres", "flames", "hurricanes",
                             "blackhawks", "avalanche", "blue_jackets", "stars", "red_wings",
                             "oilers", "panthers", "kings", "wild", "canadiens", "predators",
                             "devils", "islanders", "rangers", "senators", "flyers", "penguins",
                             "sharks", "blues", "lightning", "maple_leafs", "canucks", "golden_knights",
                             "capitals", "jets")) {
  # Credit to https://github.com/kbroman/broman for some of the function code.
  # NHL colors http://jim-nielsen.com/teamcolors/)

  ducks = c("Black" = "#010101",
            "Silver" = "#A2AAAD",
            "Orange" = "#FC4C02",
            "Gold" = "#85714D")

  coyotes = c("Black" = "#010101",
              "Maroon" = "#862633",
              "Tan" = "#DDCBA4")

  bruins = c("Black" = "#010101",
             "Yellow" = "#FFB81C")

  sabres = c("Navy Blue" = "#041E42",
             "Silver" = "#A2AAAD",
             "Yellow" = "#FFB81C",
             "Red" = "#C8102E")

  flames = c("Black" = "#010101",
             "Yellow" = "#F1BE48",
             "Red" = "#C8102E")

  hurricanes = c("Black" = "#010101",
                 "Silver" = "#A2AAAD",
                 "Red" = "#C8102E")

  blackhawks = c("Black" = "#010101",
                 "Orange" = "FF671F",
                 "Yellow" = "#FFD100",
                 "Blue" = "#001871",
                 "Red" = "#C8102E",
                 "Green" = "#00843D",
                 "Gold" = "#CC8A00")

  avalanche = c("Black" = "#010101",
                "Blue" = "#236192",
                "Silver" = "#A4A9AD",
                "Purple" = "#6F263D")

  blue_jackets = c("Navy Blue" = "#041E42",
                   "Silver" = "#A4A9AD",
                   "Red" = "#C8102E")

  stars = c("Black" = "#010101",
            "Green" = "#006341",
            "Silver" = "#8A8D8F")

  red_wings = c("Red" = "#C8102E")

  oilers = c("Navy Blue" = "#00205B",
             "Orange" = "#CF4520")

  panthers = c("Navy Blue" = "#041E42",
               "Gold" = "#B9975B",
               "Red" = "#C8102E")

  kings = c("Black" = "#010101",
            "Silver" = "#A2AAAD")

  wild = c("Green" = "#154734",
           "Tan" = "#DDCBA4",
           "Yellow" = "#EAAA00",
           "Red" = "#A6192E")

  canadiens = c("Navy Blue" = "#001E62",
                "Red" = "#A6192E")

  predators = c("Navy Blue" = "#041E42",
                "Yellow" = "#FFB81C")

  devils = c("Black" = "#010101",
             "Red" = "#C8102E")

  islanders = c("Blue" = "#003087",
                "Orange" = "#FC4C02")

  rangers = c("Blue" = "#0033A0",
              "Red" = "#C8102E")

  senators = c("Black" = "#010101",
               "Red" = "#C8102E",
               "Gold" = "#C69214")

  flyers = c("Black" = "#010101",
             "Orange" = "#FA4616")

  penguins = c("Black" = "#010101",
               "Yellow" = "#FFB81C")

  sharks = c("Black" = "#010101",
             "Orange" = "#E57200",
             "Teal" = "#006272")

  blues = c("Navy Blue" = "#041E42",
            "Blue" = "#FFB81C",
            "Yellow" = "#003087")

  lightning = c("#00205B")

  maple_leafs = c("#00205B")

  canucks = c("Blue" = "#00205B",
              "Silver" = "#97999B",
              "Dark Blue" = "#041C2C")

  golden_knights = c("Black" = "#010101",
                     "Gold" = "#B4975A",
                     "Gray" = "#333F42")

  capitals = c("Navy Blue" = "#041E42",
               "Silver" = "#A2AAAD",
               "Purple" = "#782F40",
               "Red" = "#A6192E",
               "Gray" = "#53565A")

  jets = c("Navy Blue" = "#041E42",
           "Red" = "#C8102E")

  switch(match.arg(set),
         ducks = ducks,
         coyotes = coyotes,
         bruins = bruins,
         sabres = sabres,
         flames = flames,
         hurricanes = hurricanes,
         blackhawks = blackhawks,
         avalanche = avalanche,
         blue_jackets = blue_jackets,
         stars = stars,
         red_wings = red_wings,
         oilers = oilers,
         panthers = panthers,
         kings = kings,
         wild = wild,
         canadiens = canadiens,
         predators = predators,
         devils = devils,
         islanders = islanders,
         rangers = rangers,
         senators = senators,
         flyers = flyers,
         penguins = penguins,
         sharks = sharks,
         blues = blues,
         lightning = lightning,
         maple_leafs = maple_leafs,
         canucks = canucks,
         golden_knights = golden_knights,
         capitals = capitals,
         jets = jets
         )
}

