#--- MLB colors function --#
#'@title MLB colors
#'@description Function returns a vector of character strings from a selected MLB team's color palette. The team options are c("diamondbacks", "braves", "orioles", "red_sox", "cubs", "white_sox", "reds", "indians", "rockies", "tigers", "astros", "royals", "angels", "dodgers", "marlins", "brewers", "twins", "mets", "yankees", "athletics", "phillies", "pirates", "padres", "giants", "mariners", "cardinals", "rays", "rangers", "blue_jays", "nationals").
#'@author Charles Crabtree \email{ccrabtr@umich.edu}
#'@param set Character string denoting an MLB team's color palette.
#'@return Vector of character strings from the selected MLB team's color palette.
#'@examples
#'\dontrun{
#'# Colorado Rockies colors
#'(pal = mlb.colors("rockies"))
#'pie(rep(1, length(pal)), labels = sprintf("%d (%s)", seq_along(pal), pal), col = pal)
#'
#'# Seattle Mariners colors
#'(pal = mlb.colors("mariners"))
#'pie(rep(1, length(pal)), labels = sprintf("%d (%s)", seq_along(pal), pal), col = pal)
#'}
#'@export
#' @importFrom stats hclust dist
#' @importFrom graphics par plot rect text

mlb.colors <- function(set=c("diamondbacks", "braves", "orioles", "red_sox", "cubs",
                             "white_sox", "reds", "indians", "rockies", "tigers", "astros",
                             "royals", "angels", "dodgers", "marlins", "brewers", "twins",
                             "mets", "yankees", "athletics", "phillies", "pirates", "padres",
                             "giants", "mariners", "cardinals", "rays", "rangers", "blue_jays",
                             "nationals")) {
  # Credit to https://github.com/kbroman/broman for some of the function code.
  # MLB colors http://jim-nielsen.com/teamcolors/)

  diamonbacks = c("Red" = "#A71930",
                  "Black" = "#000000",
                  "Tan" = "#E3D4AD")

  braves = c("Red" = "#CE1141",
             "Blue" = "#13274F")

  orioles = c("Orange" = "#DF4601",
              "Black" = "#000000")

  red_sox = c("Red" = "#BD3039",
              "Blue" = "#0D2B56")

  cubs = c("Red" = "#CC3433",
           "Blue" = "#0E3386")

  white_sox = c("Black" = "#000000",
                "Silver" = "#C4CED4")

  reds = c("Red" = "#C6011F",
           "Black" = "#000000")

  indians = c("Red" = "#E31937",
              "Blue" = "#002B5C")

  rockies = c("Purple" = "#333366",
              "Black" = "#231F20",
              "Silver" = "#C4CED4")

  tigers = c("Blue" = "#0C2C56")

  astros = c("Blue" = "#002D62",
             "Orange" = "#EB6E1F")

  royals = c("Blue" = "#004687",
             "Gold" = "#C09A5B")

  angels = c("Red" = "#BA0021",
             "Blue" = "#003263")

  dodgers = c("Red" = "#EF3E42",
              "Blue" = "#005A9C")

  marlins = c("Orange" = "#FF6600",
              "Blue" = "#0077C8",
              "Yellow" = "#FFD100",
              "Black" = "#000000")

  brewers = c("Navy Blue" = "#0A2351",
              "Gold" = "#B6922E")

  twins = c("Blue" = "#002B5C",
            "Red" = "#D31145")

  mets = c("Orange" = "#FF5910",
           "Blue" = "#002D72")

  yankees = c("Red" = "#E4002B",
              "Blue" = "#003087")

  athletics = c("Green" = "#003831",
                "Yellow" = "#EFB21E")

  phillies = c("Blue" = "#284898",
               "Red" = "#E81828")

  pirates = c("Yellow" = "#FDB827",
              "Black" = "#000000")

  padres = c("Blue" = "#002D62",
             "Yellow" = "#FEC325",
             "Brown" = "#7F411C",
             "Silver" = "#A0AAB2")

  giants = c("Orange" = "#FD5A1E",
             "Black" = "#000000",
             "Gold" = "#8B6F4E")

  mariners = c("Blue" = "#0C2C56",
               "Green" = "#005C5C",
               "Silver" = "#C4CED4")

  cardinals = c("Red" = "#C41E3A",
                "Blue" = "#000066",
                "Yellow" = "#FEDB00")

  rays = c("Navy Blue" = "#092C5C",
           "Light Blue" = "#8FBCE6",
            "Yellow" = "#F5D130")

  rangers = c("Red" = "#C0111F",
              "Blue" = "#003278")

  blue_jays = c("Blue" = "#134A8E",
                "Dark Blue" = "#1D2D5C",
                "Red" = "#E8291C")

  nationals = c("Red" = "#AB0003",
                "Blue" = "#11225B")

  switch(match.arg(set),
         diamonbacks = diamonbacks,
         braves = braves,
         orioles = orioles,
         red_sox = red_sox,
         cubs = cubs,
         white_sox = white_sox,
         reds = reds,
         indians = indians,
         rockies = rockies,
         tigers = tigers,
         astros = astros,
         royals = royals,
         angels = angels,
         dodgers = dodgers,
         marlins = marlins,
         brewers = brewers,
         twins = twins,
         mets = mets,
         yankees = yankees,
         athletics = athletics,
         phillies = phillies,
         pirates = pirates,
         padres = padres,
         giants = giants,
         mariners = mariners,
         cardinals = cardinals,
         rays = rays,
         rangers = rangers,
         blue_jays = blue_jays,
         nationals = nationals)
}

