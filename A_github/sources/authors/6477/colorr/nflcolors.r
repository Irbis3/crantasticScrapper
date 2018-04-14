#--- NFL colors function --#
#'@title NFL colors
#'@description Function returns a vector of character strings from a selected NFL team's color palette. The team options are c("cardinals", "falcons", "ravens", "bills", "panthers", "bears", "bengals", "browns", "cowboys", "broncos", "lions", "packers", "texans", "colts", "jaguars", "chiefs", "chargers", "rams", "dolphins", "vikings", "patriots", "saints", "giants", "jets", "raiders", "eagles", "steelers", "niners", "seahawks", "buccaneers", "titans", "redskins").
#'@author Charles Crabtree \email{ccrabtr@umich.edu}
#'@param set Character string denoting an NFL team's color palette.
#'@return Vector of character strings from the selected NFL team's color palette.
#'@examples
#'\dontrun{
#'# Detroit Lions colors
#'(pal = nfl.colors("lions"))
#'pie(rep(1, length(pal)), labels = sprintf("%d (%s)", seq_along(pal), pal), col = pal)
#'
#'# Detroit Broncos colors
#'(pal = epl.colors("broncos"))
#'pie(rep(1, length(pal)), labels = sprintf("%d (%s)", seq_along(pal), pal), col = pal)
#'}
#'@export
#' @importFrom stats hclust dist
#' @importFrom graphics par plot rect text

nfl.colors <- function(set=c("cardinals", "falcons", "ravens", "bills", "panthers", "bears",
                             "bengals", "browns", "cowboys", "broncos", "lions", "packers",
                             "texans", "colts", "jaguars", "chiefs", "chargers", "rams",
                             "dolphins", "vikings", "patriots", "saints", "giants", "jets",
                             "raiders", "eagles", "steelers", "niners", "seahawks", "buccaneers",
                             "titans", "redskins")) {
  # Credit to https://github.com/kbroman/broman for some of the function code.
  # NFL colors http://jim-nielsen.com/teamcolors/)

  cardinals = c("Burgundy" = "#97233F",
           "Black" = "#000000",
           "Yellow" = "#FFB612",
           "Silver" = "#A5ACAF")

  falcons = c("Red" = "#A71930",
              "Black" = "#000000",
              "Silver" = "#A5ACAF")

  ravens = c("Purple" = "#241773",
             "Black" = "#000000",
             "Gold" = "#9E7C0C",
             "Red" = "#C60C30")

  bills = c("Blue" = "#00338D",
            "Red" = "#C60C30")

  panthers = c("Blue" = "#0085CA",
               "Black" = "#000000",
               "Silver" = "#BFC0BF")

  bears = c("Navy Blue" = "#0B162A",
            "Orange" = "#C83803")

  bengals = c("Black" = "#000000",
              "Orange" = "#FB4F14")

  browns = c("Orange" = "#FB4F14",
             "Brown" = "#22150C",
             "Silver" = "#A5ACAF")

  cowboys = c("Dark Blue" = "#002244",
              "Light Silver" = "#B0B7BC",
              "Light Blue" = "#ACC0C6",
              "Silver" = "#A5ACAF",
              "Blue" = "#00338D",
              "Black" = "#000000")

  broncos = c("Navy Blue" = "#002244",
              "Orange" = "#FB4F14")

  lions = c("Blue" = "#005A8B",
            "Silver" = "#B0B7BC",
            "Black" = "#000000")

  packers = c("Dark Green" = "#203731",
              "Yellow" = "#FFB612")

  texans = c("Navy Blue" = "#03202F",
             "Red" = "#A71930")

  colts = c("Blue" = "#002C5F",
            "Silver" = "#A5ACAF")

  jaguars = c("Black" = "#000000",
              "Teal" = "#006778",
              "Dark Gold" = "#9F792C",
              "Light Gold" = "#D7A22A")

  chiefs = c("Red" = "#E31837",
             "Yellow" = "#FFB612",
             "Black" = "#000000")

  chargers = c("Navy Blue" = "#002244",
               "Light Blue" = "#0073CF",
               "Yellow" = "#FFB612")

  rams = c("Navy Blue" = "#002244",
           "Gold" = "#B3995D")

  dolphins = c("Teal" = "#008E97",
               "Orange" = "#F58220",
               "Blue" = "#005778")

  vikings = c("Purple" = "#4F2683",
              "Yellow" = "#FFC62F",
              "Tan" = "#E9BF9B",
              "Black" = "#000000")

  patriots = c("Navy Blue" = "#02244",
               "Red" = "#C60C30",
               "Silver" = "#B0B7BC")

  saints = c("Gold" = "#9F8958",
             "Black" = "#000000")

  giants = c("Blue" = "#0B2265",
             "Red" = "#A71930",
             "Silver" = "#A5ACAF")

  jets = c("Green" = "#203731")

  raiders = c("Silver" = "#A5ACAF",
              "Black" = "#000000")

  eagles = c("Dark Green" = "#004953",
             "Silver" = "#A5ACAF",
             "Light Blue" = "#ACC0C6",
             "Black" = "#000000",
             "Dark Silver" = "#565A5C")

  steelers = c("Black" = "#000000",
               "Yellow" = "#FFB612",
               "Red" = "#C60C30",
               "Blue" = "#00539B",
               "Silver" = "#A5ACAF")

  niners = c("Red" = "#AA0000",
             "Gold" = "#B3995D",
             "Black" = "#000000",
             "Silver" = "#A5ACAF")

  seahawks = c("Blue" = "#002244",
               "Green" = "#69BE28",
               "Silver" = "#A5ACAF")

  buccaneers = c("Red" = "#D50A0A",
                 "Dark Silver" = "#34302B",
                 "Black" = "#000000",
                 "Orange" = "#FF7900",
                 "Silver" = "#B1BABF")

  titans = c("Navy Blue" = "#002244",
             "Bright Blue" = "#4B92DB",
             "Red" = "#C60C30",
             "Silver" = "#A5ACAF")

  redskins = c("Merlot" = "#773141",
               "Yellow" = "#FFB612",
               "Black" = "#000000",
               "Dark Merlot" = "#5B2B2F")

  switch(match.arg(set),
         cardinals = cardinals,
         falcons = falcons,
         ravens = ravens,
         bills = bills,
         panthers = panthers,
         bears = bears,
         bengals = bengals,
         browns = browns,
         cowboys = cowboys,
         broncos = broncos,
         lions = lions,
         packers = packers,
         texans = texans,
         colts = colts,
         jaguars = jaguars,
         chiefs = chiefs,
         chargers = chargers,
         rams = rams,
         dolphins = dolphins,
         vikings = vikings,
         patriots = patriots,
         saints = saints,
         giants = giants,
         jets = jets,
         raiders = raiders,
         eagles = eagles,
         steelers = steelers,
         niners = niners,
         seahawks = seahawks,
         buccaneers = buccaneers,
         titans = titans,
         redskins = redskins)
}

