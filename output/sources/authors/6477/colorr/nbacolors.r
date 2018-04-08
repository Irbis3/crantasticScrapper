#--- NBA colors function --#
#'@title NBA colors
#'@description Function returns a vector of character strings from a selected NBA team's color palette. The team options are c("hawks", "hornets", "mavs", "warriors", "clippers", "heat", "pelicans", "magic", "blazers", "raptors", "celtics", "bulls", "nuggets", "rockets", "lakers", "bucks", "knicks", "sixers", "kings", "jazz", "nets", "cavs", "pistons", "pacers", "grizzlies", "wolves", "thunder", "suns", "spurs", "wizards", "sonics")
#'@author Charles Crabtree \email{ccrabtr@umich.edu}
#'@param set Character string denoting an NBA team's color palette.
#'@return Vector of character strings from the selected NBA team's color palette.
#'@examples
#'\dontrun{
#'# Detroit Pistons colors
#'(pal = nba.colors("pistons"))
#'pie(rep(1, length(pal)), labels = sprintf("%d (%s)", seq_along(pal), pal), col = pal)
#'
#'# Milwaukee Bucks colors
#'(pal = nba.colors("bucks"))
#'pie(rep(1, length(pal)), labels = sprintf("%d (%s)", seq_along(pal), pal), col = pal)
#'}
#'@export
#' @importFrom stats hclust dist
#' @importFrom graphics par plot rect text

nba.colors <- function(set=c("hawks", "hornets", "mavs", "warriors", "clippers", "heat", "pelicans",
                              "magic", "blazers", "raptors", "celtics", "bulls", "nuggets", "rockets",
                              "lakers", "bucks", "knicks", "sixers", "kings", "jazz", "nets", "cavs", "pistons",
                              "pacers", "grizzlies", "wolves", "thunder", "suns", "spurs", "wizards", "sonics")) {
  # Credit to https://github.com/kbroman/broman for some of the function code.
  # NBA colors (http://teamcolorcodes.com/)

  hawks = c("Hawks Red" = "#E03A3E",
            "Green Volt" = "#C3D600",
            "White" = "#FFFFFF",
            "Black" = "#000000")

  hornets = c("Hornets Purple" = "#1D1160",
              "Teal" = "#008CA8",
              "Gray" = "#A1A1A4")

  mavs = c("Mavericks Blue" = "#007DC5",
           "Mavericks Silver" = "#C4CED3",
           "Mavericks Navy" = "#20385B",
           "Black" = "#000000")

  warriors = c("Warriors Royal Blue" = "#006BB6",
               "Golden Yellow" = "#FDB927")

  clippers = c("Clippers Red" = "#ED174C",
               "Royal Blue" = "#006BB6",
               "Gray" = "#A1A1A4",
               "Navy" = "#00285D")

  heat = c("Heat Red" = "#98002E",
           "Heat Yellow" = "#F9A01B",
           "Black" = "#000000")

  pelicans = c("Pelicans Blue" = "#002B5C",
               "Pelicans Gold" = "#B4975A",
               "Pelicans Red" = "#E31937")

  magic = c("Magic Blue" = "#007DC5",
            "Silver" = "#C4CED3",
            "Black" = "#000000")

  blazers = c("Blazers Red" = "#F0163A",
              "Silver" = "#B6BFBF",
              "Black" = "#000000")

  raptors = c("Raptors Red" = "#CE1141",
              "Raptors Silver" = "#C4CED3",
              "Black" = "#000000")

  celtics = c("Celtics Green" = "#008348",
              "Gold" = "#FFD700",
              "Silver" = "#C0C0C0",
              "Black" = "#000000")

  bulls = c("Bulls Red" = "#CE1141",
            "Black" = "#000000")

  nuggets = c("Nuggets Light Blue" = "#4FA8FF",
              "Nuggets Gold" = "#FFB20F",
              "Nuggets Navy" = "#004770")

  rockets = c("Rockets Red" = "#CE1141",
              "Silver" = "#C4CED3",
              "Mustard" = "#FDB927",
              "Black" = "#000000")

  lakers = c("Lakers Purple" = "#552582",
              "Lakers Gold" = "#FDB927")

  bucks = c("Dark Green" = "#00471B",
            "Bucks Cream" = "#EEE1C6",
            "Light Royal Blue" = "#0077C0",
            "Black" = "#000000")

  knicks = c("Knicks Blue" = "#006BB6",
             "Orange" = "#F58426",
             "Silver" = "#BEC0C2")

  sixers = c("76ers Blue" = "#006BB6",
             "76ers Red" = "#ED174C")

  kings = c("Kings Purple" = "#724C9F",
            "Kings Silver" = "#8E9090",
            "Black" = "#000000")

  jazz = c("Jazz Navy" = "#002B5C",
           "Jazz Yellow" = "#F9A01B",
           "Jazz Green" = "#00471B",
           "Jazz Gray" = "#BEC0C2")

  nets = c("Black" = "#000000")

  cavs = c("Cavaliers Wine" = "#860038",
           "Cavaliers Gold" = "#FDBB30",
           "Cavaliers Navy" = "#002D62")

  pistons = c("Pistons Blue" = "#006BB6",
              "Pistons Red" = "#ED174C",
              "Pistons Navy" = "#001F70")

  pacers = c("Pacers Blue" = "#00275D",
             "Gold" = "#FFC633",
             "Silver" = "#BEC0C2")

  grizzlies = c("Memphis Midnight Blue" = "#23375B",
                "Beale Street Blue" = "#6189B9",
                "Smoke Blue" = "#BBD1E4",
                "Grizzlies Gold" = "#FFD432")

  wolves = c("Timberwolves Blue" = "#005083",
             "Silver" = "#C4CED3",
             "Green" = "#00A94F",
             "Black" = "#000000")

  thunder = c("Thunder Blue" = "#007DC3",
              "Orange" = "#F05133",
              "Yellow" = "#FDBB30",
              "Dark Blue" = "#002D62")

  suns = c("Suns Orange" = "#E56020",
           "Suns Purple" = "#1D1160",
           "Suns Gray" = "#63717A",
           "Black" = "#000000")

  spurs = c("Silver" = "#B6BFBF",
            "Black" = "#000000")

  wizards = c("Navy" = "#002566",
              "Red" = "#F5002F",
              "Silver" = "#C2CCCC")

  sonics = c("Sonics Green" = "#016332",
             "Sonics Yellow" = "#FCE10C")

  switch(match.arg(set),
         hawks = hawks,
         hornets = hornets,
         mavs = mavs,
         warriors = warriors,
         clippers = clippers,
         heat = heat,
         pelicans = pelicans,
         magic = magic,
         blazers = blazers,
         raptors = raptors,
         celtics = celtics,
         bulls = bulls,
         nuggets = nuggets,
         rockets = rockets,
         lakers = lakers,
         bucks = bucks,
         knicks = knicks,
         sixers = sixers,
         kings = kings,
         jazz = jazz,
         nets = nets,
         cavs = cavs,
         pistons = pistons,
         pacers = pacers,
         grizzlies = grizzlies,
         wolves = wolves,
         thunder = thunder,
         suns = suns,
         spurs = spurs,
         wizards = wizards,
         sonics = sonics
         )
}

