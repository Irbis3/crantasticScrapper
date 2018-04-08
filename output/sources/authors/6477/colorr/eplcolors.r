#--- EPL colors function --#
#'@title EPL colors
#'@description Function returns a vector of character strings from a selected EPL team's color palette. The team options are c("arsenal", "bournemouth", "burnley", "chelsea", "crystal_palace", "everton", "hull", "leicester", "liverpool", "man_city", "man_united", "middlesbrough", "southampton", "stoke_city", "sunderland", "swansea_city", "tottenham", "watford", "west_bromich_albion", "west_ham").
#'@author Charles Crabtree \email{ccrabtr@umich.edu}
#'@param set Character string denoting an EPL team's color palette.
#'@return Vector of character strings from the selected EPL team's color palette.
#'@examples
#'\dontrun{
#'# Manchester United colors
#'(pal = epl.colors("man_united"))
#'pie(rep(1, length(pal)), labels = sprintf("%d (%s)", seq_along(pal), pal), col = pal)
#'
#'# Tottenham colors
#'(pal = epl.colors("tottenham"))
#'pie(rep(1, length(pal)), labels = sprintf("%d (%s)", seq_along(pal), pal), col = pal)
#'}
#'@export
#' @importFrom stats hclust dist
#' @importFrom graphics par plot rect text

epl.colors <- function(set=c("arsenal", "bournemouth", "burnley", "chelsea", "crystal_palace", "everton", "hull",
                             "leicester", "liverpool", "man_city", "man_united", "middlesbrough", "southampton",
                             "stoke_city", "sunderland", "swansea_city", "tottenham", "watford", "west_bromich_albion",
                             "west_ham")) {
  # Credit to https://github.com/kbroman/broman for some of the function code.
  # EPL colors http://jim-nielsen.com/teamcolors/)

  arsenal = c("Red" = "#D01945",
             "Blue" = "#023975",
             "Yellow" = "#FFFF00",
             "Gold" = "#A18651")

  bournemouth = c("Red" = "#BF0C10",
                  "Gold" = "#000000",
                  "Black" = "#0C0D0F")

  burnley = c("Purple" = "#70193D",
              "Light Blue" = "#93C6E0")

  chelsea = c("Blue" = "#034694",
              "Gold" = "#DBA111",
              "Red" = "#ED1D24")

  crystal_palace = c("Red" = "#C61C3A",
                     "Blue" = "#024B90")

  everton = c("Blue" = "#00369C")

  hull = c("Orange" = "#F5A12D",
           "Black" = "#000000")

  leicester = c("Dark Blue" = "#323C8B",
                "Light Blue" = "#656CA7",
                "Gold" = "#EDB32E",
                "Off White" = "#F1F2F6",
                "Black" = "#000000")

  liverpool = c("Light Red" = "#D81E2B",
                "Light Green" = "#03A389",
                "Dark Green" = "#007360",
                "Gold" = "#F9D94A",
                "Dark Red" = "#730000")

  man_city = c("Dark Blue" = "#000A23",
               "Light Blue" = "#6CAEDC",
               "Gold" = "#F0BC6E",
               "Silver" = "#F2F3F3",
               "Black" = "#0B0B0C")

  man_united = c("Red" = "#D81A23",
                 "Yellow" = "#F3CA07",
                 "Orange" = "#F4A614",
                 "Dark Orange" = "#EF8221",
                 "Black" = "#131313")

  middlesbrough = c("Red" = "#EF3E42",
                    "Black" = "#000000")

  southampton = c("Red" = "#DA272B",
                  "Blue" = "#2AA8E0",
                  "Green" = "#25A249",
                  "Gold" = "#E7B018",
                  "Black" = "#100C0C")

  stoke_city = c("Red" = "#D84041",
                 "Light Blue" = "#22499F",
                 "Dark Blue" = "#004891",
                 "Silver" = "#F0F1F1",
                 "Black" = "#000000")

  sunderland = c("Red" = "#F22337",
                 "Gold" = "#A78D39",
                 "Black" = "#151011")

  swansea_city = c("Silver" = "#F2F2F2",
                   "Black" = "#110D0D")

  tottenham = c("Silver" = "#F1F1F5",
                "Blue" = "#031C58")

  watford = c("Yellow" = "#FFF61B",
              "Red" = "#E4002D",
              "Black" = "#000000")

  west_bromich_albion = c("Red" = "#AB3C3D",
                         "Blue" = "#083272",
                         "Green" = "#04A35E",
                         "Brown" = "#735435",
                         "Silver" = "#F4F2F2")

  west_ham = c("Purple" = "#8E2E44",
               "Blue" = "#3BC5EC",
               "Gold" = "#F5CC59")

  switch(match.arg(set),
         arsenal = arsenal,
         bournemouth = bournemouth,
         burnley = burnley,
         chelsea = chelsea,
         crystal_palace = crystal_palace,
         everton = everton,
         hull = hull,
         leicester = leicester,
         liverpool = liverpool,
         man_city = man_city,
         man_united = man_united,
         middlesbrough = middlesbrough,
         southampton = southampton,
         stoke_city = stoke_city,
         sunderland = sunderland,
         swansea_city = swansea_city,
         tottenham = tottenham,
         watford = watford,
         west_bromich_albion = west_bromich_albion,
         west_ham = west_ham
         )
}

