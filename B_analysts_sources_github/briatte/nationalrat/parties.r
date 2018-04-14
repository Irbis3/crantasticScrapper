# party colors

colors = c(
  "GRÜNE"    = "#4DAF4A", # green
  "SPÖ"      = "#E41A1C", # red
  "LIF"      = "#FFFF33", # yellow
  "NEOS"     = "#C51B7D", # magenta
  "STRONACH" = "#F781BF", # pink (party colors: red, white)
  "ÖVP"      = "#444444", # dark grey (party color: black)
  "FPÖ"      = "#377EB8", # blue
  "BZÖ"      = "#FF7F00"  # orange
)

# party names

groups = c(
  "GRÜNE"    = "Die Grünen",
  "SPÖ"      = "Sozialdemokratische Partei Österreichs",
  "LIF"      = "Liberales Forum",
  "NEOS"     = "Das Neue Österreich",
  "STRONACH" = "Team Stronach",
  "ÖVP"      = "Österreichische Volkspartei",
  "FPÖ"      = "Freiheitliche Partei Österreichs",
  "BZÖ"      = "Bündnis Zukunft Österreich"
)

# ParlGov Left/Right scores

scores = c(
  "GRÜNE"    = 2.5,
  "SPÖ"      = 3.7,
  "LIF"      = 4.9,
  "NEOS"     = 6,
  "STRONACH" = 6,
  "ÖVP"      = 6.5,
  "FPÖ"      = 8.3,
  "BZÖ"      = 8.8
)

stopifnot(names(colors) == names(groups))
stopifnot(names(colors) == names(scores))
