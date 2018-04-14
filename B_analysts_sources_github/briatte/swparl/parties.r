# party groups

groups = c(
  "PDT" = "Labour Party",
  "FRAP" = "Frauen Macht Politik!", # single member, 1999 and 2003, now in PSS
  "PES" = "Green Party",
  "PSS" = "Socialist Party",
  "PVL" = "Green Liberal Party",
  "ADI" = "Alliance of Independents", # Migros, dissolved to red-green
  "PDC" = "Christian Democratic People's Party",
  "PEV" = "Evangelical People's Party",
  "PCS" = "Christian Social Party",
  "PLR" = "Free Democratic Party",
  "PLD" = "Liberal Party",
  "UDC" = "Swiss People's Party",
  "PBD" = "Conservative Democratic Party",
  "FPS" = "Freedom Party", # now in FDP-The Liberals
  "UDF" = "Federal Democratic Union",
  "LEGA" = "Ticino League",
  "MCG" = "Geneva Citizens Movement",
  "DS"  = "Swiss Democrats",
  "IND" = "independent"
)

# party colors

colors = c(
  "PDT" = "#B2182B",      # dark red
  "FRAP" = "#F781BF",     # pink
  "PES" = "#4DAF4A",      # green
  "PSS" = "#E41A1C",      # red
  "PVL" = "#B3DE69",      # light green
  "ADI" = "#444444",      # dark grey
  "PDC" = "#FDB462",      # light brown -- light orange
  "PEV" = "#FFFFB3",      # light yellow
  "PCS" = "#01665E",      # teal
  "PLR" = "#377EB8",      # blue
  "PLD" = "#053061",      # dark blue
  "UDC" = "#00441B",      # green -- very dark green
  "PBD" = "#FFFF33",      # yellow
  "FPS" = "#A65628",      # brown
  "UDF" = "#C51B7D",      # magenta
  "LEGA" = "#80B1D3",     # light blue
  "MCG" = "#FF7F00",      # yellow/red -- orange
  "DS"  = "#000000",      # black
  "IND" = "#AAAAAA"       # light grey
)

# ParlGov Left/Right scores

scores = c(
  "PDT" = 0.5,
  "FRAP" = 1.3,
  "PES" = 1.7,
  "PSS" = 1.8,
  "PVL" = 2.6,
  "ADI" = 3.3,
  "PDC" = 4.7,
  "PEV" = 4.9,
  "PCS" = 6.2,
  "PLR" = 6.3,
  "PLD" = 7.3,
  "UDC" = 7.4,
  "PBD" = 7.4,
  "FPS" = 8.1,
  "UDF" = 8.7,
  "LEGA" = 8.7,
  "MCG" = 8.7,
  "DS" = 9.4,
  "IND" = Inf
)

stopifnot(names(colors) == names(groups))
stopifnot(names(colors) == names(scores))
