# party colors

colors = c(
  "BALAD" = "#FF7F00",   # 18, 19     : orange
  "JL" = "#000000",      # 18         : black
  "HADASH" = "#E41A1C",  # 18, 19     : red/green -- red
  "MERETZ" = "#4DAF4A",  # 18, 19, 20 : green
  "INDEP" = "#444444",   # 18         : dark grey -- note: this is a party
  "LAB" = "#B2182B",     # 18, 19     : red
  "ZU" = "#984EA3",      # 20         : dark blue -- purple
  "HATNUAH" = "#80B1D3", # 19         : light blue [ no overlap with KULANU ]
  "KADIMA" = "#BEBADA",  # 18, 19     : dark blue -- light purple
  "KULANU" = "#80B1D3",  # 20         : light blue [ no overlap with HATNUAH ]
  "YA" = "#C51B7D",      # 19, 20     : dark blue, light blue -- magenta
  "LIKUD" = "#377EB8",   # 18, 19, 20 : blue
  "UTJ" = "#053061",     # 18, 19, 20 : dark blue
  "SHAS" = "#1B9E77",    # 18, 19, 20 : bright green or blue  -- dark green
  "JH" = "#B3DE69",      # 18, 19, 20 : dark blue/light green -- light green
  "YB" = "#01665E",      # 18, 19, 20 : teal
  "NU" = "#FDB462",      # 18         : dark blue/orange -- light orange
  "UAL" = "#FFFF33"      # 18, 19     : yellow
)

groups = c(
  "BALAD" = "Balad",
  "JL" = "Joint List",
  "HADASH" = "Hadash",
  "MERETZ" = "Meretz",
  "INDEP" = "Independence",
  "LAB" = "Labour Party",
  "ZU" = "Zionist Union",
  "HATNUAH" = "Hatnuah",
  "KADIMA" = "Kadima",
  "KULANU" = "Kulanu",
  "YA" = "Yesh Atid",
  "LIKUD" = "Likud",
  "UTJ" = "United Torah Judaism",
  "SHAS" = "Shas",
  "JH" = "The Jewish Home",
  "YB" = "Yisrael Beiteinu",
  "NU" = "National Union",
  "UAL" = "United Arab List"
)

# ParlGov Left/Right scores

scores = c(
  "BALAD" = 0.7,
  "JL" = 1, # HADASH + UAL + BALAD + Ta'al
  "HADASH" = 1.3,
  "MERETZ" = 1.8,
  "INDEP" = 3.3, # Ehud Barak, split from LAB, coded identically
  "LAB" = 3.3,
  "ZU" = 4.7, # LAB + HATNUAH / 2 (2014 onwards)
  "HATNUAH" = 6,
  "KADIMA" = 6,
  "KULANU" = 6,
  "YA" = 6,
  "LIKUD" = 6.7,
  "UTJ" = 6.9,
  "SHAS" = 7,
  "JH" = 8.6, # National Religious Party
  "YB" = 8.7,
  "NU" = 9.6,
  "UAL" = Inf # missing
)

stopifnot(names(colors) == names(groups))
stopifnot(names(colors) == names(scores))
