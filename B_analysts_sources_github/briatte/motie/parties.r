# party colors

colors = c(
  "SP"     = "#B2182B", # dark red
  "GL"     = "#B3DE69", # light green
  "PVDA"   = "#E41A1C", # red
  "D66"    = "#01665E", # teal
  "CDA"    = "#4DAF4A", # green
  "50PLUS" = "#984EA3", # purple; n = 1
  "CU"     = "#80B1D3", # light blue
  "VVD"    = "#FF7F00", # orange
  "SGP"    = "#377EB8", # blue
  "PVV"    = "#FFFF33", # yellow
  "GRBVK"  = "#F781BF", # pink (arbitrary); n = 2
  "GRKO"   = "#C51B7D", # magenta (arbitrary); n = 2
  "PVDD"   = "#444444", # dark grey; n = 2
  "IND"    = "#AAAAAA"  # light grey; used for Houwers, Klein and Van Vliet
)

# ParlGov Left/Right scores

scores = c(
  "SP"     = 1.2,
  "GL"     = 2.0,
  "PVDA"   = 3.6,
  "D66"    = 4.5,
  "CDA"    = 5.9,
  "50PLUS" = 6.0, # n = 1
  "CU"     = 6.2,
  "VVD"    = 7.3,
  "SGP"    = 8.8,
  "PVV"    = 8.8,
  "GRBVK"  = Inf, # two-person group, missing, n = 2
  "GRKO"   = Inf, # two-person group, missing, n = 2
  "PVDD"   = Inf, # special interest group, n = 2
  "IND"    = Inf  # used for Houwers, Klein and Van Vliet (individuals)
)

stopifnot(names(colors) == names(scores))
