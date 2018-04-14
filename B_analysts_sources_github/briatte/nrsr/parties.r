# party colors

colors = c(
  "KSS"      = "#B2182B", # red                   -- dark red [ 3 ]
  "SDL"      = "#FB8072", # mostly red            -- light red [ 2 ]
  "SOP"      = "#F781BF", #                       -- pink [ 2 ]
  "SMER-SD"  = "#E41A1C", # red, dark green       -- red [ 2, 3, 4, 5, 6 ]
  "HZDS"     = "#C51B7D", # white, dark blue, red -- magenta [ 2, 3, 4 ]
  "SAS"      = "#01665E", # light green, blues    -- teal [ 5, 6 ]
  "SMK-MKP"  = "#4DAF4A", # red, white, green     -- green [ 2, 3, 4 ]
  "SNS"      = "#80B1D3", # white, blue, red      -- light blue [ 2, 3, 4, 5 ]
  "KDH"      = "#377EB8", # white, red, blue      -- blue [ 2, 3, 4, 5, 6 ]
  "ANO"      = "#FFFF33", # blue, yellow          -- yellow [ 2, 3 ]
  "SDKU-DS"  = "#053061", # dark blue             -- dark blue [ 2, 3, 4, 5, 6 ]
  "MOST-HID" = "#FF7F00", # orange                -- orange [ 5, 6 ]
  "OLANO"    = "#B3DE69"  #                       -- light green [6]
)

# party names

groups = c(
  "KSS" = "Komunistická strana Slovenska",
  "SDL" = "Strana demokratickej ľavice",
  "SOP" = "Strana občianskeho porozumenia",
  "SMER-SD" = "Smer - sociálna demokracia", # incl. Smer
  "HZDS" = "Hnutie za demokratické Slovensko", # incl. ĽS-HZDS - Ľudová strana - HZDS
  "SAS" = "Sloboda a Solidarita",
  "SMK-MKP" = "Strana maďarskej komunity - Magyar Közösség Pártja", # incl. SMK
  "SNS" = "Slovenská národná strana",
  "KDH" = "Kresťanskodemokratické hnutie",
  "ANO" = "Aliancia Nového Občana",
  "SDKU-DS" = "Slovenská demokratická a kresťanská únia – Demokratická strana", # incl. SDK, SDKÚ, SDKÚ–DS
  "MOST-HID" = "Most–Híd",
  "OLANO" = "Obyčajní ľudia a nezávislé osobnosti"
)

# ParlGov Left/Right scores

scores = c(
  "KSS" = 0.5,
  "SDL" = 3.2, # score of SDL (SDL 2005 = 3.3)
  "SOP" = 3.3,
  "SMER-SD" = 3.4,
  "HZDS" = 4.9,
  "SAS" = 6,
  "SMK-MKP" = 6.5, # ParlGov 12-10
  "SNS" = 7.0,
  "KDH" = 7.1,
  "ANO" = 7.2,
  "SDKU-DS" = 7.3, # mean of SDK and SDKU-DS (7.2, 7.4)
  "MOST-HID" = 7.4,
  "OLANO" = 7.4
)

stopifnot(names(colors) == names(groups))
stopifnot(names(colors) == names(scores))
