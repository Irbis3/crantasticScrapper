# party colors

colors = c(
  "KSCM"    = "#E41A1C", # Komunistická Strana Čech a Moravy, KSČM (Communist Party), red
  "CSSD"    = "#FF7F00", # Česká strana sociálně demokratická, ČSSD (Social Democratic Party), orange
  "SPO"     = "#01665E", # Strana Práv Občanů (upper chamber only), teal
  "SZ"      = "#4DAF4A", # Strana Zelených (Greens), green
  "KDU"     = "#FFFF33", # Křesťanská a demokratická unie, (People's Party), yellow
  "PIR"     = "#000000", # black (upper chamber only)
  "NEZ"     = "#C51B7D", # magenta (upper chamber only)
  "ANO2011" = "#053061", # Akce nespokojených občanů, ANO 2011, dark blue
  "VV"      = "#80B1D3", # Věci veřejné (Public Affairs), light blue
  "SNKED"   = "#A65628", # SNK Evropští demokraté (European Democrats) (upper chamber only), brown (official: yellow)
  "4KOAL"   = "#B3DE69", # Čtyřkoalice (Four-Party Coalition), light green/olive (upper chamber only)
  "ODA"     = "#FFFFB3", # Občanská demokratická aliance, light yellow
  "US"      = "#00441b", # Unie Svobody–Demokratická unie, US–DEU (Freedom Union–Democratic Union), dark green
  "ODS"     = "#377EB8", # Občanská demokratická strana (Civic Democratic Party), blue
  "TOP09"   = "#984EA3", # Tradice Odpovědnost Prosperita, purple
  "USVIT"   = "#B3DE69", # Úsvit přímé demokracie, light green (lower chamber only)
  "SPR"     = "#444444", # Republikáni Miroslava Sládka, dark grey (lower chamber only)
  "NK"      = "#444444", # Nestraníci (Independents), dark grey (upper chamber only)
  "IND"     = "#AAAAAA"  # unaffiliated, light grey (not used in the lower chamber)
)

groups = c(
  "KSCM" = "Komunistická strana",
  "CSSD" = "Česká strana sociálně demokratická",
  "SPO" = "Strana Práv Občanů",
  "SZ" = "Strana zelených",
  "KDU" = "Křesťanská a demokratická unie",
  "PIR" = "Česká pirátská strana",
  "NEZ" = "NEZÁVISLÍ",
  "ANO2011" = "Akce nespokojených občanů", # new in leg. 7
  "VV" = "Věci veřejné",
  "SNKED" = "SNK Evropští demokraté",
  "4KOAL" = "Čtyřkoalice",
  "ODA" = "Občanská demokratická aliance",
  "US" = "Unie Svobody–Demokratická unie",
  "ODS" = "Občanská demokratická strana",
  "TOP09" = "Tradice Odpovědnost Prosperita",
  "USVIT" = "Úsvit přímé demokracie", # new in leg. 7
  "SPR" = "Republikáni Miroslava Sládka",
  "NK"  = "Nestraníci",
  "IND" = "independent"
)

# ParlGov Left/Right scores

scores = c(
  "KSCM"  = 0.7,
  "CSSD"  = 3,
  "SPO"   = 3.3,
  "SZ"    = 4.1,
  "KDU"   = 5.8,
  "PIR"   = 5, # (SZ + KDU) / 2 (coalition that elected a single Pir. candidate)
  "NEZ"   = 5.5,
  "ANO2011" = 6,
  "VV"    = 6,
  "SNKED" = 6.1,
  "4KOAL" = 6.7, # upper chamber coalition; (KDU + ODA + US) / 3
  "ODA"   = 7.1,
  "US"    = 7.2,
  "ODS"   = 7.4,
  "TOP09" = 7.4,
  "USVIT" = 7.4,
  "SPR"   = 9.8,
  "NK"    = Inf,
  "IND" = Inf
)

stopifnot(names(colors) == names(groups))
stopifnot(names(colors) == names(scores))
