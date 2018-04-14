# party colors

colors = c(
  "AAA" = "#FFFF33", # Anti Austerity All. -- yellow (D31, no overlap)
  "SOC" = "#E41A1C", # Socialist Party     -- red
  "SF"  = "#B3DE69", # Sinn Féin           -- light green
  "WP"  = "#B2182B", # Workers' Party      -- dark red (not used)
  "DL"  = "#FFFF33", # Democratic Left     -- yellow (D28, no overlap)
  "GP"  = "#4DAF4A", # Green Party         -- green
  "LAB" = "#F781BF", # Labour              -- pink
  "FF"  = "#FF7F00", # Fianna Fáil         -- orange
  "FG"  = "#377EB8", # Fine Gael           -- blue
  "RENUA"   = "#80B1D3", # RENUA Ireland   -- light blue
  "PD"  = "#053061", # Progressive Dem.    -- dark blue
  "CHAIR" = "#444444", # chamber chair        -- dark grey (not used)
  "IND" = "#AAAAAA"  # unaffiliated        -- light grey
)

groups = c(
  "AAA" = "Anti Austerity Alliance",
  "SOC" = "Socialist Party",
  "SF"  = "Sinn Féin",
  "WP"  = "Workers' Party", # not used
  "DL"  = "Democratic Left",
  "GP"  = "Green Party",
  "LAB" = "Labour Party",
  "FF"  = "Fianna Fáil",
  "FG"  = "Fine Gael",
  "RENUA" = "RENUA Ireland",
  "PD" = "Progressive Democrats",
  "CHAIR" = "chamber chair",
  "IND" = "independent")

# ParlGov Left/Right scores

scores = c(
  "AAA" = 1.3, # ran SOC councillors, coded identically
  "SOC" = 1.3,
  "SF"  = 1.3,
  "WP"  = 1.3, # not used
  "DL"  = 1.9,
  "GP"  = 2.4,
  "LAB" = 3.6,
  "FF"  = 6.1,
  "FG"  = 6.4,
  "RENUA" = 6.4, # expelled members of FG, coded identically
  "PD" = 8,
  "CHAIR" = Inf,
  "IND" = Inf
)

stopifnot(names(colors) == names(groups))
stopifnot(names(colors) == names(scores))
