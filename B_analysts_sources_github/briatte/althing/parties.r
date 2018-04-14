# party colors

colors = c(
  "VG"  = "#4DAF4A", # Vinstrihreyfingin – grænt framboð -- green
  "P"   = "#444444", # Píratar                           -- dark grey
  "SF"  = "#E41A1C", # Samfylkingin-Jafnaðarmannaflokkur -- red
  "F"   = "#B3DE69", # Framsóknarflokkurinn              -- light green
  "BF"  = "#984EA3", # Björt framtíð                     -- purple
  "HR"  = "#01665E", # Borgarahreyfingin / Hreyfingin    -- teal
  "FL"  = "#80B1D3", # Frjálslyndi flokkurinn            -- light blue
  "S"   = "#377EB8", # Sjálfstæðisflokkurinn             -- blue
  "IND" = "#AAAAAA"
)

# party names

groups = c(
  "VG" = "Vinstrihreyfingin – grænt framboð",
  "P" = "Píratar",
  "SF" = "Samfylkingin-Jafnaðarmannaflokkur", # and other Social-Democratic parties before 1999
  "F" = "Framsóknarflokkurinn",
  "BF" = "Björt framtíð",
  "HR" = "Hreyfingin", # ex-Borgarahreyfingin
  "FL" = "Frjálslyndi flokkurinn",
  "S" = "Sjálfstæðisflokkurinn",
  "IND" = "independent"
)

# ParlGov Left/Right scores

scores = c(
  "VG"  = 1.2,
  "P"   = 2.6,
  "SF"  = 4.1,
  "F"   = 5,
  "BF"  = 6,
  "HR"  = 6,
  "FL"  = 6.2,
  "S"   = 7.5,
  "IND" = Inf
)

stopifnot(names(colors) == names(groups))
stopifnot(names(colors) == names(scores))
