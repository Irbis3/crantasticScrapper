# party colors

colors = c(
  "PP-DD" = "#984EA3",  # Partidul Poporului - Dan Diaconescu, populist, purple
  "VERZII" = "#4DAF4A", # Partidul Verde, green
  "PER" = "#4DAF4A",    # Partidul Ecologist Român, green
  "FER" = "#B3DE69",    # Federația Ecologistă din România, light green
  "PSD" = "#E41A1C",    # Partidul Social Democrat, red
  "UNPR" = "#FB8072",   # Uniunea Naţională pentru Progresul României (PSD/PNL splitters), light red
  "FSN"  = "#FF7F00",   # Frontul Salvării Naționale, orange (renamed PD, then became PD-L)
  "PD-L" = "#FF7F00",   # Partidul Democrat-Liberal, liberal-conservative, orange (ex-FSN/PD; includes FC)
  "PNTCD" = "#FFFFB3",  # Partidul Naţional Ţaranesc Creştin-Democrat, agrarian, light yellow
  "PDAR" = "#053061", # Partidul Democrat Agrar din România
  "PC" = "#80B1D3",     # Partidul Conservator (ex-PUR-SL), light blue
  "PUNR" = "#80B1D3",   # Partidul Unității Națiunii Române - now absorbed in PC, light blue
  "UDMR" = "#1B9E77",   # Uniunea Democrată Maghiară din România, liberal-conservative, dark green
  "PNL" = "#FFFF33",    # Partidul Național Liberal, yellow
  "PRM" = "#A65628",    # Partidul România Mare, extreme-right, brown
  "MIN" = "#444444",    # minorities, dark grey
  "IND" = "#AAAAAA"     # independents, light grey
)

# party names

groups = c(
  "PP-DD" = "Partidul Poporului - Dan Diaconescu",
  "VERZII" = "Partidul Verde",
  "PER" = "Partidul Ecologist Român",
  "FER" = "Federația Ecologistă din România",
  "PSD" = "Partidul Social Democrat",
  "UNPR" = "Uniunea Naţională pentru Progresul României",
  "FSN" = "Frontul Salvării Naționale",
  "PD-L" = "Partidul Democrat-Liberal",
  "PNTCD" = "Partidul Naţional Ţaranesc Creştin-Democrat",
  "PDAR" = "Partidul Democrat Agrar din România",
  "PC" = "Partidul Conservator",
  "PUNR" = "Partidul Unității Națiunii Române",
  "UDMR" = "Uniunea Democrată Maghiară din România",
  "PNL" = "Partidul Național Liberal",
  "PRM" = "Partidul România Mare",
  "MIN" = "Minoritatilor",
  "IND" = "Independent"
)

# ParlGov Left/Right scores

scores = c(
  "PP-DD" = 1.3,
  "VERZII" = 2.6,
  "PER" = 2.6,
  "FER" = 2.6,
  "PSD" = 3.2, 
  "UNPR" = 3.3,
  "FSN" = 5.4,
  "PD-L" = 5.4,
  "PNTCD" = 5.5,
  "PDAR" = 5.6,
  "PC" = 4.8,
  "PUNR" = 5.6,
  "UDMR" = 6,
  "PNL" = 6.1,
  "PRM" = 6.7,
  "MIN" = Inf,
  "IND" = Inf
)

stopifnot(names(colors) == names(groups))
stopifnot(names(colors) == names(scores))
