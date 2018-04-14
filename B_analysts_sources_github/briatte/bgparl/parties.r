# party colors

colors = c(
  "KB" = "#E41A1C",   # red
  "DPS" = "#80B1D3",  # light blue
  "A" = "#A65628",    # brown
  "NMS" = "#FFFF33",  # yellow (2005-2009 only)
  "BNS" = "#FF7F00",  # orange
  "ODS" = "#BEBADA",  # light purple
  "GERB" = "#984EA3", # purple
  "BBZ" = "#FFFF33",  # light blue, yellow -- yellow (2014 only)
  "RB" = "#053061",   # dark blue -- (2014 only)
  "SK" = "#377EB8",   # blue
  "DSB" = "#053061",  # dark blue (2005-2009 only)
  "PF" = "#01665E",    # green, red -- teal (2014 only; no cosponsorship so far)
  "RZS" = "#FDB462"   # light orange
)

# party names
groups = c(
  "KB" = "Coalition for Bulgaria", # Коалиция за България (led by Socialists)
  "DPS" = "Movement for Rights and Freedoms", # Движение за права и свободи
  "A" = "Ataka", # Атака
  "NMS" = "National Movement Simeon the Second", # Национално движение Симеон Втори
  "BNS" = "Bulgarian People's Union", # Български Народен Съюз
  "ODS" = "United Democratic Forces", # Обединени Демократични Сили
  "GERB" = "Citizens for European Development of Bulgaria", # ГЕРБ
  "BBZ" = "Bulgaria Without Censorship", # България без цензура
  "RB" = "Reformist Bloc",  # Реформаторски блок
  "SK" = "Blue Coalition", # Синята коалиция
  "DSB" = "Democrats for Strong Bulgaria", # Демократи за Силна България
  "PF" = "Patriotic Front", # Патриотичен фронт
  "RZS" = "Order, Lawfulness, Justice" # Ред, законност и справедливост
)

# ParlGov Left/Right scores

scores = c(
  "KB" = 2.9,
  "DPS" = 4.6,
  "A" = 5.5,
  "NMS" = 5.8,
  "BNS" = 5.8,
  "ODS" = 7,
  "GERB" = 7.4,
  "BBZ" = 7.4,
  "RB" = 7.4,
  "SK" = 7.4,
  "DSB" = 7.9,
  "PF" = 8.7, # both IMRO and NFSB are 8.7
  "RZS" = 8.7
)

stopifnot(names(colors) == names(groups))
stopifnot(names(colors) == names(scores))
