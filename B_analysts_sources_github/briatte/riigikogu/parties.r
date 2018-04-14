# party colors

colors = c(
  "K"  = "#01665E", # dark green/teal
  "SDE" = "#E41A1C", # red
  "ERL" = "#377EB8",  # blue; l. 11 only, -> EKRE
  "EER" = "#4DAF4A", # green
  "EV"  = "#80B1D3", # light blue
  "EKRE" = "#377EB8", # blue; no intersection with ERL
  "RE" = "#FFFF33", # yellow
  "IRL" = "#053061" # dark blue
)

groups = c(
  "K"  = "Eesti Keskerakond", # eesti-keskerakonna-fraktsioon
  "SDE" = "Sotsiaaldemokraatlik Erakond", # sotsiaaldemokraatliku-erakonna-fraktsioon
  "ERL" = "Eestimaa Rahvalliit",
  "EER" = "Erakonna Eestimaa Rohelised",
  "EV"  = "Eesti Vabaerakond", # eesti-vabaerakonna-fraktsioon
  "EKRE" = "Eesti Konservatiivne Rahvaerakond", # eesti-konservatiivse-rahvaerakonna-fraktsioon
  "RE" = "Eesti Reformierakond", # eesti-reformierakonna-fraktsioon
  "IRL" = "Isamaa ja Res Publica Liit" # isamaa-ja-res-publica-liidu-fraktsioon
)
# ParlGov Left/Right scores

scores = c(
  "K"  = 4,
  "SDE" = 4.2,
  "ERL" = 4.6,
  "EER" = 5.6,
  "EV"  = 7.4,
  "EKRE" = 7.4,
  "RE" = 7.9,
  "IRL" = 8.5
)

stopifnot(names(colors) == names(groups))
stopifnot(names(colors) == names(scores))
