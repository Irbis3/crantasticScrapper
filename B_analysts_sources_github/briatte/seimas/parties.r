# party colors

colors = c(
  "LSDP" = "#E41A1C", # red       all
  "LDDP" = "#B2182B", # d. red    1992, 1996, 2000
  "LVZS" = "#4DAF4A", # green     1996-
  "NDP" = "#B3DE69",  # l. green  1996, 2000
  "DP" = "#053061",   # d. blue  2004-
  "LLRA" = "#FB8072", # light red all
  "NS" = "#377EB8",   # blue      2000, 2004, 2008
  "LCS" = "#F781BF",  # pink      1992, 1996, 2000
  "TT" = "#FFFF33",   # yellow    2004-
  "LKDP" = "#FFFFB3", # l. yellow 1992 only
  "KDS" = "#FFFF33",  # yellow    1992, 1996, 2000 (stops before TT starts)
  "LLIS" = "#01665E", # d. green  1996, 2000 (stops before LICS starts)
  "NKS" = "#FDB462",  # l. orange 2000 only
  "SK" = "#80B1D3",   # l. blue   1992 only (stops before TS-LKD starts)
  "TS-LKD" = "#80B1D3", # l. blue 1996-
  "LLAS" = "#FDB462", # l. orange 2000 only
  "LS" = "#FF7F00",   # orange    2008, 2012
  "LICS" = "#01665E", # d. green  2004, 2008
  "LTS" = "#C51B7D",  # magenta   1992, 1996
  "TPP" = "#A65628",  # brown     2008 only
  "JL" = "#984EA3",   # purple    1992, 1996, 2000
  "LPKTS" = "#BEBADA", # l. purple 1992, 1996
  "DK" = "#444444",    # d. grey  2012 only
  "IND" = "#AAAAAA"
)

groups = c(
  "LSDP" = "Lietuvos socialdemokratų partija", # Soc-Dem
  "LDDP" = "Lietuvos demokratinė darbo partija", # Democratic Labour Party
  "LVZS" = "Lietuvos valstiečių ir žaliųjų sąjunga", # Peasants and Greens
  "NDP" = "Naujosios demokratijos partija", # New Democratic Party
  "DP" = "Darbo partija",    # Labour Party
  "LLRA" = "Lietuvos lenkų rinkimų akcija",    # Poles
  "NS" = "Naujoji sąjunga (socialliberalai)", # New Union
  "LCS" = "Lietuvos centro sąjunga", # Centre Union of Lithuania
  "TT" = "Tvarka ir teisingumas", # Order & Justice
  "LKDP" = "Lietuvos Krikščionių demokratų partijos", # Lithuanian Christian Democrats
  "KDS" = "Krikščionių demokratų sąjunga", # Christian Democratic Union
  "LLIS" = "Lietuvos liberalų sąjunga", # Liberal Union of Lithuania
  "NKS" = "Nuosaikiųjų konservatorių sąjunga", # Moderate Conservative Union
  "SK" = "Sąjūdžio koalicija", # Sajudis coalition
  "TS-LKD" = "Tėvynės sąjunga - Lietuvos krikščionys demokratai", # Chr-Dems
  "LLAS" = "Lietuvos laisvės sąjunga", # Lithuanian Liberty Union
  "LS" = "Liberalų sąjūdis", # Liberal Movement
  "LICS" = "Liberalų ir centro sąjunga", # Liberal/Centre Union, yellow and blue
  "LTS" = "Lietuvių tautininkų sąjunga", # Lithuanian National Union List
  "TPP" = "Tautos prisikėlimo partija", # National Resurrection Party, orange and black
  "JL" = "Jaunosios Lietuvos", # Young Lithuania nationalists
  "LPKTS" = "Lietuvos politinių kalinių ir terminių sąjunga", # Union of Lith. Polit. Prisoners and Deportees
  "DK" = "Drąsos kelias", # Way of Courage
  "IND" = "independent"
)

# ParlGov Left/Right scores

scores = c(
  "LSDP" = 3.2,
  "LDDP" = 3.3,
  "LVZS" = 3.3,
  "NDP" = 3.3, # 1261; coded as LVLS/LVZS
  "DP" = 3.9,
  "LLRA" = 3.9,
  "NS" = 4.3,
  "LCS" = 4.9, # 887
  "TT" = 5.3,
  "LKDP" = 6.2, # 675
  "KDS" = 6.2, # 493
  "LLIS" = 6.8, # 378
  "NKS" = 7.4, # 709; coded as TS-LK, which it split from
  "SK" = 7.4, # 1045; coded as TS-LK, which largely absorbed it
  "TS-LKD" = 7.4,
  "LLAS" = 7.8, # 1562
  "LS" = 7.8,
  "LICS" = 7.8,
  "LTS" = 8.7, # 432
  "TPP" = 8.7,
  "JL" = 9.8, # 383
  "LPKTS" = 9.8, # 1447
  "DK" = Inf, # missing
  "IND" = Inf # incl. single Ind. party member
)

stopifnot(names(colors) == names(groups))
stopifnot(names(colors) == names(scores))

