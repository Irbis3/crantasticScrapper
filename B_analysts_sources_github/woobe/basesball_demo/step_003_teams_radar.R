# Radar Charts

# Note: Run 'step_001_data_munging.R' first ...


# ------------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------------

suppressPackageStartupMessages(library(highcharter))


# ------------------------------------------------------------------------------
# Overall Team Stats
# ------------------------------------------------------------------------------

# Function to extract data
create_team_overall_df <- function(team = "Boston Red Sox",
                                   yearID = 2016) {

  # Extract
  df1 = d_teams[which(d_teams$name == team),]
  df1 = df1[which(df1$yearID == yearID),]

  # Reformat
  df2 = data.frame(Games = df1$G,
                   Games_Played_at_Home = df1$Ghome,
                   Losses = df1$L,
                   Wins = df1$W
                   )
  df3 = data.frame(name = colnames(df2), value = round(t(df2),3))

  # Return
  return(df3)


}

# Create subsets
df_2014 = create_team_overall_df("Boston Red Sox", 2014)
df_2015 = create_team_overall_df("Boston Red Sox", 2015)
df_2016 = create_team_overall_df("Boston Red Sox", 2016)

# Title Text
txt_title = "AL Div. E Boston Red Sox 2014 - 2016 Team Stats"

# Create Polar Chart
p1 = highchart() %>%
  hc_title(text = txt_title,
           style = list(fontSize = "15px")) %>%
  hc_chart(type = "", polar = TRUE) %>%
  hc_xAxis(categories = df_2016$name) %>%
  hc_add_series(name = "2014", df_2014$value,
                showInLegend = FALSE, color = "red") %>%
  hc_add_series(name = "2015", df_2015$value,
                showInLegend = FALSE, color = "steelblue") %>%
  hc_add_series(name = "2016", df_2016$value,
                showInLegend = FALSE, color = "darkorange")
print(p1)

# ------------------------------------------------------------------------------
# Offensive Team Stats
# ------------------------------------------------------------------------------

# Function to extract data
create_team_off_df <- function(team = "Boston Red Sox",
                                   yearID = 2016) {

  # Extract
  df1 = d_teams[which(d_teams$name == team),]
  df1 = df1[which(df1$yearID == yearID),]

  # Reformat
  df2 = data.frame(
    Runs_Scored = df1$R,
    # At_Bats = df1$AB,
    Hits_by_Batters = df1$H,
    Doubles = df1$'2B',
    Triples = df1$'3B',
    Homeruns_by_Batters = df1$HR,
    Walks_by_Batters = df1$BB,
    Strikeouts_by_Batters = df1$SO,
    Stolen_Bases = df1$SB,
    Caught_Stealing = df1$CS,
    Batters_Hit_by_Pitch = df1$HBP,
    Sacrifice_Flies = df1$SF)

  df3 = data.frame(name = colnames(df2), value = round(t(df2),3))

  # Return
  return(df3)

}

# Create Subsets
df_off_redsox = create_team_off_df("Boston Red Sox", 2016)
df_off_twins = create_team_off_df("Minnesota Twins", 2016)


# Title Text
txt_title = "2016 Offensive Stats (Red Sox vs Twins)"

# Create Polar Chart
p2 = highchart() %>%
  hc_title(text = txt_title,
           style = list(fontSize = "15px")) %>%
  hc_chart(type = "", polar = TRUE) %>%
  hc_xAxis(categories = df_off_redsox$name) %>%
  hc_add_series(name = "Boston Red Sox", df_off_redsox$value,
                showInLegend = FALSE, color = "red") %>%
  hc_add_series(name = "Minnesota Twins", df_off_twins$value,
                showInLegend = FALSE, color = "steelblue")
print(p2)


# ------------------------------------------------------------------------------
# Defensive Team Stats
# ------------------------------------------------------------------------------

# Function to extract data
create_team_def_df <- function(team = "Boston Red Sox",
                               yearID = 2016) {

  # Extract
  df1 = d_teams[which(d_teams$name == team),]
  df1 = df1[which(df1$yearID == yearID),]

  # Reformat
  df2 = data.frame(
    Opponents_Runs_Scored = df1$RA,
    Earned_Runs_Allowed = df1$ER,
    Earned_Run_Average = df1$ERA,
    Complete_Games = df1$CG,
    Shutouts = df1$SHO,
    Saves = df1$SV,
    #Outs_Pitched = df1$IPOuts, # error I can't figure out why
    Hits_Allowed = df1$HA,
    Homeruns_Allowed = df1$HRA,
    Walks_Allowed = df1$BBA,
    Strikeouts_by_Pitchers = df1$SOA,
    Errors = df1$E,
    Double_Plays = df1$DP,
    Fielding_Percentage = df1$FP)
  #df2$Outs_Pitched = df1$IPouts

  df3 = data.frame(name = colnames(df2), value = round(t(df2),3))

  # Return
  return(df3)

}

# Create Subsets
df_def_redsox = create_team_def_df("Boston Red Sox", 2016)
df_def_twins = create_team_def_df("Minnesota Twins", 2016)


# Title Text
txt_title = "2016 Defensive Stats (Red Sox vs Twins)"

# Create Polar Chart
p3 = highchart() %>%
  hc_title(text = txt_title,
           style = list(fontSize = "15px")) %>%
  hc_chart(type = "", polar = TRUE) %>%
  hc_xAxis(categories = df_def_redsox$name) %>%
  hc_add_series(name = "Boston Red Sox", df_def_redsox$value,
                showInLegend = FALSE, color = "red") %>%
  hc_add_series(name = "Minnesota Twins", df_def_twins$value,
                showInLegend = FALSE, color = "steelblue")
print(p3)
