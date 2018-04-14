# New Master Table - Data Munging Step

# ------------------------------------------------------------------------------
# Pre-load Libraries
# ------------------------------------------------------------------------------
suppressPackageStartupMessages(library(RJDBC))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))


# ------------------------------------------------------------------------------
# Connect to dashDB
# ------------------------------------------------------------------------------

# Load Credentials
dsn_driver = "com.ibm.db2.jcc.DB2Driver"
dsn_database = "BLUDB"
dsn_hostname = "enter_hostname_here"
dsn_port = "50000"
dsn_protocol = "TCPIP"
dsn_uid = "enter_user_id_here"
dsn_pwd = "enter_password_here"

# Connect to dashDB
jcc = JDBC("com.ibm.db2.jcc.DB2Driver", "./db2jcc4.jar");
jdbc_path = paste("jdbc:db2://",  dsn_hostname, ":",
                  dsn_port, "/", dsn_database, sep="");
conn = dbConnect(jcc, jdbc_path, user=dsn_uid, password=dsn_pwd)


# ------------------------------------------------------------------------------
# Download Data
# ------------------------------------------------------------------------------

# Key Tables
d_master = fetch(dbSendQuery(conn, "select * from MASTER"), -1)
d_bat = fetch(dbSendQuery(conn, "select * from BATTING"), -1)
d_field = fetch(dbSendQuery(conn, "select * from FIELDING"), -1)
d_pitch = fetch(dbSendQuery(conn, "select * from PITCHING"), -1)
d_teams = fetch(dbSendQuery(conn, "select * from TEAMS"), -1)

# Save
# save(d_master, d_bat, d_field, d_pitch, d_teams, file = "local_data.rda")

# Maybe later ...
# d_bat_pos = fetch(dbSendQuery(conn, "select * from BATTING_POST"), -1)
# d_field_pos = fetch(dbSendQuery(conn, "select * from FIELDING_POST"), -1)
# d_pitch_pos = fetch(dbSendQuery(conn, "select * from PITCHING_POST"), -1)

# Close connection
dbDisconnect(conn)


# ------------------------------------------------------------------------------
# Merging with Master
# ------------------------------------------------------------------------------

# If using local data
# load(file = "local_data.rda")

d_bat_munged = merge(d_master, d_bat,
                     by.x = "playerID", by.y = "playerID", all.y = TRUE)

d_field_munged = merge(d_master, d_field,
                     by.x = "playerID", by.y = "playerID", all.y = TRUE)

d_pitch_munged = merge(d_master, d_pitch,
                       by.x = "playerID", by.y = "playerID", all.y = TRUE)


# ------------------------------------------------------------------------------
# Adding Features
# debutYear, age, careerYear
# ------------------------------------------------------------------------------

d_bat_munged$debutYear = lubridate::year(d_bat_munged$debut)
d_bat_munged$age = d_bat_munged$yearID - d_bat_munged$birthYear
d_bat_munged$careerYear = d_bat_munged$yearID - d_bat_munged$debutYear + 1
d_bat_munged = d_bat_munged %>% arrange(playerID, careerYear, teamID)

d_field_munged$debutYear = lubridate::year(d_field_munged$debut)
d_field_munged$age = d_field_munged$yearID - d_field_munged$birthYear
d_field_munged$careerYear = d_field_munged$yearID - d_field_munged$debutYear + 1
d_field_munged = d_field_munged %>% arrange(playerID, careerYear, teamID)

d_pitch_munged$debutYear = lubridate::year(d_pitch_munged$debut)
d_pitch_munged$age = d_pitch_munged$yearID - d_pitch_munged$birthYear
d_pitch_munged$careerYear = d_pitch_munged$yearID - d_pitch_munged$debutYear + 1
d_pitch_munged = d_pitch_munged %>% arrange(playerID, careerYear, teamID)


# ------------------------------------------------------------------------------
# Getting Average Stats
# ------------------------------------------------------------------------------

colnames(d_bat_munged) = c(colnames(d_bat_munged)[1:32],
                           "TwoB", "ThreeB", colnames(d_bat_munged)[35:48])

# Batting
d_bat_avg =
  d_bat_munged %>%
  filter(yearID > 1900) %>%
  group_by(playerID, nameFirst, nameLast,
           birthMonth, birthCountry, birthState, birthCity,
           weight, height, bats, throws, finalGame) %>%
  summarise(avg_stint = mean(stint, na.rm = TRUE),
            avg_G = mean(G, na.rm = TRUE),
            avg_AB = mean(AB, na.rm = TRUE),
            avg_R = mean(R, na.rm = TRUE),
            avg_H = mean(H, na.rm = TRUE),
            avg_2B = mean(TwoB, na.rm = TRUE),
            avg_3B = mean(ThreeB, na.rm = TRUE),
            avg_HR = mean(HR, na.rm = TRUE),
            avg_RBI = mean(RBI, na.rm = TRUE),
            avg_SB = mean(SB, na.rm = TRUE),
            avg_CS = mean(CS, na.rm = TRUE),
            avg_BB = mean(BB, na.rm = TRUE),
            avg_SO = mean(SO, na.rm = TRUE),
            avg_IBB = mean(IBB, na.rm = TRUE),
            avg_HBP = mean(HBP, na.rm = TRUE),
            avg_SH = mean(SH, na.rm = TRUE),
            avg_SF = mean(SF, na.rm = TRUE),
            avg_GIDP = mean(GIDP, na.rm = TRUE)
  ) %>%
  filter(avg_HR >= 5)

# Pitching
d_pitch_avg =
  d_pitch_munged %>%
  filter(yearID > 1900) %>%
  filter(G >= 10) %>%
  group_by(playerID, nameFirst, nameLast,
           birthMonth, birthCountry, birthState, birthCity,
           weight, height, bats, throws, finalGame) %>%
  summarise(avg_stint = mean(stint, na.rm = TRUE),
            avg_W = mean(W, na.rm = TRUE),
            avg_L = mean(L, na.rm = TRUE),
            avg_G = mean(G, na.rm = TRUE),
            avg_GS = mean(GS, na.rm = TRUE),
            avg_CG = mean(CG, na.rm = TRUE),
            avg_SHO = mean(SHO, na.rm = TRUE),
            avg_SV = mean(SV, na.rm = TRUE),
            avg_IPouts = mean(IPouts, na.rm = TRUE),
            avg_ER = mean(ER, na.rm = TRUE),
            avg_HR = mean(HR, na.rm = TRUE),
            avg_BB = mean(BB, na.rm = TRUE),
            avg_SO = mean(SO, na.rm = TRUE),
            avg_BAOpp = mean(BAOpp, na.rm = TRUE),
            avg_ERA = mean(ERA, na.rm = TRUE),
            avg_IBB = mean(IBB, na.rm = TRUE),
            avg_WP = mean(WP, na.rm = TRUE),
            avg_HBP = mean(HBP, na.rm = TRUE),
            avg_BK = mean(BK, na.rm = TRUE),
            avg_BFP = mean(BFP, na.rm = TRUE),
            avg_GF = mean(GF, na.rm = TRUE),
            avg_R = mean(R, na.rm = TRUE),
            avg_SH = mean(SH, na.rm = TRUE),
            avg_SF = mean(SF, na.rm = TRUE),
            avg_GIDP = mean(GIDP, na.rm = TRUE)
  )

