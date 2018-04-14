library("dplyr")

load("data/meas100ail.RData")

# sin tubos
meas100ail2 <- mutate(meas100ail, biggerValue = value + 10)
meas100ail2 <- select(meas100ail2, - cityURL, - locationURL)
meas100ail2 <- mutate(meas100ail2, countrySmall = tolower(country))
meas100ail2 <- select(meas100ail2, - country)

# con tubos
meas100ail2 <- meas100ail %>%
  mutate(biggerValue = value + 10) %>%
  select(- cityURL, - locationURL) %>%
  mutate(countrySmall = tolower(country)) %>%
  select(- country)
