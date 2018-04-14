# ---------------------------------------------------------------------------- #
# Combine data sets
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

# Load packages
library(rio)
library(repmis)
library(dplyr)
library(lubridate)
library(countrycode)
library(DataCombine)
library(tidyr)
library(stringr)
library(psData)
library(WDI)
library(foreign)
library(R.cache)
library(imfr)
library(zoo)
library(fredr) # if not installed use devtools::install_github('christophergandrud/fredr')

# Set working directory
possibles <- "/git_repositories/macropru/"
set_valid_wd(possibles)

# Load macro-pru data -------
boe <- read.dta("data/raw/Data_ReinhardtSowerbutts 2015 BoE WP 546.dta")
boe$country <- countrycode(boe$countrycode, origin = "iso3c", 
                           destination = "country.name")

# Quarter variable
boe$year_quarter <- sprintf("%s.%s", boe$year, boe$quarter) %>% 
    as.numeric

boe$iso2c <- countrycode(boe$country, origin = 'country.name',
                         destination = 'iso2c')

boe <- MoveFront(boe, c("country", "iso2c", "countrycode", "year_quarter", 
                        "quarter"))

# Macropru governance
## p. 16 https://www.imf.org/external/pubs/ft/wp/2013/wp13166.pdf
source('data/raw/macropru_governance_indices.R')

macro_gov$country <- countrycode(macro_gov$country, origin = 'country.name',
                                 destination = 'country.name')

# Election timing data --------
## From http://hyde.research.yale.edu/nelda
elections <- import("data/raw/NELDA.xls", col_names = F)

elections <- elections[, c(3, 5:7)]

# Create election quarters
elections$X6 <- as.character(elections$X6)
elections$X6 <- sprintf("0%s", elections$X6)

for (i in 1:nrow(elections)) {
    if (nchar(elections[i, "X6"]) == 5) {
        elections[i, "X6"] <- substr(elections[i, "X6"], 2, 5)
    }
}

elections$election_date <- sprintf("%s%s", elections$X5, elections$X6)
elections$election_date <- ymd(elections$election_date)
elections$year_quarter <- quarter(elections$election_date, with_year = TRUE)

elections <- elections %>% DropNA(c('year_quarter'))

#  Other cleaning
elections <- elections %>% filter(X5 >= 1999)
elections$X3[elections$X3 == "Democratic Republic of Vietnam"] <- "Vietnam"
elections$X3[elections$X3 == "German Federal Republic"] <- "Germany"
elections$X3[elections$X3 == "Serbia (Yugoslavia)"] <- "Serbia"
elections$X3[elections$X3 == "Serbia, Yugoslavia"] <- "Serbia"
elections$country <- countrycode(elections$X3, origin = "country.name", 
                                 destination = "country.name")

elections <- elections %>% select(country, year_quarter, election_date, X7)
elections$value <- 1

elections <- FindDups(elections, Vars = c("country", "election_date", 
                                          "X7"), NotDups = T)

elections <- elections %>% spread(X7, value) %>% arrange(country, election_date)

# Any election
any_e <- elections %>% dplyr::select(country, year_quarter)
any_e$any_election <- 1
any_e <- any_e %>% FindDups(c('country', 'year_quarter'), NotDups = T)

exec_election <- elections %>% dplyr::select(country, year_quarter, Executive)
exec_election <- exec_election %>% DropNA('Executive')
exec_election <- exec_election %>% FindDups(c('country', 'year_quarter'), 
                                            NotDups = T)
exec_election <- exec_election %>% rename(executive_election = Executive)

elections_sub <- merge(any_e, exec_election, by = c('country', 'year_quarter'),
                       all = T)

# Inequality ---------
load('data/raw/swiidV4_0.RData') 

swiid$country <- countrycode(swiid$country, origin = 'country.name', 
                               destination = 'country.name')

swiid <- swiid[, c('country', 'year', '_1_gini_net', '_1_gini_market', 
                   '_1_redist', '_1_share1')]

names(swiid) <- gsub('_1_', '', names(swiid))

# FinStress -----
URL <- 'https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/FinStress.csv'
finstress_index <- import(URL)

# Quarter means
finstress_index$year_quarter <- quarter(finstress_index$date, with_year = T)
finstress_index <- finstress_index %>% group_by(country, year_quarter) %>%
    mutate(finstress_qt_mean = mean(FinStress))

finstress_index <- FindDups(finstress_index, c('country', 'year_quarter'), 
                            NotDups = T)

finstress_index <- finstress_index %>% select(country, year_quarter, 
                                            finstress_qt_mean) %>% as.data.frame

# Polity ---------
polity <- PolityGet(vars = c("polity2"))
polity$country <- countrycode(polity$iso2c, origin = 'iso2c',
                              destination = 'country.name')
polity <- polity %>% select(-iso2c, -standardized_country)

# DPI ---------
dpi <- DpiGet(vars = c("execrlc"))
dpi$country <- countrycode(dpi$country, origin = 'country.name',
                              destination = 'country.name')
dpi <- dpi %>% select(-iso2c)

# Clean up Missing
dpi$execrlc[dpi$execrlc == -999] <- NA
dpi$execrlc[dpi$execrlc == 0] <- NA

# Unified Democracy Score --------------------------------------
# Downloaded from http://www.unified-democracy-scores.org/uds.html
uds <- import('data/raw/uds_summary.csv') %>%
        dplyr::select(country, year, mean) %>% 
        rename(uds_mean = mean)

uds$country <- countrycode(uds$country, origin = 'country.name',
                           destination = 'country.name')

# Wang et al. GFS Fiscal transparency -----------
# Downloaded from https://www.imf.org/External/pubs/cat/longres.aspx?sk=43177.0
fiscal_trans <- import('data/raw/wp15188.xlsx', sheet = "GFS Index Score", 
                       skip = 2)
fiscal_trans$country <- countrycode(fiscal_trans$Country, origin = 'country.name',
                                    destination = 'country.name')

fiscal_trans <- fiscal_trans[, c(17, 4:14)]

fiscal_trans <- fiscal_trans %>% gather(year, fiscal_trans_gfs, 
                                        2:ncol(fiscal_trans)) %>%
                    arrange(country, year)

# CBI (Bodea and Hicks) -------------
# Downloaded from http://www.princeton.edu/~rhicks/data.html
cbi <- foreign::read.dta('data/raw/cb_rh_iodata.dta')

## Eurozone
eurozone_cbi <- cbi %>% filter(countryname == 'European Union') %>% 
                    select(-countryname)

euro_member <- import('https://raw.githubusercontent.com/christophergandrud/euro_membership/master/data/euro_membership_data.csv')

euro_cbi <- merge(euro_member, eurozone_cbi, by = 'year') %>%
                select(country, year, lvau, lvaw)

cbi$country <- countrycode(cbi$cowcode, origin = 'cown',
                           destination = 'country.name')

cbi <- cbi %>% select(country, year, lvau, lvaw)
cbi <- rbind(cbi, euro_cbi) %>% arrange(country, year)

cbi <- cbi %>% rename(cbi = lvau) %>% rename(cbi_weighted = lvaw)

cbi <- cbi %>% DropNA(c('country', 'cbi'))
cbi <- FindDups(cbi, c('country', 'year'), NotDups = TRUE)

## Assume CBI is constant from 2010 through 2014
extender <- function(df, year_original, year_new) {
    temp <- df %>% filter(year == year_original)
    temp$year <- year_new
    df <- rbind(df, temp) %>% arrange(country, year)
}

for (i in 2011:2014) cbi <- extender(cbi, year_original = 2010, year_new = i)

# WDI -------------
wdi <- WDI(indicator = c('NY.GDP.MKTP.KD.ZG', 'FP.CPI.TOTL.ZG', 'SI.POV.GINI',
                         'FS.AST.DOMS.GD.ZS', 'NY.GDP.PCAP.PP.KD',
                         'FB.BNK.CAPA.ZS', 'NY.GDP.MKTP.KD'), 
           start = 1990, end = 2015)
wdi <- wdi %>% rename(gdp_growth = NY.GDP.MKTP.KD.ZG) %>% 
    rename(inflation = FP.CPI.TOTL.ZG) %>%
    rename(gini = SI.POV.GINI) %>% 
    rename(domestic_credit = FS.AST.DOMS.GD.ZS) %>%
    rename(gdp_per_capita = NY.GDP.PCAP.PP.KD) %>%
    rename(capital_to_assets = FB.BNK.CAPA.ZS) %>%
    rename(gdp_market_constant_usd = NY.GDP.MKTP.KD)

wdi$country <- countrycode(wdi$iso2c, origin = 'iso2c',
                           destination = 'country.name')
wdi <- wdi %>% DropNA('country')

wdi <- wdi %>% dplyr::select(-iso2c)
wdi <- change(wdi, Var = 'domestic_credit', GroupVar = 'country', 
                  NewVar = 'domestic_credit_change')

# Create lags 
for (i in c('gdp_growth', 'inflation', 'gini', 'domestic_credit', 
            'gdp_per_capita')) {
    wdi <- slide(wdi, Var = i, GroupVar = 'country', TimeVar = 'year',
                 NewVar = sprintf('%s_lag_1', i))
}

# BIS Housing price change -----------
## Downloaded from http://www.bis.org/statistics/pp_selected.htm
bis <- import('data/raw/full_BIS_SELECTED_PP_csv.csv')

# Keep year-on-year real quarterly housing price change (%)
bis <- bis %>% grepl.sub(pattern = 'R:Real', Var = 'Value') %>%
        grepl.sub(pattern = '771:Year-on-year changes, in per cent', 
                  Var = 'Unit of measure') %>%
        select(-Frequency, -Value, -`Unit of measure`, -`Time Period`)

bis_col <- str_split_fixed(bis$`Reference area`, n = 2, pattern = ':') %>%
            as.data.frame %>% setNames(c('iso2c', 'country'))
bis <- cbind(bis_col, bis) %>% select(-iso2c, -`Reference area`)
bis$country <- countrycode(bis$country, origin = 'country.name',
                           destination = 'country.name')
bis <- bis %>% DropNA('country')

bis <- bis %>% gather(year_quarter, bis_housing_change, 2:ncol(bis))
bis$year_quarter <- gsub('-Q', '.', bis$year_quarter) %>% as.numeric
bis$bis_housing_change <- bis$bis_housing_change %>% as.numeric
bis <- bis %>% arrange(country, year_quarter)
bis <- bis %>% DropNA('bis_housing_change')

# Credit to the non-financial sector ----------------------------
# BIS: http://www.bis.org/statistics/totcredit.htm
bis_credit <- import('data/raw/totcredit.xlsx', sheet = 3, skip = 2)

bis_credit <- bis_credit %>% gather(id, value, 2:ncol(bis_credit))
bis_credit <- bis_credit %>% DropNA('value')

bis_id <- str_split_fixed(bis_credit$id, pattern = ':', n = 3)
bis_credit <- cbind(bis_credit, bis_id[, 2:3])

bis_credit$country <- countrycode(bis_credit$`1`, origin = 'iso2c',
                                  destination = 'country.name')

bis_credit <- bis_credit %>% DropNA('country') %>%
    filter(`2` == 'C:A:M:770:A')
bis_credit$year_quarter <- quarter(bis_credit$`Period      `, with_year = TRUE)
bis_credit <- bis_credit %>% dplyr::select(country, year_quarter, value) %>%
    rename(bis_credit_non_finance = value)

# Create year-on-year change
bis_credit <- change(bis_credit, Var = 'bis_credit_non_finance', 
                     GroupVar = 'country', TimeVar = 'year_quarter', 
                     NewVar = 'bis_credit_change', slideBy = -4)
FindDups(bis_credit, c('country', 'year_quarter'))

#  Political Constraints (Henisz) ----------------------------- 
pol_constraints <- import('data/raw/polcon2012.dta') %>%
                    dplyr::select(polity_country, year, polconiii, polconv)

pol_constraints$country <- countrycode(pol_constraints$polity_country,
                                     origin = 'country.name',
                                     destination = 'country.name')

pol_constraints <- DropNA(pol_constraints, 'country')
pol_constraints <- pol_constraints %>% dplyr::select(-polity_country) %>%
                        filter(year >= 1999)

# Central Bank Policy Rate, per annum ------------------------------
# from IMF International Financial Statistics
# Downloaded 23 March 2016
ifs <- import('data/raw/Interest_Rates.xlsx', skip = 5, na = '...')

# Cleanup
names(ifs) <- c('filler', 'country', ifs[1, 3:ncol(ifs)])
ifs <- ifs[-1, -1]
ifs <- ifs[, 1:17]

ifs <- ifs %>% gather(year, cb_policy_rate, 2:ncol(ifs))

# Convert Euroarea to euro member policy rates
euro_policy_rate <- ifs %>% filter(country == 'Euro Area')

# Download euro member data
euro_member <- import('https://raw.githubusercontent.com/christophergandrud/euro_membership/master/data/euro_membership_data.csv')

euro_policy_rate <- merge(euro_member, euro_policy_rate, by = 'year') %>%
    select(country.x, year, cb_policy_rate) %>%
    rename(country = country.x)

ifs$country <- countrycode(ifs$country, origin = 'country.name', 
                           destination = 'country.name')

ifs <- ifs %>% DropNA(c('country', 'cb_policy_rate')) 

ifs <- rbind(euro_policy_rate, ifs) %>% arrange(country, year)

ifs$cb_policy_rate <- as.numeric(ifs$cb_policy_rate)

ifs <- change(ifs, Var = 'cb_policy_rate', GroupVar = 'country', 
             TimeVar = 'year', NewVar = 'cb_policy_rate_change', 
             type = 'percent')

FindDups(ifs, c('country', 'year'))

# Exchange rate regime -------------------
# Coarse classification from Reinhart & Rogoff (2010)
# Downloaded from: http://www.carmenreinhart.com/data/browse-by-topic/topics/11/
ex_regime <- import('data/raw/ERA-Annual coarse class.xls', skip = 4)

# Clean up
names(ex_regime) <- c('year', names(ex_regime[-1]))
ex_regime <- ex_regime %>% DropNA('year')

ex_regime <- ex_regime %>% gather(country, ex_regime, 2:ncol(ex_regime))

ex_regime$country <- countrycode(ex_regime$country, origin = 'country.name',
                                 destination = 'country.name')

ex_regime <- ex_regime %>% DropNA(c('country', 'year', 'ex_regime'))

# US Fed Funds Rate (from FRED database) --------------
fed_funds <- fred_loop(single_symbol = 'FEDFUNDS',  var_name = 'us_fed_funds')

# Find quarter averages
fed_funds$year_quarter <- quarter(fed_funds$date, with_year = TRUE)

fed_funds <- fed_funds %>% group_by(year_quarter) %>%
                summarise(us_fed_funds = mean(us_fed_funds))

# IMF BOP: Capital Account, Capital transfers, Net, US Dollars -----------------
# ID: BKT_BP6_USD
boe_iso2c <- unique(boe$iso2c)

cap_account <- imf_data(database_id = 'BOP', indicator = 'BKT_BP6_USD',
                        country = boe_iso2c,
                 freq = 'Q', start = 1995, end = 2015)

cap_account <- cap_account %>% rename(capital_transfers_net = BKT_BP6_USD) 
cap_account$year_quarter <- gsub('-0', '.', cap_account$year_quarter) %>% 
                    as.numeric
cap_account$country <- countrycode(cap_account$iso2c, origin = 'iso2c',
                                   destination = 'country.name')
cap_account$year <- as.integer(cap_account$year_quarter)

# Find capital transfer % of (2005) GDP change
gdp_2005 <- wdi %>% filter(year == 2005) %>% 
                select(country, gdp_market_constant_usd)

cap_account <- merge(cap_account, gdp_2005, by = c('country'))

cap_account$cap_transfers_gdp <- (cap_account$capital_transfers_net / 
                                cap_account$gdp_market_constant_usd) * 100
# Find percentage change
cap_account <- change(cap_account, Var = 'cap_transfers_gdp', 
                      GroupVar = 'iso2c', TimeVar = 'year_quarter',
                      NewVar = 'cap_trans_perc_change',
                      slideBy = -4)

cap_account <- DropNA(cap_account, 'cap_trans_perc_change')
cap_account$cap_trans_perc_change_ma <- ave(
    cap_account_no_na$cap_trans_perc_change, as.factor(cap_account_no_na$iso2c), 
    FUN = function(x) rollmean(x, k = 4, fill = NA)) 

cap_account <- cap_account %>% select(country, year_quarter, cap_transfers_gdp,
                                      cap_trans_perc_change,
                                      cap_trans_perc_change_ma)

# Combine ------
comb <- merge(boe, elections_sub, by = c("country", "year_quarter"), 
              all.x = T)
comb <- merge(comb, macro_gov, by = 'country', all.x = T)
comb <- merge(comb, swiid, by = c('country', 'year'), all.x = T)
comb <- merge(comb, polity, by = c('country', 'year'), all.x = T)
comb <- merge(comb, uds, by = c('country', 'year'), all.x = T)
comb <- merge(comb, finstress_index, by = c('country', 'year_quarter'), 
              all.x = T)
comb <- merge(comb, dpi, by = c('country', 'year'), all.x = T)
comb <- merge(comb, fiscal_trans, by = c('country', 'year'), all.x = T)
comb <- merge(comb, cbi, by = c('country', 'year'), all.x = T)
comb <- dMerge(comb, bis, by = c('country', 'year_quarter'), all = T)
comb <- dMerge(comb, bis_credit, by = c('country', 'year_quarter'), all = T)
comb <- dMerge(comb, wdi, by = c('country', 'year'), all.x = T)
comb <- merge(comb, pol_constraints, by = c('country', 'year'), all.x = T)
comb <- merge(comb, ifs, by = c('country', 'year'), all.x = T)
comb <- merge(comb, ex_regime, by = c('country', 'year'), all.x = T)
comb <- merge(comb, fed_funds, by = c('year_quarter'), all.x = T)
comb <- merge(comb, cap_account, by = c('country', 'year_quarter'), all.x = T)

comb <- comb %>% arrange(country, year_quarter)
FindDups(comb, c('country', 'year_quarter'))

# Clean up --------------
# Capital cumulative sum
comb <- comb %>% arrange(country, year_quarter) %>% group_by(country) %>%
    mutate(cumsum_capital = cumsum(Capital))

# Finish creating election dummy
for (i in 5:7) {
    comb[, i][is.na(comb[, i])] <- 0
}

# Clean elections variable
comb$any_election[is.na(comb$any_election)] <- 0
comb$executive_election[is.na(comb$executive_election)] <- 0

# Create 4 qtrs from any election dummy
comb <- SpreadDummy(comb, Var = 'any_election', GroupVar = 'country',
                    NewVar = 'any_election_4qt', spreadBy = 3)

# Create 4 qtrs from executive election dummy
comb <- SpreadDummy(comb, Var = 'executive_election', GroupVar = 'country',
                    NewVar = 'executive_election_4qt', spreadBy = 3)

# Create 4 qtrs after any election dummy
comb <- SpreadDummy(comb, Var = 'any_election', GroupVar = 'country',
                    NewVar = 'any_election_4qt_after', spreadBy = -4)
comb$any_election_4qt_after[comb$any_election == 1] <- 0

# Create 4 qtrs from executive election dummy
comb <- SpreadDummy(comb, Var = 'executive_election', GroupVar = 'country',
                    NewVar = 'executive_election_4qt_after', spreadBy = -4)
comb$executive_election_4qt_after[comb$executive_election == 1] <- 0

# Any tightening
tighten <- names(comb)[grep("*_Tighten", names(comb))]

comb$any_tighten <- 0
for (i in tighten) {
    comb$any_tighten[comb[, i] == 1] <- 1
}

# Any loosening
loosen <- names(comb)[grep("*_Loosen", names(comb))]

comb$any_loosen <- 0
for (i in loosen) {
    comb$any_loosen[comb[, i] == 1] <- 1
}

# Cumulative tightening
comb <- comb %>% arrange(country, year_quarter) %>% group_by(country) %>%
    mutate(cumsum_any_tighten = cumsum(any_tighten))

comb <- slide(comb, Var = 'cumsum_any_tighten', GroupVar = 'country', 
              TimeVar = 'year_quarter', NewVar = 'lag_cumsum_any_tighten')

# Lag bis non-financial credit change
comb <- slide(comb, Var = 'bis_credit_change', GroupVar = 'country', 
              TimeVar = 'year_quarter', NewVar = 'lag_bis_credit_change')

comb <- comb %>% MoveFront(c("country", "countrycode", "year",
                             "year_quarter", "quarter"))

# Make sure there are no missing years
comb$year <- comb$year_quarter %>% round(digits = 0) %>% as.integer

comb <- comb %>% dplyr::select(-standardized_country, -countryname)
comb <- comb %>% DropNA('countrycode') # Keep only observations in the BoE data
      
# Make sure ISO 2-letter country code is complete
comb$iso2c <- countrycode(comb$country, origin = "country.name",
                          destination = "iso2c")

# Export -----------
export(comb, file = 'data/main_combined.csv')
