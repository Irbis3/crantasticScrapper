pkgs = c("dplyr", "readr")
lapply(pkgs, library, character.only = T)

download.file("https://data.gov.uk/dataset/basic-company-data/datapackage.zip", "datapackage.zip")
dir.create("companies-data")
unzip("datapackage.zip", exdir = "companies-data")
f = list.files("companies-data/data", full.names = T)
i = 1
for(i in 1:length(f)){
  unzip(f[i], exdir = "companies-data")
}

f = list.files("companies-data", pattern = ".csv", full.names = T)
i = 1

l = as.list(1:length(f))
for(i in 1:length(f)){
  l[[i]] = read_csv(f[i])
}
companies = bind_rows(l) # 3 million companies
View(companies)
companies

sel_toyota = grepl(pattern = "TOYOTA", companies$CompanyName)
summary(sel_toyota)
companies$CompanyName[sel_toyota]
companies_toyota = companies[sel_toyota,]

sel_carsales1 = grepl(pattern = "45112", companies$SICCode.SicText_1)
sel_carsales2 = grepl(pattern = "45112", companies$SICCode.SicText_2)
carcos = companies[sel_carsales1 | sel_carsales2,]

