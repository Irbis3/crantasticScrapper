# from https://github.com/leeper/tabulizer

#1st
# @powershell -NoProfile -ExecutionPolicy Bypass -Command "iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))" && SET PATH=%PATH%;%ALLUSERSPROFILE%\chocolatey\bin

# 2nd
# choco install jdk7 -y


Sys.setenv(JAVA_HOME = dir("C:/Program Files/Java/", pattern = "jdk1", full.names = TRUE))


devtools::install_github("leeper/tabulizerjars")
# install.packages("rJava")
# install
install.packages(c("rJava", "RWeka"))

library("RWeka")
library("partykit")

data("Titanic", package = "datasets")
ttnc <- as.data.frame(Titanic)
ttnc <- ttnc[rep(1:nrow(ttnc), ttnc$Freq), 1:4]
names(ttnc)[2] <- "Gender"

j48 <- J48(Survived ~ ., data = ttnc)
plot(j48)

party_j48 <- as.party(j48)

plot(party_j48)

tree_list(party_j48)
