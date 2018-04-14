ages <- array()

assignAges <- function(ages, minAge, maxAge, pct){
  ages[minAge:maxAge] <- pct/(maxAge-minAge+1)
  ages
}

ages <- assignAges(ages, 1, 5, 5.6)
ages <- assignAges(ages, 5, 9, 5.5)
ages <- assignAges(ages, 10, 14, 6)
ages <- assignAges(ages, 15, 19, 6.5)
ages <- assignAges(ages, 20, 24, 5.2)
ages <- assignAges(ages, 25, 29, 5.2)
ages <- assignAges(ages, 30, 34, 5.0)
ages <- assignAges(ages, 35, 39, 5.0)
ages <- assignAges(ages, 40, 44, 5.9)
ages <- assignAges(ages, 45, 49, 7.3)
ages <- assignAges(ages, 50, 54, 8.6)
ages <- assignAges(ages, 55, 59, 8.5)
ages <- assignAges(ages, 60, 64, 8.0)
ages <- assignAges(ages, 65, 69, 5.9)
ages <- assignAges(ages, 70, 74, 4.3)
ages <- assignAges(ages, 75, 79, 3.1)
ages <- assignAges(ages, 80, 84, 2.3)
ages <- assignAges(ages, 85, 90, 2.1)

# I'm guessing here, since i don't have a key.
raceAbbrevs <- list(I="Indian", W="White", H="Hispanic", P="Pacific", B="Black", O="Other")
# There are others, which should just be classified as Other

# From wikipedia, as of 2011
# The percentages on Wikipedia add up to 116.6%, so I guess people can be counted twice.
# We'll just normalize over 1.166 -- it should get us close to some simplified version of reality.
# Create a named array
races <- unlist(list(I=.032, W=.846, H=.167, P=.011, B=.022, O=.088))
races <- races/1.166


# http://quickfacts.census.gov/qfd/states/06/06033.html Says 50% female
gender <- list(M=.5, F=.5, O=0.0)
genAbbrev <- list(M="Male", F="Female", O="Other")
