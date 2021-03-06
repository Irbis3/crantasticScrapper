
#Test the function ICDSeq
expect_that(ICDSeq(start = "A1", end = "A3"), matches(c("A01", "A02", "A03")))
expect_that(ICDSeq(start = "Z1", end = "Z6"), matches(c("Z01", "Z02", "Z03",
                                   "Z04", "Z05", "Z06")))

#Test that the mortality database is coded correctly
expect_that(hom.juarez[hom.juarez$CAUSADEF %in% ICDSeq("W25", "W29"),]$CAUSE,
            matches("Cut/pierce"))

expect_that(hom.juarez[hom.juarez$CAUSADEF %in% ICDSeq("W32", "W34"),]$CAUSE,
            matches("Firearm"))

expect_that(hom.juarez[hom.juarez$CAUSADEF %in% "X59",]$CAUSE,
            matches("Unspecified"))

expect_that(hom.juarez[hom.juarez$CAUSADEF %in% ICDSeq("W65", "W74"),]$CAUSE,
            matches("Drowning"))


