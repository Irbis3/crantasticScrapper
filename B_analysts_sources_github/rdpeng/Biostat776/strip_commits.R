## Strip commits of author/email

library(readr)
commits <- read_lines("commit_logs.txt.bz2")

alines <- grep("^Author", commits, perl = TRUE)
nameemail <- regexec(".*?: (.*?) <(.*?)>", commits[alines],
                     perl = TRUE)
m <- regmatches(commits[alines], nameemail)
nm <- sapply(m, function(x) x[2])
em <- sapply(m, function(x) x[3])

nm.a <- abbreviate(nm)
em.a <- abbreviate(em)
author.b <- paste("Author: ", nm.a, " <", em.a, ">", sep = "")

commits[alines] <- author.b

write_lines(commits, "commit_logs_strip.txt")
