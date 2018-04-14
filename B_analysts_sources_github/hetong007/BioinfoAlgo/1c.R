source('../SuffixArrayAndBWTMatch.R')
txt = readLines('rosalind_1c.txt')
ans = ExactMatch(origin=txt[2],patterns=txt[1])[[1]]
ans = ans -1
ans = paste(ans,collapse=' ')
writeLines(ans,'1c.txt')
