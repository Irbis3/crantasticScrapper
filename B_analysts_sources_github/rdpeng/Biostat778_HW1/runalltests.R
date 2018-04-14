infiles <- dir(pattern = glob2rx("*.R"))

for(i in seq_along(infiles)) {
        outfile <- paste0(infiles[i], "out.save")
        cmd <- sprintf("R CMD BATCH --no-save %s %s", infiles[i], outfile)
        system(cmd)
}
