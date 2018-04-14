
# Need to know what the *.csv expands to at run time to know if
# re-running the command would change matters
# Could just take the time stamp of the directory and rerun
# if anything has changed.
x = system("wc -l *.csv", intern = TRUE)

