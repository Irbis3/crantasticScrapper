library(ggplot2)

## Data courtesy C. DeSante, Oberlin
td <- read.csv("https://raw.githubusercontent.com/kjhealy/tdata/master/data/tdata.csv")

ggplot(td) + geom_tile(aes(x=H, y=T, fill=tc, width=1)) + scale_fill_identity() +
    theme_bw()
