library(manipulate)
library(ggplot2)

## Example: Slider
data <- read.csv("eno.csv")
manipulate(
        qplot(log(eno), data = data, bins = n.breaks),
        n.breaks = slider(3, 20, label = "Bins")
)

## Example: Picker
skin <- read.csv("data/skin.csv")
m <- merge(data, skin, by = "id")
manipulate(
        qplot(log(eno), 
              data = filter(m, mopos == allergic), 
              bins = n.breaks),
        n.breaks = slider(3, 20, label = "Bins"),
        allergic = picker("Yes" = "yes", "No" = "no",
                          label = "Mouse Allergic?")
)


## Example: Combining controls
eno <- read.csv("data/eno.csv")
env <- read.csv("data/environmental.csv")
skin <- read.csv("data/skin.csv")
m <- merge(eno, env, by = "id")
m <- merge(m, skin, by = "id")
xlim <- range(log(m$pm25), na.rm = TRUE)
ylim <- range(log(m$eno), na.rm = TRUE)

library(dplyr)
manipulate({
        g <- ggplot(data = filter(m, mopos == allergic),
                    aes(log(pm25), log(eno))) + 
                xlim(xlim) + ylim(ylim) + 
                geom_smooth(method = "loess", span = span.p)
        if(addpoints) 
                g <- g + geom_point()
        print(g)
}, allergic = picker("Yes" = "yes", "No" = "no", 
                     label = "Mouse Allergic?"),
addpoints = checkbox(FALSE, "Add Points?"),
span.p = slider(0.2, 1, initial = 2/3, label = "Span"))


## Example: Button
manipulate({
        if(reset) {
                seed <- as.integer(Sys.time())
                set.seed(seed)
        }
        x <- rnorm(100)
        y <- 4 + 1.5 * x + rnorm(100)
        qplot(x, y, xlim = c(-4, 4), ylim = c(-1, 10))
}, reset = button("Reset seed?"), 
show.cor = button("Show Correlation?"))
