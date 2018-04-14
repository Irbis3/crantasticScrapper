

subMuni <- function(df, munis) {
    df <- subset(df, Municipality %in% munis)
}

plotHomicide <- function(df, sub, title) {
    cols <- c("Badiraguato" = "#FB9A99","Cd. Juarez" = "#E31A1C",
              "Tijuana" = "#A6CEE3", "Acapulco" = "#1F78B4",
              "Nuevo Laredo" = "#B2DF8A",
              "Lazaro Cardenas" = "#33A02C",
              "Matamoros" = "#FDBF6F", "Miguel Hidalgo" = "#FF7F00",
              "Toluca" = "#CAB2D6", "Naucalpan" = "#6A3D9A")
    df <- subset(df, variable == sub & Year)
    df <- ddply(df, .(Municipality), transform,
                       order = value[Year == 2007])
    df$Municipality <- with(df, reorder(Municipality, -order))
    ggplot(df, aes(Year, value, group = Municipality,
                   color = Municipality)) +
        geom_line(size = 1.2) +
        scale_x_continuous(limits = c(1990, 2010)) +
        scale_y_continuous(limits = c(0, max(df$value))) +
        ylab("homicide rate") +
        opts(title = title) +
        scale_colour_manual(values = cols)
}

addLabels <- function(p) direct.label(p, "last.points")

savePlot <- function(p, filename, width = 640, height = 480,
                     AA = FALSE){
    if(AA) {
        Cairo(file = filename, width=width, height=height)
        print(p)
        dev.off()
    } else {
        print(p)
        dev.print(png, file = filename, width=width, height=height)
    }
}

plotM <- function(df, sub, title, filename)
    savePlot(addLabels(plotHomicide(df, sub, title)), filename)

