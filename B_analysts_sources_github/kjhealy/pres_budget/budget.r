library(ggalt)
library(hrbrthemes)
library(Cairo)
library(rvest)

## Make a "figures" subdirectory if one doesn't exist
ifelse(!dir.exists(file.path("figures")), dir.create(file.path("figures")), FALSE)


url <- "https://www.nytimes.com/interactive/2017/03/15/us/politics/trump-budget-proposal.html"

## Use SelectorGadget to extract the XPath that identifies the table:
## http://selectorgadget.com

spending <- read_html(url)

spending.df <- spending %>%
    html_node("table") %>%
    html_table()

head(spending.df)

colnames(spending.df) <- c("Agency", "Y2017", "Y2018", "change", "pct")

## Clean up. Everything is character, and we need numbers.
spending.df$Y2017 <- gsub("\\$", "", spending.df$Y2017)
spending.df$Y2018 <- gsub("\\$", "", spending.df$Y2018)
spending.df$Y2018 <- gsub("\\$", "", spending.df$Y2018)


spending.df$Y2017 <- as.numeric(spending.df$Y2017)
spending.df$Y2018 <- as.numeric(spending.df$Y2018)
spending.df$change <- with(spending.df, Y2018 - Y2017)
spending.df$pct <- with(spending.df,
                       round((change / Y2017)*100, 0))

## This variable is meaningless, it's just a hack for the legend we'll
## make below by overriding the fill aesthetic in the guides() call.
spending.df$dummy <- "Current"
spending.df$dummy[2] <- "Proposed"

cairo_pdf(file="figures/spending-raw.pdf", height = 6, width = 6)
p <- ggplot(spending.df, aes(y=reorder(Agency, Y2017), x=Y2017, xend=Y2018, fill = dummy))

p1 <- p + geom_dumbbell(size=1.25, color="#e3e2e1",
                        colour_x = "#5b8124", colour_xend = "#bad744",,
                dot_guide=TRUE, dot_guide_size=0.25) +
    scale_x_continuous(labels = scales::dollar) +
    guides(fill = guide_legend(override.aes = list(color = c("#5b8124", "#bad744"),
                                                   size = 4))) +
    labs(x="Billions of Dollars",
         y = NULL,
         fill = NULL,
         title = "Proposed Discretionary Spending Changes\nin Billions of Dollars",
         caption = "Source: NYT. Figure: Kieran Healy.") +
    theme_ipsum(grid= "X" ) +
    theme(panel.grid.major.x=element_line(size=0.1),
          legend.position = "top")

p1

dev.off()

ggsave("figures/budget-raw.png")


cairo_pdf(file="figures/spending-raw-nodef.pdf", height = 6, width = 6)
p <- ggplot(subset(spending.df, Agency %nin% "Defense"),
            aes(y=reorder(Agency, Y2017), x=Y2017, xend=Y2018, fill = dummy))

p1 <- p + geom_dumbbell(size=1.25, color="#e3e2e1",
                        colour_x = "#5b8124", colour_xend = "#bad744",,
                dot_guide=TRUE, dot_guide_size=0.25) +
    scale_x_continuous(labels = scales::dollar) +
    guides(fill = guide_legend(override.aes = list(color = c("#5b8124", "#bad744"),
                                                   size = 4))) +
    labs(x="Billions of Dollars",
         y = NULL,
         fill = NULL,
         title = "Proposed Discretionary Spending Changes\nin Billions of Dollars, excluding Defense",
         caption = "Source: NYT. Figure: Kieran Healy.") +
    theme_ipsum(grid= "X" ) +
    theme(panel.grid.major.x=element_line(size=0.1),
          legend.position = "top")

p1

dev.off()

ggsave("figures/budget-raw-nodef.png")


cairo_pdf(file="figures/spending-pct.pdf", height = 6, width = 6)
p <- ggplot(spending.df, aes(y=reorder(Agency, pct), x=0, xend= pct, fill = dummy))

p1 <- p + geom_vline(xintercept = 0, color = "#e3e2e1", size = 0.7) +
    geom_dumbbell(size=1.25, color="#e3e2e1",
                        colour_x = "#5b8124", colour_xend = "#bad744",,
                        dot_guide=TRUE, dot_guide_size=0.25) +
    guides(fill = guide_legend(override.aes = list(color = c("#5b8124", "#bad744"),
                                                   size = 4))) +
    labs(x="Percent change",
         y = NULL,
         fill = NULL,
         title = "Proposed Discretionary Spending Changes,\nas Percent of Current Agency Budget",
         caption = "Source: NYT. Figure: Kieran Healy.") +
    theme_ipsum(grid= "X" ) +
    theme(panel.grid.major.x=element_line(size=0.1),
          legend.position = "top")

p1

dev.off()

ggsave("figures/budget-pct.png")
