## CBO Estimates viz
## Dumbbell geom and theme courtesty @hrbrmstr

library(tidyverse)
library(ggalt)
library(hrbrthemes)
library(Cairo)

## Make a "figures" subdirectory if one doesn't exist
ifelse(!dir.exists(file.path("figures")), dir.create(file.path("figures")), FALSE)

data <- read_csv("data/cbo-table4.csv")

## Geom Dumbbell wants wide format
data_s <- select(data, Law:Age, Net)
data_w <- spread(data_s, Law, Net)
data_w$Dummy <- c("Current Law", "AHCA")

party.colors <- c("#2E74C0", "#CB454A")

cairo_pdf(file="figures/cbo-tab4.pdf", height = 6, width = 6)
p <- ggplot(data_w, aes(y=Age, x=Current, xend=AHCA, fill = Dummy))

p1 <- p + geom_dumbbell(size=3, color="#e3e2e1",
                colour_x = party.colors[1], colour_xend = party.colors[2],
                dot_guide=TRUE, dot_guide_size=0.25) +
    scale_x_continuous(labels = scales::dollar,
                       breaks = c(1000, 5000, 10000, 15000)) +
    guides(fill = guide_legend(override.aes = list(color = rev(party.colors),
                                                   size = 4))) +
    labs(x="Net Premium Paid",
         y = NULL,
         fill = NULL,
         title = "CBO Estimates of Net Premium Payments\nUnder Current Law and the AHCA",
         caption = "Source: CBO Estimates, March 13th 2017, Table 4. Figure: Kieran Healy.") +
    theme_ipsum(grid= "X" ) +
    theme(panel.grid.major.x=element_line(size=0.1),
          legend.position = "top") +
    facet_wrap(~ Income, ncol = 1)

p1

dev.off()


ggsave("figures/cbo-tab4.png")


## May CBO scoring

theme_set(theme_minimal())

data_may <- read_csv("data/cbo-hr1628.csv")
data_may$Income <- gsub("0 \\(", "0\n(", data_may$Income)

p <- ggplot(data_may, aes(x = Income, y = Premium, fill = Law))

p + geom_col(position = "dodge") + facet_wrap(~ Age, nrow = 3) + coord_flip() +
    theme(legend.position = "top") +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = my.colors()) +
    scale_y_continuous(labels = scales::dollar) +
    labs(x = "",
         y = "Net Premium Paid",
         title = "Net Premiums Paid: May 24th CBO Estimates",
         subtitle = "For a single person, showing implementation scenarios by Age and Income.")
