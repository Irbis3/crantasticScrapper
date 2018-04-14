### Two ways to redraw a horrible chart
library(ggplot2)
library(scales)

theme_set(theme_minimal())

data <- read.csv("data/data.csv", header = TRUE)


## Scatterplot
p <- ggplot(data, aes(x = Employees, y = Revenue))

p + geom_smooth(method = "lm") +
    geom_point(aes(color = Mayer), size = 3) +
    theme(legend.position = "bottom") +
    labs(color = "Mayer is CEO",
         x = "Employees",
         y = "Revenue (Millions)") +
    ggtitle("Yahoo Employees vs Revenues, 2004-2014") +
    scale_y_continuous(labels = dollar) +
    scale_x_continuous(labels = comma)
credit("\nNote: Two observations for 2012 (Mayer appointed in July). Kieran Healy, http://kieranhealy.org")



## Line segements with year labels
p + geom_path(color = "gray80") +
    geom_text(aes(color = Mayer, label = Year), size = 3, fontface = "bold") +
    theme(legend.position = "bottom") +
    labs(color = "Mayer is CEO",
         x = "Employees",
         y = "Revenue (Millions)") +
    ggtitle("Yahoo Employees vs Revenues, 2004-2014") +
    scale_y_continuous(labels = dollar) +
    scale_x_continuous(labels = comma)
credit("\nNote: Two observations for 2012 (Mayer appointed in July). Kieran Healy, http://kieranhealy.org")


### Revenue/Employee ratio over time
p <- ggplot(data,
            aes(x = Year, y = Revenue/Employees))

p + geom_vline(xintercept = 2012) +
    geom_line(color = "gray60", size = 2) +
    annotate("text", x = 2013, y = 0.44, label = " Mayer becomes CEO", size = 2.5) +
    labs(x = "Year\n",
         y = "Revenue/Employees") +
    ggtitle("Yahoo Revenue to Employee Ratio, 2004-2014")

credit("\n\nNote: Two observations for 2012 (Mayer appointed in July). Kieran Healy, http://kieranhealy.org")
