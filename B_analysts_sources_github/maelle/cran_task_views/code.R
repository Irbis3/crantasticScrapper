library("dplyr")
library("ctv")
library("ggplot2")

library("ctv")

# get views
a <- available.views()

tasks <- data.frame(
  name = vapply(a, "[[", character(1), "name"),
  version = vapply(a, "[[", character(1), "version"),
  maintainer = vapply(a, "[[", character(1), "maintainer"),
  stringsAsFactors = FALSE
)

tasks <- mutate(tasks, version = lubridate::ymd(version))

# Plot
tasks %>%
  dplyr::mutate(y = 1:nrow(tasks)) %>%
ggplot() +
  geom_label(aes(x = version, y = y , label = name)) +
  ylab("") +
  xlim(c(min(tasks$version, na.rm = TRUE), 
         max(tasks$version, na.rm = TRUE) + 10))+ theme(
           axis.text.y = element_blank(),
           axis.ticks.y = element_blank()) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  xlab("Latest update") +
  ggtitle("CRAN Task Views") +
  theme(text = element_text(size=20))

ggsave("crantaskviews.png", width = 14, height = 7)
