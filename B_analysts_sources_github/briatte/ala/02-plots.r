# ==============================================================================
# PLOT THEME
# ==============================================================================

theme_mods <- theme(
  panel.grid.minor = element_blank(),
  legend.text = element_text(size = rel(1)),
  strip.background = element_blank(),
  strip.text = element_blank()
)

# ==============================================================================
# GENERAL TOPICS
# ==============================================================================

i <- read_csv(f_tags, col_types = s_tags)
topics <- i$parent
names(topics) <- i$tag

td <- filter(d, !is.na(tags)) %>%
  apply(1, function(x) {
    data_frame(
      year = as.integer(x[ "year" ]),
      topic = str_split(x[ "tags" ], ";") %>%
        unlist %>%
        topics[ . ] %>%
        unique,
      weight = 1 / length(topic)
    )
  }) %>%
  bind_rows %>%
  group_by(year, topic) %>%
  summarise(w = sum(weight))

group_by(td, year) %>%
  mutate(p_w = w / sum(w)) %>%
  filter(year %in% 2000:2016) %>%
  ggplot(aes(x = year, y = p_w, fill = topic)) +
  geom_area() +
  scale_x_continuous(breaks = seq(2000, 2016, by = 2)) +
  scale_fill_brewer("Topics", palette = "Set2") +
  scale_y_continuous(labels = percent_format()) +
  labs(y = "Weighted proportion of articles", x = "Year of publication") +
  theme_bw() +
  theme_mods

ggsave("fig1_general_topics.png", width = 7, height = 6)

# ==============================================================================
# SPECIFIC TOPICS
# ==============================================================================

tags <- topics[ names(topics) != topics ]

t <- str_split(d$tags, ";") %>%
  unlist %>%
  table

t <- t[ names(t) %in% names(tags) ]
t <- t[ order(t, decreasing = TRUE) ]#[ 1:10 ]

summary(as.vector(t))
summary(as.vector(t[ names(t) %in% names(tags) ]))

# tags used above median
t[ t > 63 & names(t) %in% names(tags)  ]

# ignored: industry, business, state of the web
# also, code below is ugly

a <- filter(d, year %in% 2000:2016) %>%
  group_by(year) %>%
  summarise(articles = n(),
            html = str_count(tags, "html") %>%
              sum(na.rm = TRUE),
            css = str_count(tags, "css") %>%
              sum(na.rm = TRUE),
            interaction_design = str_count(tags, "interaction-design") %>%
              sum(na.rm = TRUE),
            javascript = str_count(tags, "javascript") %>%
              sum(na.rm = TRUE),
            workflow_tools = str_count(tags, "workflow-tools") %>%
              sum(na.rm = TRUE),
            usability = str_count(tags, "usability") %>%
              sum(na.rm = TRUE),
            project_management = str_count(tags, "project-management") %>%
              sum(na.rm = TRUE),
            browsers = str_count(tags, "browsers") %>%
              sum(na.rm = TRUE),
            graphic_design = str_count(tags, "graphic-design") %>%
              sum(na.rm = TRUE),
            layout_grids = str_count(tags, "layout-grids") %>%
              sum(na.rm = TRUE)) %>%
  gather(tag, n, html:layout_grids)

ggplot(a, aes(as.integer(year), n, group = tag, fill = tag)) + # n / articles
  geom_hline(yintercept = 0, color = "grey50") +
  geom_col() +
  facet_grid(tag ~ ., scales = "free_x") +
  scale_x_continuous(breaks = seq(2000, 2016, by = 2)) +
  scale_fill_brewer("Topics", palette = "Paired") +
  #scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Raw number of articles", x = "Year of publication") +
  theme_bw() +
  theme_mods

ggsave("fig2_specific_topics.png", width = 7, height = 8)
