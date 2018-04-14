# Load packages
library(readr)
library(dplyr)
library(ggplot2)

df_reviews <- read_csv("yelp_reviews.csv")

# Overview of Data
df_reviews

# Summary of Data
summary(df_reviews)

# Add pos/neg
df_reviews <- df_reviews %>% mutate(pos_norm = pos_words / review_length)
df_reviews <- df_reviews %>% mutate(neg_norm = neg_words / review_length)

# qplots
qplot(data=df_reviews, stars)
ggsave("lc1-qplot-1.png", dpi=300, width=4, height=3) # save for post
qplot(data=df_reviews, review_length)
ggsave("lc1-qplot-2.png", dpi=300, width=4, height=3)
qplot(data=df_reviews, stars, pos_words)
ggsave("lc1-qplot-3.png", dpi=300, width=4, height=3)

# Business
df_businesses <- read_csv("yelp_businesses.csv")

# Overview
df_businesses

# Merge df's
df_reviews <- df_reviews %>% left_join(df_businesses) # incorrect, do not run!
df_reviews <- df_reviews %>% left_join(df_businesses, by="business_id") # correct

# Aggregate cities + avg_stars
df_cities <- df_reviews %>% group_by(city) %>% summarize(avg_stars = mean(stars.x))
df_cities

# Aggregate states
df_states <- df_reviews %>% group_by(state) %>% summarize(avg_stars = mean(stars.x), count=n()) %>% arrange(desc(count))
df_states

# ggplot2 plots
ggplot(data=df_states, aes(state, avg_stars))
ggsave("lc1-ggplot-1.png", dpi=300, width=4, height=3)
ggplot(data=df_states, aes(state, avg_stars)) + geom_bar(stat="identity")
ggsave("lc1-ggplot-2.png", dpi=300, width=4, height=3)
ggplot(data=df_states, aes(state, avg_stars)) + geom_bar(stat="identity") + coord_flip()
ggsave("lc1-ggplot-3.png", dpi=300, width=4, height=3)

df_states <- df_states %>% arrange(desc(avg_stars)) %>% filter(count > 2000) %>% mutate(state = factor(state, levels=rev(state)))

ggplot(data=df_states, aes(state, avg_stars)) + geom_bar(stat="identity") + coord_flip()
ggsave("lc1-ggplot-4.png", dpi=300, width=4, height=3)

ggplot(data=df_states, aes(state, avg_stars)) + geom_bar(stat="identity") + coord_flip() + geom_text(aes(label=round(avg_stars, 2)), hjust=1, color="white")
ggsave("lc1-ggplot-5.png", dpi=300, width=4, height=3)

# Theme
library(RColorBrewer)

fte_theme <- function() {
      
      # Generate the colors for the chart procedurally with RColorBrewer
      palette <- brewer.pal("Greys", n=9)
      color.background = palette[2]
      color.grid.major = palette[3]
      color.axis.text = palette[6]
      color.axis.title = palette[7]
      color.title = palette[9]
      
      # Begin construction of chart
      theme_bw(base_size=9) +
        
      # Set the entire chart region to a light gray color
      theme(panel.background=element_rect(fill=color.background, color=color.background)) +
      theme(plot.background=element_rect(fill=color.background, color=color.background)) +
      theme(panel.border=element_rect(color=color.background)) +
      
      # Format the grid
      theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
      theme(panel.grid.minor=element_blank()) +
      theme(axis.ticks=element_blank()) +
      
      # Format the legend, but hide by default
      theme(legend.position="none") +
      theme(legend.background = element_rect(fill=color.background)) +
      theme(legend.text = element_text(size=7,color=color.axis.title)) +
      
      # Set title and axis labels, and format these and tick marks
      theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
      theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
      theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
      theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
      theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
      
      # Plot margins
      theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
    }


ggplot(data=df_states, aes(state, avg_stars)) + geom_bar(stat="identity") + coord_flip() + geom_text(aes(label=round(avg_stars, 2)), hjust=2, size=2, color="white") + fte_theme() + labs(y="Average Star Rating by State", x="State", title="Average Yelp Review Star Ratings by State")
ggsave("lc1-ggplot-6.png", dpi=300, width=4, height=3)

df_states <- df_reviews %>% group_by(state) %>% summarize(avg_stars = mean(stars.x), count=n(), se_mean=sd(stars.x)/sqrt(count)) %>% arrange(desc(avg_stars)) %>% filter(count > 2000) %>% mutate(state = factor(state, levels=rev(state)))

ggplot(data=df_states, aes(state, avg_stars)) + geom_bar(stat="identity") + coord_flip() + geom_text(aes(label=round(avg_stars, 2)), hjust=2, size=2, color="white") + fte_theme() + labs(y="Average Star Rating by State", x="State", title="Average Yelp Review Star Ratings by State") + geom_errorbar(aes(ymin=avg_stars - 1.96 * se_mean, ymax=avg_stars + 1.96 * se_mean))
ggsave("lc1-ggplot-7.png", dpi=300, width=4, height=3)