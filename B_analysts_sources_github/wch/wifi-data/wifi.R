library(plyr)
library(ggplot2)

# NOTE: Before running this script, you need to collect all the lines from
# syslog that contain "wpa_supplicant". This can be done by running the lines
# below from the command shell:
#  sudo cat /var/log/syslog        | grep wpa_supplicant >  wpa_log.txt
#  sudo cat /var/log/syslog.1      | grep wpa_supplicant >> wpa_log.txt
#  sudo zcat /var/log/syslog.*.gz  | grep wpa_supplicant >> wpa_log.txt


# Read in data --------------------------------------------

# All CoCo BSSIDs start with this - use to filter out other BSSIDs
coco_bssid <- "^e0:1c:41:1e"

# Filter for successful connections
logtext_to_df <- function(logfile) {
  logtext <- readLines(logfile)

  conn_lines <- logtext[grepl("CTRL-EVENT-CONNECTED", logtext)]
  conn_df <- read.table(header=FALSE, text=conn_lines, stringsAsFactors=FALSE)
  conn_df <- conn_df[, c(1, 2, 3, 11)]
  names(conn_df) <- c("month", "day", "time", "bssid")
  conn_df$status <- "success"

  disconn_lines <- logtext[grepl("CTRL-EVENT-DISCONNECTED", logtext)]
  disconn_df <- read.table(header=FALSE, text=disconn_lines, stringsAsFactors=FALSE)
  disconn_df <- disconn_df[, c(1, 2, 3, 8)]
  names(disconn_df) <- c("month", "day", "time", "bssid")
  disconn_df$bssid <- sub("^bssid=", "", disconn_df$bssid)
  disconn_df$status <- "disconnect"
  
  timeout_lines <- logtext[grepl("Authentication with .* timed out", logtext)]
  timeout_df <- read.table(header=FALSE, text=timeout_lines, stringsAsFactors=FALSE)
  timeout_df <- timeout_df[, c(1, 2, 3, 9)]
  names(timeout_df) <- c("month", "day", "time", "bssid")
  timeout_df$status <- "timeout"
  
  # Put them all together
  all <- rbind(conn_df, disconn_df, timeout_df)
  all$status <- factor(all$status, levels = c("success", "disconnect", "timeout"))
  
  # Convert time columns to POSIXct (add in with current year because it's
  # not present in logs)
  now_year <- format(Sys.Date(), "%Y")
  all$time <- with(all, paste(now_year, month, day, time))
  all$time <- as.POSIXct(all$time, format = "%Y %b %d %H:%M:%S")
  all$month <- all$day <- NULL

  # Since data doesn't contain years, make sure dates after now are actually for
  # previous year
  in_future <- all$time > Sys.time()
  all$time[in_future] <- all$time[in_future] - as.difftime(365, units = "days")

  # Drop 00:00:00:00:00:00 events, since they seem to come after timeouts
  # for a real BSSID
  all <- all[all$bssid != "00:00:00:00:00:00", ]

  arrange(all, time)
}

wpa <- logtext_to_df('wpa_log.txt')

# Drop non-CoCo log data
wpa <- wpa[grepl(coco_bssid, wpa$bssid), ]

# Summarize counts ----------------------------------------

wpa_count <- ddply(wpa, c("bssid", "status"), summarise, count = length(status))


# BSSID frequencies and bands -----------------------------
bssid_table <- function(logfile) {
  # Find all bssid and associated frequencies from logfile
  log_text <- readLines(logfile)
  bssid_text <- log_text[grepl("SSID='CoCo'", log_text)]
  bssids <- data.frame(
    bssid = sub(".*with ([^ ]+) .*", "\\1", bssid_text),
    freq = as.numeric(sub(".*freq=([^ ]+) .*", "\\1", bssid_text)),
    stringsAsFactors = FALSE
  )
  bssids <- unique(bssids)
  
  # Read in table of all possible channels, and merge in
  all_channels <- read.csv('channels.csv')
  bssids <- join(bssids, all_channels, by = "freq")
  arrange(bssids, bssid)
}

bssids <- bssid_table('wpa_log.txt')

# Drop non-CoCo log data
bssids <- bssids[grepl(coco_bssid, bssids$bssid), ]

wpa <- join(wpa, bssids, by = "bssid")
wpa_count <- join(wpa_count, bssids, by = "bssid")

# Save processed data to text files -----------------------
write.csv(wpa, 'wpa_log_filtered.csv', row.names = FALSE)
write.csv(wpa_count, 'wpa_log_filtered_counts.csv', row.names = FALSE)


# Plots ---------------------------------------------------

# Trim bssid lables to last 4 hex digits and add "GHz"
wpa_count$bssid_label <- with(wpa_count,
  paste0(sub("............", "", bssid), " (", range, " - ", channel, ")"))

# Histogram
ggplot(wpa_count, aes(x=reorder(bssid_label, count, sum), y=count, fill=status)) +
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values=c("#666666", "#EE6666", "#0088DD"),
    guide = guide_legend(reverse=TRUE)) +
  xlab("Abbreviated BSSID - all start with e0:1c:41:1e") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=12))
ggsave("connection_counts.png", width=8, height=5, dpi=100)


# Connections over time
wpa2 <- wpa
wpa2$day <- as.Date(wpa2$time)
wpa2$time_noday <- wpa2$time - difftime(wpa2$day, Sys.Date())
ggplot(wpa2, aes(x=time_noday, y=day, colour=status)) +
  geom_point(size=3, shape="|") +
  scale_colour_manual(values=c("#666666", "#EE6666", "#0088DD"),
    guide = guide_legend(reverse=TRUE))
ggsave("connection_time.png", width=24, height=5, dpi=100)
