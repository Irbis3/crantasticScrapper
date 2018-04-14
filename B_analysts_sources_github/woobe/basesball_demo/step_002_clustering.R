# Clustering

# Note: Run 'step_001_data_munging.R' first ...

# ------------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------------

suppressPackageStartupMessages(library(h2o))
suppressPackageStartupMessages(library(plotly))


# ------------------------------------------------------------------------------
# H2O
# ------------------------------------------------------------------------------

# Load H2O
h2o.init(nthreads = -1)
h_bat_avg = as.h2o(d_bat_avg)
h_pitch_avg = as.h2o(d_pitch_avg)


# ------------------------------------------------------------------------------
# Analysis for Batting
# ------------------------------------------------------------------------------

# Features
features = setdiff(colnames(h_bat_avg), c("playerID",
                                          "nameFirst", "nameLast", "finalGame",
                                          "birthMonth", "birthCountry", "birthState",
                                          "birthCity"
                                          ))

# Build a K-means model for clustering
model_km_bat = h2o.kmeans(training_frame = h_bat_avg,
                          x = features,
                          nfolds = 5,
                          k = 10,
                          seed = 1234)
h_clusters = h2o.predict(model_km_bat, h_bat_avg)

# Buld a PCA model for dimensionality reduction
model_pca_bat = h2o.prcomp(training_frame = h_bat_avg,
                           x = features,
                           transform = "STANDARDIZE",
                           pca_method = "GLRM",
                           use_all_factor_levels = TRUE,
                           k = 10,
                           seed = 1234)
h_pca = h2o.predict(model_pca_bat, h_bat_avg)

# Convert data back to normal data frames
d_clusters = as.data.frame(h_clusters)
colnames(d_clusters) = "cluster"
d_pca = as.data.frame(h_pca)

# Create df for plotly
d_bat_clusters = data.frame(d_bat_avg[, 1:12],
                            clusters = d_clusters,
                            d_pca)
d_bat_clusters$cluster = as.factor(d_bat_clusters$cluster)

# Create plotly plot for all players
p1 = d_bat_clusters %>%
  plot_ly(x = ~PC1, y = ~PC2, color = ~cluster,
          type = "scatter", mode = "markers", marker = list(size = 8),
          text = ~paste(nameFirst, nameLast)) %>%
  layout(yaxis = list(title = "PCy"), xaxis = list(title = "PCx")) %>%
  layout(title = "Clusters of All Batters from 1900")
print(p1)

# Create plotly plot for active players only
p2 = d_bat_clusters %>%
  filter(finalGame == "2016-10-02") %>%
  plot_ly(x = ~PC1, y = ~PC2, color = ~cluster,
          type = "scatter", mode = "markers", marker = list(size = 8),
          text = ~paste(nameFirst, nameLast)) %>%
  layout(yaxis = list(title = "PCy"), xaxis = list(title = "PCx")) %>%
  layout(title = "Clusters of Active Batters")
print(p2)

# ------------------------------------------------------------------------------
# Analysis for Pitching
# ------------------------------------------------------------------------------

# Features
features = setdiff(colnames(h_pitch_avg), c("playerID",
                                          "nameFirst", "nameLast", "finalGame",
                                          "birthMonth", "birthCountry", "birthState",
                                          "birthCity"
))

# Build a K-means model for clustering
model_km_pitch = h2o.kmeans(training_frame = h_pitch_avg,
                            x = features,
                            nfolds = 5,
                            k = 10,
                            seed = 1234)
h_clusters = h2o.predict(model_km_pitch, h_pitch_avg)

# Buld a PCA model for dimensionality reduction
model_pca_pitch = h2o.prcomp(training_frame = h_pitch_avg,
                           x = features,
                           transform = "STANDARDIZE",
                           pca_method = "GLRM",
                           use_all_factor_levels = TRUE,
                           k = 10,
                           seed = 1234)
h_pca = h2o.predict(model_pca_pitch, h_pitch_avg)

# Convert data back to normal data frames
d_clusters = as.data.frame(h_clusters)
colnames(d_clusters) = "cluster"
d_pca = as.data.frame(h_pca)

# Create df for plotly
d_pitch_clusters = data.frame(d_pitch_avg,
                            clusters = d_clusters,
                            d_pca)
d_pitch_clusters$cluster = as.factor(d_pitch_clusters$cluster)

# Create plotly plot for all players
p3 = d_pitch_clusters %>%
  plot_ly(x = ~PC2, y = ~PC3, color = ~cluster,
          type = "scatter", mode = "markers", marker = list(size = 8),
          text = ~paste(nameFirst, nameLast)) %>%
  layout(yaxis = list(title = "PCy"), xaxis = list(title = "PCx")) %>%
  layout(title = "Clusters of All Pitchers from 1900")
print(p3)

# Create plotly plot for active players only
p4 = d_pitch_clusters %>%
  filter(finalGame == "2016-10-02") %>%
  plot_ly(x = ~PC2, y = ~PC3, color = ~cluster,
          type = "scatter", mode = "markers", marker = list(size = 8),
          text = ~paste(nameFirst, nameLast)) %>%
  layout(yaxis = list(title = "PCy"), xaxis = list(title = "PCx")) %>%
  layout(title = "Clusters of Active Pitchers")
print(p4)

