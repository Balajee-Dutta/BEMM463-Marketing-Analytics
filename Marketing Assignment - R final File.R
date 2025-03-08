install.packages("readxl")
install.packages("tidyverse")
install.packages("factoextra")
install.packages("cluster")
install.packages("openxlsx")
install.packages("dendextend")
install.packages("ggplot2")
install.packages("car")

################################################################################
# SECTION: INSTALL & LOAD REQUIRED LIBRARIES
# This section installs (if needed) and loads all the necessary packages for 
# data manipulation, clustering, and visualization.
################################################################################


# Load required libraries
library(readxl)
library(tidyverse)
library(factoextra)
library(cluster)
library(openxlsx)
library(dendextend)
library(ggplot2)
library(car)

################################################################################
# SECTION: IMPORT DATA & INITIAL EXPLORATION
# This section reads the smartwatch data from an Excel file, shows basic 
# information about the data, and allows you to inspect its structure.
################################################################################

# Read data from Excel (replace file.choose() with your file path if needed)
smartwatch_data <- read_excel(file.choose())

# Peek at data
names(smartwatch_data)    # View column names to understand dataset structure
summary(smartwatch_data)  # Get summary statistics of the data
view(smartwatch_data)     # Open data in an interactive viewer for inspection

################################################################################
# SECTION: SELECT VARIABLES FOR CLUSTERING
# This section picks the key attributes, standardizes them, and creates a 
# new dataframe 'dfz' ready for clustering.
################################################################################

# Selecting the key attributes relevant for segmentation analysis
attributes <- c("ConstCom", "TimelyInf", "TaskMgm", 
                "DeviceSt", "Wellness", "Athlete", "Style")

df_attr <- smartwatch_data[, attributes]

# Standardize the selected attributes to ensure equal weight in clustering
dfz <- scale(df_attr)

# View transformed data
summary(dfz)  # Check standardized values
view(dfz)     # Inspect the structure of standardized data

################################################################################
# SECTION: DETERMINE OPTIMAL NUMBER OF CLUSTERS
# This section uses the Elbow Method, a hierarchical dendrogram (for visual 
# reference), and the Silhouette Method to help decide the best number of clusters.
################################################################################

#--- Elbow Method ---
fviz_nbclust(dfz, FUN = kmeans, method = "wss") +
  ggtitle("Elbow Method for Determining Optimal Clusters")

# Convert to a dendrogram and highlight 4 clusters in distinct colors
hclust_result <- hclust(dist(dfz), method = "ward.D2")
dend <- as.dendrogram(hclust_result)
dend_colored <- color_branches(dend, k = 4, col = c("red", "blue", "green", "purple"))
plot(dend_colored, 
     main = "Dendrogram for Hierarchical Clustering (Further Validation)", 
     xlab = "Observations", ylab = "Height", 
     sub = "Each color denotes a distinct cluster")
# Dendrogram is used for further validation (4 clusters in distinct colors)


#--- Silhouette Method ---
fviz_nbclust(dfz, FUN = kmeans, method = "silhouette") +
  ggtitle("Silhouette Method for Determining Optimal Clusters")

# Decide on the number of clusters (example: 4)
k_elbow <- 4  
print(paste("Optimal clusters from Elbow Method:", k_elbow))

# You may override 'k_elbow' if other methods suggest a different optimal k
k <- k_elbow

################################################################################
# SECTION: APPLY K-MEANS CLUSTERING
# This section runs the K-Means algorithm on the standardized data using the 
# chosen number of clusters, merges the cluster labels with the original data, 
# and then visualizes each cluster with a single legend on the right.
################################################################################

# 1) Run K-Means
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(dfz, centers = k, nstart = 25)

# 2) Print basic info
num_clusters <- length(unique(kmeans_result$cluster))
print(paste("Number of identified clusters:", num_clusters))

# 3) Add cluster labels to the original dataset
df_final <- cbind(smartwatch_data, cluster = factor(kmeans_result$cluster))

# 4) Rename cluster assignments in 'kmeans_result' so fviz_cluster() 
#    displays descriptive labels in the legend
kmeans_result$cluster <- factor(
  kmeans_result$cluster,
  levels = 1:4, 
  labels = c("Fitness Enthusiasts", 
             "Business Professionals", 
             "Fashion-Conscious", 
             "Tech-Savvy Consumers")
)

# 5) Visualize clusters using fviz_cluster, passing ONLY numeric data ('dfz')
#    and specifying to hide the fill legend while labeling the color legend
fviz_cluster(
  kmeans_result, 
  data = dfz, 
  geom = "point", 
  ellipse.type = "convex", 
  label = "none"
) +
  scale_fill_discrete(guide = FALSE) +            
  scale_color_discrete(name = "Cluster Segments") # Show color legend

################################################################################
# SECTION: DESCRIBE EACH SEGMENT
# This section examines each cluster's size, calculates mean attribute scores, 
# assigns meaningful names, identifies the largest cluster, and creates
# segment-based and demographic-based profiles. It also plots the clusters
# with text labels, but without using get_fviz_data().
################################################################################

# Count of observations in each cluster
cluster_sizes <- table(df_final$cluster)
print(cluster_sizes)

# Calculate mean values for each attribute within each cluster
attr_means <- df_final %>%
  group_by(cluster) %>%
  summarise(across(all_of(attributes), mean, .names = "{.col}_mean"))
print(attr_means)
view(attr_means)

# Assign meaningful names to clusters based on characteristics
# Create a data frame from dfz
dfz_plot <- as.data.frame(dfz)

# Add a Cluster column
dfz_plot$Cluster <- kmeans_result$cluster
segment_names <- c("Fitness Enthusiasts", 
                   "Business Professionals", 
                   "Fashion-Conscious", 
                   "Tech-Savvy Consumers")
names(segment_names) <- levels(df_final$cluster)
df_final$Segment<- segment_names[as.character(df_final$cluster)]
dfz_plot$Segment<- segment_names[as.character(dfz_plot$Cluster)]

# 4) Create a cluster plot object without displaying it (plot=FALSE)
p_new <- fviz_cluster(
  kmeans_result,
  data         = dfz,       
  geom         = "point",
  ellipse.type = "convex",
  label        = "none",
  plot         = FALSE
)

# 5) Build a custom ggplot using p_new$data for x,y and dfz_plot$Segment for labels
ggplot(p_new$data, aes(x = x, y = y, color = cluster)) +
  geom_point() +
  geom_text(
    aes(label = dfz_plot$Segment),
    size          = 4, 
    vjust         = -0.10, 
    check_overlap = TRUE
  ) +
  scale_color_discrete(name = "Cluster Segments") +
  scale_fill_discrete(guide = FALSE) +
  ggtitle("K-Means Cluster Plot with Named Segments") +
  theme_minimal()


# Count the number of respondents in each segment
cluster_count <- df_final %>%
  group_by(Segment) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Print cluster counts
print(cluster_count)

# Identify and print the biggest cluster
largest_cluster <- cluster_count[1, ]
cat("The biggest cluster/segment is:", largest_cluster$Segment, 
    "with", largest_cluster$Count, "respondents.\n")

# Generate segment-based attributes profile
df_segment_profile <- df_final %>%
  group_by(cluster) %>%
  summarise(across(all_of(attributes), mean, na.rm = TRUE))
print(df_segment_profile)
view(df_segment_profile)  # Open interactive viewer for segment-based profile

# Generate demographic-based profile
demographic_attributes <- c("AmznP", "Female", "Degree", "Income", "Age")
df_demographic_profile <- df_final %>%
  group_by(cluster) %>%
  summarise(across(all_of(demographic_attributes), mean, na.rm = TRUE))
print(df_demographic_profile)
view(df_demographic_profile)  # Open interactive viewer for demographic-based profile

# Convert data to long format for better visualization
df_long <- df_final %>%
  pivot_longer(cols = all_of(attributes), names_to = "Attribute", values_to = "Score")


################################################################################
# SECTION: CHOOSE BEST SEGMENT FOR INTEL
# This section visualizes segment sizes and provides bar charts for each cluster 
# to assess which segment might be most suitable for Intel's focus.
################################################################################

# Visualize overall segment size
ggplot(df_final, aes(x = Segment, fill = Segment)) +
  geom_bar() +
  theme_minimal() +
  ggtitle("Market Size of Each Segment")

# Bar Chart for Cluster 1
cluster_1 <- df_final %>% filter(cluster == 1)
cluster_1_means <- colMeans(cluster_1 %>% select(all_of(attributes)))
cluster_1_df <- data.frame(Attribute = names(cluster_1_means), Mean = cluster_1_means)

ggplot(cluster_1_df, aes(x = Attribute, y = Mean, fill = Attribute)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Cluster 1 Bar Chart", x = "Attributes", y = "Mean Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar Chart for Cluster 2
cluster_2 <- df_final %>% filter(cluster == 2)
cluster_2_means <- colMeans(cluster_2 %>% select(all_of(attributes)))
cluster_2_df <- data.frame(Attribute = names(cluster_2_means), Mean = cluster_2_means)

ggplot(cluster_2_df, aes(x = Attribute, y = Mean, fill = Attribute)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Cluster 2 Bar Chart", x = "Attributes", y = "Mean Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar Chart for Cluster 3
cluster_3 <- df_final %>% filter(cluster == 3)
cluster_3_means <- colMeans(cluster_3 %>% select(all_of(attributes)))
cluster_3_df <- data.frame(Attribute = names(cluster_3_means), Mean = cluster_3_means)

ggplot(cluster_3_df, aes(x = Attribute, y = Mean, fill = Attribute)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Cluster 3 Bar Chart", x = "Attributes", y = "Mean Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar Chart for Cluster 4
cluster_4 <- df_final %>% filter(cluster == 4)
cluster_4_means <- colMeans(cluster_4 %>% select(all_of(attributes)))
cluster_4_df <- data.frame(Attribute = names(cluster_4_means), Mean = cluster_4_means)

ggplot(cluster_4_df, aes(x = Attribute, y = Mean, fill = Attribute)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Cluster 4 Bar Chart", x = "Attributes", y = "Mean Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################################################
# SECTION: VALIDATION USING ANOVA TEST
# This section runs ANOVA across all chosen attributes to confirm that the 
# clusters differ significantly, then visualizes the F-values.
################################################################################

anova_results <- lapply(attributes, function(var) {
  aov(as.formula(paste(var, "~ cluster")), data = df_final) 
})
anova_results <- lapply(anova_results, summary)

names(anova_results) <- attributes
print(anova_results)

anova_df <- data.frame(
  Variable = attributes,
  F_Value = sapply(anova_results, function(x) x[[1]]["cluster", "F value"]),
  P_Value = sapply(anova_results, function(x) x[[1]]["cluster", "Pr(>F)"])
)

ggplot(anova_df, aes(x = F_Value,
                     y = reorder(Variable, F_Value),
                     color = P_Value < 0.05)) +
  geom_point(size = 5) +
  theme_minimal() +
  labs(title = "ANOVA F-Values by Attribute",
       x = "F-Value",
       y = "Attribute",
       color = "Significant (p < 0.05)")


