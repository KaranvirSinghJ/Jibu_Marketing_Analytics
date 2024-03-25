library(readxl)
library(dplyr)
library(ggplot2)
library(cluster) # For clustering

# Load the data (adjust the path as necessary)

# Assuming the relevant segmentation data is similar across sheets and can be combined
# Read the data from the first sheet to start
data <-BoJScore_Final 

# For demonstration, assuming 'Average Litres Produced Per Day (Last 3 months)' and 'BOJ Total Score' 
# could be proxies for customer engagement or satisfaction

# Basic cleaning and feature engineering
data_clean <- data %>%
  filter(!is.na(`Average Litres Produced Per Day (Last 3 months)`), !is.na(`BOJ Total Score`)) %>%
  mutate(CustomerSegment = cut(`BOJ Total Score`, breaks = quantile(`BOJ Total Score`, probs = 0:4 / 4), include.lowest = TRUE, labels = FALSE))

# Basic EDA
ggplot(data_clean, aes(x = `BOJ Total Score`)) +
  geom_histogram(fill = "blue", bins = 30) +
  theme_minimal() +
  labs(title = "Distribution of BOJ Total Scores", x = "BOJ Total Score", y = "Count")

ggplot(data_clean, aes(x = `Average Litres Produced Per Day (Last 3 months)`, y = `BOJ Total Score`, color = as.factor(CustomerSegment))) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "BOJ Total Score vs. Average Litres Produced", x = "Average Litres Produced Per Day (Last 3 months)", y = "BOJ Total Score")

# Example of clustering (e.g., K-means)
set.seed(123) # For reproducibility
kmeans_result <- kmeans(data_clean[, c('Average Litres Produced Per Day (Last 3 months)', 'BOJ Total Score')], centers = 4)
data_clean$Cluster <- as.factor(kmeans_result$cluster)

# Plot the results
ggplot(data_clean, aes(x = `Average Litres Produced Per Day (Last 3 months)`, y = `BOJ Total Score`, color = Cluster)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Customer Segmentation with K-means", x = "Average Litres Produced Per Day (Last 3 months)", y = "BOJ Total Score")

# Remember to adjust paths, sheet names, and analysis based on your specific needs and data structure.
