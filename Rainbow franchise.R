# Load necessary libraries
library(tidyverse) # For data manipulation and visualization
library(readxl) # For reading Excel files
library(cluster) # For cluster analysis
library(caret) # For supervised learning
library(arules) # For Market Basket Analysis (MBA)
library(sna) # For network analysis
library(syuzhet) # For sentiment analysis
library(lmtest) # For multiple regression analysis
library(DAAG) # For A/B testing

# Load the data
data <- RawBoJData

# Initial data exploration
summary(data)
str(data)

# Data management: Cleaning and preparation
# Identifying missing values
sum(is.na(data))

# Numeric columns: Impute missing values with the mean
numeric_columns <- sapply(data, is.numeric)
data[numeric_columns] <- lapply(data[numeric_columns], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Categorical columns: Replace NA with "Unknown" or the most common value
categorical_columns <- sapply(data, is.character)
data[categorical_columns] <- lapply(data[categorical_columns], function(x) ifelse(is.na(x), "Unknown", x))


#preparing data for customer segmentation
# Select and scale numeric variables for clustering

data_scaled <- scale(data[numeric_columns])

#customer segmentation with K-Means Clustering

library(cluster)
set.seed(123) # For reproducibility
k <- 4 # Example, replace with chosen k
km_res <- kmeans(data_scaled, centers = k)

# Analyze cluster sizes
table(km_res$cluster)

# Visualize clusters (if possible, depending on the number of dimensions)
# Use PCA to reduce dimensions if needed
library(factoextra)
fviz_cluster(km_res, data = data_scaled)

# Examine cluster means for each variable
aggregate(data_scaled, by=list(cluster=km_res$cluster), mean)

# Load necessary libraries
library(tidyverse)
library(cluster)
library(factoextra)

# Assuming 'data' is your cleaned dataset and 'km_res' is your k-means result
# Assuming 'data_scaled' is your scaled data used for k-means

# Add the cluster assignments to the original data
data$cluster <- as.factor(km_res$cluster)

# Profile clusters by calculating the means for numerical variables within each cluster
cluster_profiles <- data %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop')

# View cluster profiles
print(cluster_profiles)

# For categorical variables, calculate the distribution across clusters
categorical_columns <- data %>%
  select_if(is.character) %>%
  names()

# Create a function to plot the distribution of a categorical variable across clusters
plot_categorical_distribution <- function(var_name) {
  data %>%
    count(cluster, !!sym(var_name)) %>%
    ggplot(aes(x = cluster, y = n, fill = !!sym(var_name))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Distribution of", var_name, "across Clusters"),
         x = "Cluster",
         y = "Count") +
    theme_minimal()
}

# Apply the function to each categorical variable
plots <- map(categorical_columns, plot_categorical_distribution)

# If you want to view a specific plot, for example for the first categorical variable
print(plots[[1]])

# Analyzing intra-cluster correlations for numerical variables
# This step could help identify which variables are most characteristic of each cluster
numerical_columns <- data %>%
  select_if(is.numeric) %>%
  names()

# Correlation plot for each cluster
for (i in 1:k) {
  cat("Correlation plot for Cluster", i, ":\n")
  data_cluster <- data %>% filter(cluster == i) %>% select(all_of(numerical_columns))
  correlations <- cor(data_cluster, use = "pairwise.complete.obs")
  
  # Use factoextra to visualize the correlation matrix
  fviz_corr(correlations, label = TRUE) + labs(title = paste("Correlation Matrix for Cluster", i))
}


