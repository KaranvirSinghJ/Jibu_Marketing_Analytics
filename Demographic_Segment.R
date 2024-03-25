library(dplyr)
library(cluster)
library(factoextra)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(janitor)
library(lubridate)
library(methods)
library(modelr)
library(dplyr)
library(rfm)



# Read the data from Excel
jobu_demo <- readxl::read_excel("/Users/chandrle/marketting_rstudio/boj_raw_data.xlsx")

jobu_demo<-clean_names(jobu_demo)
head(jobu_demo)
summary(jobu_demo)

# Select relevant features for clustering
features <- select(jobu_demo, franchise_age, average_litres_produced_per_day, volumeto_retailers, volumeto_businesses,volumeto_hh)

# Standardize numerical variables
scaled_features <- scale(features)

# Determine optimal number of clusters using the elbow method
wss <- numeric(10)
for (i in 1:10) {
  wss[i] <- sum(kmeans(scaled_features, centers = i)$withinss)
}
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within-cluster Sum of Squares")

# Based on the plot, select the optimal number of clusters (elbow point)

# Perform K-means clustering
k <- 3  # Assuming 3 clusters based on the elbow method
kmeans_model <- kmeans(scaled_features, centers = k)

# Add cluster labels to the original data
jobu_demo$cluster <- kmeans_model$cluster

# Analyze and interpret the resulting clusters
summary(jobu_demo$cluster)  # Summary statistics of each cluster

# Visualize clustering results
fviz_cluster(kmeans_model, data = scaled_features)



gender_summary <- jobu_demo %>%
  group_by(franchisee_gender) %>%
  summarise(mean_litres_produced_per_day = mean(average_litres_produced_per_day))

# Print the summary
print(gender_summary)
# Create a bar plot for AverageLitresProducedPerDay by FranchiseeGender
ggplot(jobu_demo, aes(x = franchisee_gender, y = average_litres_produced_per_day, fill = franchisee_gender)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Litres Produced Per Day by Franchisee Gender",
       x = "Franchisee Gender",
       y = "Average Litres Produced Per Day") +
  theme_minimal()


# Plot AverageLitresProducedPerDay by FranchiseAge
ggplot(jobu_demo, aes(x = franchisee_age, y = average_litres_produced_per_day)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Average Litres Produced per Day by Franchise Age",
       x = "Franchise Age (years)",
       y = "Average Litres Produced per Day")

# Plot FranchiseeAge distribution
ggplot(jobu_demo, aes(x = franchise_age)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Franchisee Age",
       x = "Franchisee Age Group",
       y = "Frequency")

# Plot FranchiseeGender distribution
ggplot(jobu_demo, aes(x = franchisee_gender)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Franchisee Gender",
       x = "Franchisee Gender",
       y = "Frequency")

# Calculate frequency of FranchiseeGender
gender_frequency <- table(jobu_demo$franchisee_gender)

# Print the frequency table
print(gender_frequency)
# Plot Volume distribution by Market Segment
ggplot(jobu_demo, aes(x = franchise_name, y = volumeto_hh, fill = "Households")) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_bar(aes(y = volumeto_retailers, fill = "Retailers"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = volumeto_businesses, fill = "Businesses"), stat = "identity", position = "dodge") +
  labs(title = "Volume Distribution by Market Segment",
       x = "Franchise Name",
       y = "Volume",
       fill = "Market Segment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot VolumetoRetailers, VolumetoHH, and VolumetoBusinesses vs Country
ggplot(jobu_demo, aes(x = country)) +
  geom_bar(aes(y = volumeto_retailers, fill = "Retailers"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = volumeto_hh, fill = "Households"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = volumeto_businesses, fill = "Businesses"), stat = "identity", position = "dodge") +
  labs(title = "Volume Distribution by Country and Market Segment",
       x = "Country",
       y = "Volume",
       fill = "Market Segment") +
  theme_minimal()


# Plot VolumetoRetailers, VolumetoHH, and VolumetoBusinesses vs Country
ggplot(jobu_demo, aes(x = country)) +
  geom_bar(aes(y = volumeto_retailers, fill = "Retailers"), stat = "identity") +
  geom_bar(aes(y = volumeto_hh, fill = "Households"), stat = "identity") +
  geom_bar(aes(y = volumeto_businesses, fill = "Businesses"), stat = "identity") +
  labs(title = "Volume Distribution by Country and Market Segment",
       x = "Country",
       y = "Volume",
       fill = "Market Segment") +
  scale_y_continuous(labels = scales::comma) +  # Add commas to y-axis labels for better readability
  theme_minimal()
# Plot

# Group data by country and sum up the volumes for each market segment
segment_totals <- jobu_demo %>%
  group_by(country) %>%
  summarise(
    Total_VolumetoRetailers = sum(volumeto_retailers),
    Total_VolumetoHH = sum(volumeto_hh),
    Total_VolumetoBusinesses = sum(volumeto_businesses)
  )

# Print the resulting data frame
print(segment_totals)


# Plot VolumetoRetailers, VolumetoHH, and VolumetoBusinesses vs FranchiseeGender
ggplot(jobu_demo, aes(x = franchisee_gender)) +
  geom_bar(aes(y = volumeto_retailers, fill = "Retailers"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = volumeto_hh, fill = "Households"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = volumeto_businesses, fill = "Businesses"), stat = "identity", position = "dodge") +
  labs(title = "Volume Distribution by Franchisee Gender and Market Segment",
       x = "Franchisee Gender",
       y = "Volume",
       fill = "Market Segment") +
  theme_minimal()
