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
  
  jibu <- readxl::read_excel("/Users/chandrle/marketting_rstudio/clean.xlsx")
  
  # View the structure of the data
  str(jibu)
  
  # Summary statistics
  summary(jibu)
  
  #Clean Missing data
  # Check for missing values
  na_count <- sum(is.na(jibu))
  if (na_count > 0) {
    print(paste("There are", na_count, "missing values in the data."))
  } else {
    print("No missing values found.")
  }
  # Assuming 'jibu' is your dataframe
  # Get columns with missing data
  columns_with_missing <- colnames(jibu)[colSums(is.na(jibu)) > 0]
  
  # Print columns with missing data
  print(columns_with_missing)
  
  # Fill missing values with median
  jibu <- jibu
  for (col in colnames(jibu)) {
    if (anyNA(jibu[[col]])) {
      jibu[[col]][is.na(jibu[[col]])] <- mean(jibu[[col]], na.rm = TRUE)
    }
  }
  
  # Explore the first few rows of the data
  head(jibu)
  
  # Explore correlation matrix
  jibu<-clean_names(jibu)
  
  # Assuming 'jibu' is your data frame
  # Select relevant features for clustering
  selected_features <- jibu[, c("age_of_franchise_in_months", "average_litres_produced_per_day_last_3_months", "s_1_1_p_q_score", "s_1_3_product_color_taste_and_smell", "s_1_7_franchise_hygiene")]
  
  # Preprocessing: Standardize numerical variables
  scaled_features <- scale(selected_features)
  
  # Determine optimal number of clusters using Elbow method or other techniques
  # For example, using the Elbow method
  wss <- numeric(10)
  for (i in 1:10) {
    wss[i] <- sum(kmeans(scaled_features, centers = i)$withinss)
  }
  plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within-cluster Sum of Squares")
  
  # Based on the plot, select the optimal number of clusters (elbow point)
  
  # Perform K-means clustering
  k <- 4  # Assuming 4 clusters based on the elbow method
  kmeans_model <- kmeans(scaled_features, centers = k)
  
  # Add cluster labels to the original data
  jibu$cluster <- kmeans_model$cluster
  
  # Analyze and interpret the resulting clusters
  summary(jibu$cluster)  # Summary statistics of each cluster
  
  # 2. Data Visualization
  # Example: Histogram of 'average_litres_produced_per_day_last_3_months'
  ggplot(jibu, aes(x = average_litres_produced_per_day_last_3_months)) +
    geom_histogram(binwidth = 100)
  
  # 3. Comparative Analysis
  # Example: Boxplot of 'sales' by 'Country'
  ggplot(jibu, aes(x = country, y = sales)) +
    geom_boxplot()
  
  jibu[is.na(jibu)] <- mean(jibu, na.rm = TRUE)
  
  # Select relevant columns for segmentation analysis
  cluster_vars <- select(jibu, 
                         country, 
                         m_water_site_id_name, 
                         age_of_franchise_in_months, 
                         status, 
                         enumerator_given_name, 
                         average_litres_produced_per_day_last_3_months,
                         s_1_1_p_q_score,
                         s_1_3_product_color_taste_and_smell,
                         s_1_4_all_finished_products_are_complete,
                         s_1_5_properly_filled_bottles,
                         s_1_7_franchise_hygiene,
                         p_1_quality,
                         s_2_1_sales_targets,
                         s_3_1_customer_feedback_management,
                         p_2_sales,
                         p_3_customer_experience,
                         p_4_efficient_operations,
                         boj_total_score,
                         total,
                         quality,
                         sales,
                         cx,
                         ops)
  
  # Perform clustering analysis (example using k-means)
  kmeans_result <- kmeans(cluster_vars[, -c(1:5)], centers = 3)
  
  # Visualize clustering results
  fviz_cluster(kmeans_result, data = cluster_vars[, -c(1:5)])
  kmeans_result
  
  
  
  
  
  # Split the data by country
  unique_countries <- unique(jibu$country)
  
  # Explore each country subset
  for (country in unique_countries) {
    # Subset data for the current country
    country_data <- subset(jibu, country == country)
    
    # Summary statistics
    summary(country_data)
    
    # Visualize data distribution
    # Example: Histogram of average_litres_produced_per_day_last_3_months
    ggplot(country_data, aes(x = average_litres_produced_per_day_last_3_months)) +
      geom_histogram(binwidth = 100) +
      ggtitle(paste("Histogram of Average Litres Produced per Day (Last 3 months) -", country))
    
    # Example: Boxplot of sales by enumerator_given_name
    ggplot(country_data, aes(x = enumerator_given_name, y = sales)) +
      geom_boxplot() +
      ggtitle(paste("Boxplot of Sales by Enumerator Given Name -", country))
    
    # Example: Scatter plot of age_of_franchise_in_months vs. total
    ggplot(country_data, aes(x = age_of_franchise_in_months, y = total)) +
      geom_point() +
      ggtitle(paste("Scatter Plot of Age of Franchise in Months vs. Total -", country))
     
    # Correlation analysis
    cor_vars <- c("age_of_franchise_in_months", "average_litres_produced_per_day_last_3_months", "s_1_1_p_q_score")
    cor_matrix <- cor(country_data[, cor_vars])
    print(paste("Correlation Matrix for", country))
    print(cor_matrix)
    
    # Visualization of correlation matrix
    corrplot(cor_matrix, method = "square")
    
  }
  
  # Calculate correlation coefficients
  correlation <- cor(jibu$average_litres_produced_per_day_last_3_months, jibu[, -c(1:5)])
  
  # Display correlation coefficients
  print(correlation)
  
  
  
  