# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Customer segmentation by type and engagement level
customer_segmentation <- df %>%
  mutate(Total_Active_Customers = total_number_of_active_customers_purchased_atleast_once_this_month_retailers_number_of_customers +
           total_number_of_active_customers_purchased_atleast_once_this_month_households_number_of_customers +
           total_number_of_active_customers_purchased_atleast_once_this_month_businesses_institutions_number_of_customers,
         Total_New_Customers = total_number_of_new_customers_recruited_at_the_franchise_this_month_households_number_of_customers +
           total_number_of_new_customers_recruited_at_the_franchise_this_month_businesses_institutions_number_of_customers) %>%
  group_by(country, state, city) %>%
  summarise(Average_Active_Customers = mean(Total_Active_Customers),
            Average_New_Customers = mean(Total_New_Customers),
            Average_Hygiene_Score = mean(franchise_hygiene_audit_score_months_average),
            Average_PQ_Score = mean(p_q_audit_score_current_month)) %>%
  arrange(desc(Average_Active_Customers))

# Visualization for Average Number of Active Customers
ggplot(customer_segmentation, aes(x = reorder(city, Average_Active_Customers), y = Average_Active_Customers)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average Number of Active Customers by City",
       x = "City",
       y = "Average Number of Active Customers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualization for Average Number of New Customers
ggplot(customer_segmentation, aes(x = reorder(city, Average_New_Customers), y = Average_New_Customers)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Average Number of New Customers by City",
       x = "City",
       y = "Average Number of New Customers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualization for Average Hygiene Score
ggplot(customer_segmentation, aes(x = reorder(city, Average_Hygiene_Score), y = Average_Hygiene_Score)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Average Hygiene Score by City",
       x = "City",
       y = "Average Hygiene Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualization for Average PQ Score
ggplot(customer_segmentation, aes(x = reorder(city, Average_PQ_Score), y = Average_PQ_Score)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Average PQ Score by City",
       x = "City",
       y = "Average PQ Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
