library(tm)
library(topicmodels)
library(tidytext)
library(textstem)
library(ldatuning)
library(lexicon)
library(janitor)
library(tidyr)
library(tidyverse)
library(tidyselect)
library(caret)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(factoextra)
library(arules)
library(arulesViz)
library(readr)
library(dplyr)
library(lubridate)
library(methods)
library(modelr)
library(data.table)
library(ggplot2)
library(e1071)
library(corrplot)
library(cluster)
library(factoextra)
library(devtools)
library('revgeo')
install_github('mhudecheck/revgeo')
library(maps)
library("openxlsx")

franHealthData<-as.data.frame(Franchise_Health_Data_Input_Form_2023_2024_02_08)
franHealthData<-clean_names(franHealthData)

franHealthData_subset<-data.frame(franHealthData$franchise_name, franHealthData$franchise_age, 
                                  franHealthData$franchise_hygiene_audit_score_months_average, franHealthData$p_q_audit_score_current_month,
                                  franHealthData$percent_water_wastage_this_month, 
                                  franHealthData$targets_achievement_average_liters_per_day_for_the_month_actual,
                                  franHealthData$targets_achievement_average_liters_per_day_for_the_month_target,
                                  franHealthData$targets_achievement_lpg_target_purchases_achieved_for_the_month_target,
                                  franHealthData$targets_achievement_lpg_target_purchases_achieved_for_the_month_actual,
                                  franHealthData$targets_achievement_porridge_target_purchases_achieved_for_the_month_target,
                                  franHealthData$targets_achievement_porridge_target_purchases_achieved_for_the_month_actual)

franHealthData_subset<-clean_names(franHealthData_subset)

franHealthData_subset<-na.omit(franHealthData_subset)

ggplot(franHealthData_subset, aes(x = fran_health_data_franchise_name, y = fran_health_data_franchise_age)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

franHealth_dendo<-hclust(dist(franHealthData_subset$fran_health_data_franchise_age))
plot(franHealth_dendo, main = "Franchise age since their openning", xlab = "Category", sub = "")


# Define the breaks for age groups
breaks <- c(0, 30, 60, 100)

# Create age groups using cut function
franHealthData_subset$age_group <- cut(franHealthData_subset$fran_health_data_franchise_age, breaks = breaks, labels = c("0-30", "31-60", "61-100"))

# Create a bar plot to visualize the age groups
ggplot(franHealthData_subset, aes(x = age_group)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Age Group Segmentation", x = "Age Group", y = "Count") +
  theme_minimal()


ggplot(franHealthData_subset) +
  geom_line(aes(x = seq_along(fran_health_data_franchise_hygiene_audit_score_months_average),
                y = fran_health_data_franchise_hygiene_audit_score_months_average), color = "blue") +
  geom_line(aes(x = seq_along(fran_health_data_p_q_audit_score_current_month),
                y = fran_health_data_p_q_audit_score_current_month), color = "red") +
  labs(title = "Audits", x = "Index", y = "Value") +
  theme_minimal()

franHealthData_location_subset<-data.frame(franchise_name=franHealthData$franchise_name, country=franHealthData$country, 
                                           country_longitude=franHealthData$country_location_answered_longitude,
                                           country_latitude=franHealthData$country_location_answered_latitude, 
                                           country_altitude=franHealthData$country_location_answered_altitude,
                                           country_accuracy=franHealthData$country_location_answered_accuracy,
                                           franchise_latitude=franHealthData$franchise_name_location_answered_latitude,
                                           franchise_longitude=franHealthData$franchise_name_location_answered_longitude,
                                           franchise_altitude=franHealthData$franchise_name_location_answered_altitude,
                                           franchise_accuracy=franHealthData$franchise_name_location_answered_accuracy)



franHealthData_location_subset<-clean_names(franHealthData_location_subset)
franHealthData_location_subset<-na.omit(franHealthData_location_subset)

#country kmeans analysis
country_location<-franHealthData_location_subset[3:6]
distance=dist(country_location)
country_location_h<-hclust(distance)
plot(country_location_h)
set.seed(2024)
cluster_country_location<-kmeans(country_location,4,nstart = 20)

location_analysis<-as.data.frame(cluster_country_location$centers)
location_analysis$cluster<-c("Kenya","Rwanda","DRC Goma","Uganda")
summary(aov(country_longitude~cluster,location_analysis))
summary(aov(country_latitude~cluster,location_analysis))
list_of_names<-c("Kenya","Rwanda","DRC Goma","Uganda")
fviz_cluster(cluster_country_location,country_location)+
  scale_fill_discrete(labels=c("Kenya","Rwanda","DRC Goma","Uganda")) +
  scale_color_discrete(labels = c("Kenya","Rwanda","DRC Goma","Uganda"))

fviz_nbclust(country_location,kmeans,method="silhouette")


#Water sold per day in liters
avg_liters_daily<-data.frame(franchise_name=franHealthData$franchise_name, 
                             avg_liters_per_day_actual=franHealthData$targets_achievement_average_liters_per_day_for_the_month_actual,
                             avg_liters_per_day_target=franHealthData$targets_achievement_average_liters_per_day_for_the_month_target)

avg_liters_daily<-clean_names(avg_liters_daily)
avg_liters_daily<-na.omit(avg_liters_daily)
avg_liters_daily_numbers<-avg_liters_daily[2:3]
distance=dist(avg_liters_daily_numbers)
avg_liters_daily_numbers_h<-hclust(distance)
plot(avg_liters_daily_numbers_h)

set.seed(2024)
cluster_avg_liters_daily_numbers<-kmeans(avg_liters_daily_numbers,2,nstart = 20)
mydata$cluster<-cluster_avg_liters_daily_numbers$cluster
daily_analysis<-as.data.frame(cluster_avg_liters_daily_numbers$centers)
daily_analysis$cluster<-c("Kenya","Rwanda","DRC Goma","Uganda")
summary(aov(avg_liters_per_day_actual~cluster,daily_analysis))
summary(aov(avg_liters_per_day_target~cluster,daily_analysis))
list_of_names<-c("Kenya","Rwanda","DRC Goma","Uganda")
fviz_cluster(cluster_avg_liters_daily_numbers,avg_liters_daily_numbers)+
  scale_fill_discrete(labels=c("Kenya","Rwanda","DRC Goma","Uganda")) +
  scale_color_discrete(labels = c("Kenya","Rwanda","DRC Goma","Uganda"))

fviz_nbclust(avg_liters_daily_numbers,kmeans,method="silhouette")


# Bottles sold in actual vs targeted
franHealthData_bottles_sold<-data.frame(fran_name=franHealthData$franchise_name,
                                        ip_address=franHealthData$ip_address,
                                        record_date=franHealthData$submitted_on,
                                        profit_loss_for_month=franHealthData$profit_loss_position_for_this_month,
                                        total_20l_bottles_sold_in_month=franHealthData$total_number_of_new_bottles_sold_this_month_20l_tap_bottle_number_of_bottles,
                                        total_18_9l_bottles_sold_in_month=franHealthData$total_number_of_new_bottles_sold_this_month_18_9l_bottle_jumbos_number_of_bottles,
                                        total_20l_jerrican_bottles_sold_in_month=franHealthData$total_number_of_new_bottles_sold_this_month_20l_jerrican_number_of_bottles,
                                        total_10l_bottles_sold_in_month=franHealthData$total_number_of_new_bottles_sold_this_month_10l_bottle_jerrican_number_of_bottles,
                                        total_5l_bottles_sold_in_month=franHealthData$total_number_of_new_bottles_sold_this_month_5l_bottle_jerrican_number_of_bottles,
                                        total_20l_refill_bottles_sold_in_month=franHealthData$total_number_of_refill_bottles_sold_this_month_number_of_20l_18_9l_jumbo_bottles_20l_tap_bottle_number_of_bottles,
                                        total_18_9l_refill_bottles_sold_in_month=franHealthData$total_number_of_refill_bottles_sold_this_month_number_of_20l_18_9l_jumbo_bottles_18_9l_bottle_jumbos_number_of_bottles,
                                        total_20l_jerrican_refill_bottles_sold_in_month=franHealthData$total_number_of_refill_bottles_sold_this_month_number_of_20l_18_9l_jumbo_bottles_20l_jerricans_number_of_bottles,
                                        total_5l_refill_bottles_sold_in_month=franHealthData$total_number_of_refill_bottles_sold_this_month_number_of_20l_18_9l_jumbo_bottles_5l_bottle_jerricans_number_of_bottles,
                                        total_10l_refill_bottles_sold_in_month=franHealthData$total_number_of_refill_bottles_sold_this_month_number_of_20l_18_9l_jumbo_bottles_10l_bottle_jerricans_number_of_bottles)

franHealthData_bottles_sold<-clean_names(franHealthData_bottles_sold)

franHealthData_bottles_sold<-na.omit(franHealthData_bottles_sold)
summary(franHealthData_bottles_sold)
any(is.na(franHealthData_bottles_sold))
present <- as.Date("2023-12-18")


franHealthData_bottles_sold$record_date <- as.Date(franHealthData_bottles_sold$record_date, format = "%m/%d/%y")
rfm <- franHealthData_bottles_sold %>%
  group_by(ip_address) %>%
  summarise(
    Recency = as.numeric(difftime(present, max(record_date), units = "days")), # Recency
    Frequency = n(), # Frequency
    MonetaryValue = sum(profit_loss_for_month) # Monetary Value
  )


# Rename columns
colnames(rfm) <- c("ipAddress", "Recency", "Frequency", "MonetaryValue")


rfm$MonetaryValue <- as.integer(rfm$MonetaryValue)

ip_to_numeric <- function(ip_address) {
  if (grepl(":", ip_address)) {
    groups <- unlist(strsplit(ip_address, ":"))
    integer_values <- as.integer(groups, base=16)
    numeric_value <- sum(integer_values * 2^(16 * (7:0)))
    return(numeric_value)
  } else {
    octets <- as.integer(strsplit(ip_address, "\\.")[[1]])
    numeric_value <- sum(octets * c(256^3, 256^2, 256, 1))
    return(numeric_value)
  }
}

rfm$ipAddress<-sapply(rfm$ipAddress,ip_to_numeric)
rfm<-na.omit(rfm)
summary(rfm)


# Compute correlation matrix
correlation_matrix <- cor(rfm)

# Create heatmap with annotations
corrplot(correlation_matrix, method = "color", type = "upper", addCoef.col = "black", tl.col = "black", tl.srt = 45)






























# Finding cities based on logitutde, latitude of the cities
results<-revgeo(longitude=franHealthData_location_subset$country_longitude, latitude=franHealthData_location_subset$country_latitude, 
                provider = 'photon', output="frame") %>% mutate(franchise_name = franHealthData_location_subset$franchise_name)

#write.xlsx(results, 'locations.xlsx')
newFranHealthData<-data.frame(franchise_name=franHealthData$franchise_name,
                              total_number_of_active_customers_purchased_atleast_once_this_month_retailers_number_of_customers=franHealthData$total_number_of_active_customers_purchased_atleast_once_this_month_retailers_number_of_customers,
                              total_number_of_active_customers_purchased_atleast_once_this_month_households_number_of_customers=franHealthData$total_number_of_active_customers_purchased_atleast_once_this_month_households_number_of_customers,
                              total_number_of_active_customers_purchased_atleast_once_this_month_businesses_institutions_number_of_customers=franHealthData$total_number_of_active_customers_purchased_atleast_once_this_month_businesses_institutions_number_of_customers,
                              total_number_of_new_customers_recruited_at_the_franchise_this_month_retailers_number_of_customers=franHealthData$total_number_of_new_customers_recruited_at_the_franchise_this_month_retailers_number_of_customers,
                              total_number_of_new_customers_recruited_at_the_franchise_this_month_households_number_of_customers=franHealthData$total_number_of_new_customers_recruited_at_the_franchise_this_month_households_number_of_customers,
                              total_number_of_new_customers_recruited_at_the_franchise_this_month_businesses_institutions_number_of_customers=franHealthData$total_number_of_new_customers_recruited_at_the_franchise_this_month_businesses_institutions_number_of_customers,
                              franchise_hygiene_audit_score_months_average=franHealthData$franchise_hygiene_audit_score_months_average,
                              p_q_audit_score_current_month=franHealthData$p_q_audit_score_current_month,
                              month=franHealthData$month,
                              profit_and_loss=franHealthData$profit_loss_position_for_this_month,
                              average_liters_per_day_target=franHealthData$targets_achievement_average_liters_per_day_for_the_month_target,
                              average_liters_per_day_actual=franHealthData$targets_achievement_average_liters_per_day_for_the_month_actual,
                              avtivation_sponsorship_promotion_this_month=franHealthData$number_of_activations_sponsorships_promotions_held_by_franchise_this_month,
                              total_new_bottles_20L=franHealthData$total_number_of_new_bottles_sold_this_month_20l_tap_bottle_number_of_bottles,
                              total_new_bottles_20L_jerrican=franHealthData$total_number_of_new_bottles_sold_this_month_20l_jerrican_number_of_bottles,
                              total_new_bottles_18_9L=franHealthData$total_number_of_new_bottles_sold_this_month_18_9l_bottle_jumbos_number_of_bottles,
                              total_new_bottles_10L=franHealthData$total_number_of_new_bottles_sold_this_month_10l_bottle_jerrican_number_of_bottles,
                              total_new_bottles_5L=franHealthData$total_number_of_new_bottles_sold_this_month_5l_bottle_jerrican_number_of_bottles,
                              total_refill_bottles_20L=franHealthData$total_number_of_refill_bottles_sold_this_month_number_of_20l_18_9l_jumbo_bottles_20l_tap_bottle_number_of_bottles,
                              total_refill_bottles_18_9L=franHealthData$total_number_of_refill_bottles_sold_this_month_number_of_20l_18_9l_jumbo_bottles_18_9l_bottle_jumbos_number_of_bottles,
                              total_refill_bottles_20L_jerrican=franHealthData$total_number_of_refill_bottles_sold_this_month_number_of_20l_18_9l_jumbo_bottles_20l_jerricans_number_of_bottles,
                              total_refill_bottles_10L=franHealthData$total_number_of_refill_bottles_sold_this_month_number_of_20l_18_9l_jumbo_bottles_10l_bottle_jerricans_number_of_bottles,
                              total_refill_bottles_5L=franHealthData$total_number_of_refill_bottles_sold_this_month_number_of_20l_18_9l_jumbo_bottles_5l_bottle_jerricans_number_of_bottles,
                              customers_rewarded_numbers=franHealthData$number_customers_rewarded_by_franchise_this_month)

newFranHealthData<-na.omit(newFranHealthData)


df1 <- newFranHealthData[, c("total_number_of_active_customers_purchased_atleast_once_this_month_retailers_number_of_customers",
                          "total_number_of_active_customers_purchased_atleast_once_this_month_households_number_of_customers",
                          "total_number_of_active_customers_purchased_atleast_once_this_month_businesses_institutions_number_of_customers",
                          "total_number_of_new_customers_recruited_at_the_franchise_this_month_retailers_number_of_customers",
                          "total_number_of_new_customers_recruited_at_the_franchise_this_month_households_number_of_customers",
                          "total_number_of_new_customers_recruited_at_the_franchise_this_month_businesses_institutions_number_of_customers",
                          "franchise_hygiene_audit_score_months_average", "p_q_audit_score_current_month","franchise_name","month",
                          "profit_and_loss", "average_liters_per_day_target","average_liters_per_day_actual","avtivation_sponsorship_promotion_this_month",
                          "total_new_bottles_20L","total_new_bottles_20L_jerrican","total_new_bottles_18_9L","total_new_bottles_10L",
                          "total_new_bottles_5L","total_refill_bottles_20L","total_refill_bottles_18_9L","total_refill_bottles_20L_jerrican","total_refill_bottles_10L",
                          "total_refill_bottles_5L","customers_rewarded_numbers")]
df2 <- results[, c("franchise_name", "country", "state","city","street")]

# Combine selected columns into a new dataframe
new_df <- merge(df2, df1, by = "franchise_name", all.x = TRUE)
cleaned_dataset <- subset(new_df, country != "Country Not Found")
cleaned_dataset <- subset(new_df, state != "State Not Found")
cleaned_dataset <- subset(new_df, city != "City Not Found")
cleaned_dataset <- subset(new_df, street != "Street Not Found")

file_ready<-na.omit(cleaned_dataset)
file_ready<-subset(file_ready, city != "City Not Found")

new_file<-unique(file_ready)



# creating new cleaned data file out of the locations and other variables
write.xlsx(new_file, 'franchise_health_data.xlsx')






updated_file<-as.data.frame(franchise_health_data2)


performance_variable<-data.frame(franchise_name=updated_file$franchise_name, 
                             total_active_customer_retailer=updated_file$total_number_of_active_customers_purchased_atleast_once_this_month_retailers_number_of_customers,
                             total_active_customer_business=updated_file$total_number_of_active_customers_purchased_atleast_once_this_month_businesses_institutions_number_of_customers,
                             total_active_customer_household=updated_file$total_number_of_active_customers_purchased_atleast_once_this_month_households_number_of_customers,
                             total_new_recruited_customer_retailer=updated_file$total_number_of_new_customers_recruited_at_the_franchise_this_month_retailers_number_of_customers,
                             total_new_recruited_customer_business=updated_file$total_number_of_new_customers_recruited_at_the_franchise_this_month_businesses_institutions_number_of_customers,
                             total_new_recruited_customer_household=updated_file$total_number_of_new_customers_recruited_at_the_franchise_this_month_households_number_of_customers)



performance_variable<-clean_names(performance_variable)
performance_variable<-na.omit(performance_variable)
performance_variable<-performance_variable[2:7]
distance=dist(performance_variable)
performance_variable_h<-hclust(distance)
plot(performance_variable_h)

set.seed(2024)
performance_variable<-na.omit(performance_variable)
cluster_performance_variable<-kmeans(performance_variable,3,nstart = 20)
performance_analysis<-as.data.frame(cluster_performance_variable$centers)
fviz_cluster(cluster_performance_variable,performance_variable)



fviz_nbclust(avg_liters_daily_numbers,kmeans,method="silhouette")



active_performance_variable<-subset(performance_variable[1:4])
active_performance_variable<-clean_names(active_performance_variable)
active_performance_variable<-na.omit(active_performance_variable)
active_performance_variable_num<-active_performance_variable[2:4]
distance=dist(active_performance_variable_num)
active_performance_variable_h<-hclust(distance)
plot(active_performance_variable_h)
set.seed(2024)
cluster_active_performance_variable<-kmeans(active_performance_variable_num,3,nstart = 20)
active_performance_analysis<-as.data.frame(cluster_active_performance_variable$centers)
fviz_cluster(cluster_active_performance_variable,active_performance_variable_num)





new_performance_variable<-subset(performance_variable[, c(1,5:7)])
new_performance_variable<-clean_names(new_performance_variable)
new_performance_variable<-na.omit(new_performance_variable)
new_performance_variable_num<-new_performance_variable[2:4]
distance=dist(new_performance_variable_num)
new_performance_variable_h<-hclust(distance)
plot(new_performance_variable_h)
set.seed(2024)
cluster_new_performance_variable<-kmeans(new_performance_variable_num,3,nstart = 20)
new_performance_analysis<-as.data.frame(cluster_new_performance_variable$centers)
fviz_cluster(cluster_new_performance_variable,new_performance_variable_num)






bottles_sold<-data.frame(franchise_name=updated_file$franchise_name, 
                            total_new_bottles_20L=updated_file$total_new_bottles_20L,
                            total_new_bottles_20L_jerrican=updated_file$total_new_bottles_20L_jerrican,
                            total_new_bottles_10L=updated_file$total_new_bottles_10L,
                            total_new_bottles_5L=updated_file$total_new_bottles_5L,
                            total_new_bottles_18_9L=updated_file$total_new_bottles_18_9L,
                            total_refill_bottles_20L=updated_file$total_refill_bottles_20L,
                            total_refill_bottles_10L=updated_file$total_refill_bottles_10L,
                            total_refill_bottles_5L=updated_file$total_refill_bottles_5L,
                            total_refill_bottles_20L_jerrican=updated_file$total_refill_bottles_20L_jerrican,
                            total_refill_bottles_18_9L=updated_file$total_refill_bottles_18_9L)
bottles_sold<-clean_names(bottles_sold)
bottles_sold<-na.omit(bottles_sold)
bottles_sold_num<-bottles_sold[2:11]
distance=dist(bottles_sold_num)
bottles_sold_h<-hclust(distance)
plot(bottles_sold_h)
set.seed(2024)
cluster_bottles_sold<-kmeans(bottles_sold_num,3,nstart = 20)
bottles_sold_analysis<-as.data.frame(cluster_bottles_sold$centers)
fviz_cluster(cluster_bottles_sold,bottles_sold_num)




#cluster for new bottles sold only
bottles_sold_new<-subset(bottles_sold[1:6])
bottles_sold_new<-clean_names(bottles_sold_new)
bottles_sold_new<-na.omit(bottles_sold_new)
bottles_sold_new_num<-bottles_sold_new[2:6]
distance=dist(bottles_sold_new_num)
bottles_sold_new_h<-hclust(distance)
plot(bottles_sold_new_h)
set.seed(2024)
cluster_bottles_sold_new<-kmeans(bottles_sold_new_num,3,nstart = 20)
bottles_sold_new_analysis<-as.data.frame(cluster_bottles_sold_new$centers)
fviz_cluster(cluster_bottles_sold_new,bottles_sold_new_num)



#cluster for refilled bottles only
bottles_sold_refill<-subset(bottles_sold[, c(1, 7:11)])
bottles_sold_refill<-clean_names(bottles_sold_refill)
bottles_sold_refill_num<-na.omit(bottles_sold_refill)
bottles_sold_refill_num<-bottles_sold_refill[2:6]
distance=dist(bottles_sold_refill_num)
bottles_sold_refill_h<-hclust(distance)
plot(bottles_sold_refill_h)
set.seed(2024)
cluster_bottles_sold_refill<-kmeans(bottles_sold_refill_num,3,nstart = 20)
bottles_sold_refill_analysis<-as.data.frame(cluster_bottles_sold_refill$centers)
fviz_cluster(cluster_bottles_sold_refill,bottles_sold_refill_num)


# Clusting audit score 
franchise_score<-data.frame(franchise_name=updated_file$franchise_name, 
                         average_month_score=updated_file$franchise_hygiene_audit_score_months_average,
                         average_p_q_score=updated_file$p_q_audit_score_current_month)
franchise_score<-clean_names(franchise_score)
franchise_score<-na.omit(franchise_score)
franchise_score_num<-franchise_score[2:3]
distance=dist(franchise_score_num)
franchise_score_h<-hclust(distance)
plot(franchise_score_h)
set.seed(2024)
cluster_franchise_score<-kmeans(franchise_score_num,3,nstart = 20)
franchise_score_analysis<-as.data.frame(cluster_franchise_score$centers)
fviz_cluster(cluster_franchise_score,franchise_score_num)




