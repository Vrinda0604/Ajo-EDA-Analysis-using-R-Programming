# Load required libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

#Load the dataset
Ajio_data<-read.csv("C:/Users/hp/OneDrive/Documents/Ajio.csv")
# View the result
View(Ajio_data)

#view the first few rows of the dataset
View(head(Ajio_data))
View(head(Ajio_data,10))

#view the last few rows of the dataset
View(tail(Ajio_data))

#Check the structure of dataset
str(Ajio_data)

#summary statistics of the dataset
print(summary(Ajio_data))

#Check for missing values
colSums(is.na(Ajio_data))

#Remove duplicate rows, if any
Ajio_data<-Ajio_data%>% distinct()

#Remove rows with missing values using na.omit()
Ajio_data<-na.omit(Ajio_data)

#Rename specific columns
Ajio_data<-rename(Ajio_data,
                    shipping_time=usually_ships_within,
                    country=seller_country)
print(Ajio_data)

#Find the Average discount of products by country
avg_discount_usd_by_country<-Ajio_data%>%
  group_by(country) %>%
  summarize(avg_discount_usd=mean(discount_usd,na.rm=TRUE)) %>%
  arrange(desc(avg_discount_usd))
# View the result
View(head(avg_discount_usd_by_country))

#Identify the Most Common product type offered by brands
most_common_products<-Ajio_data%>%
  count(product_type) %>%
  arrange(desc(n)) %>%
  slice(5)
print(most_common_products)

#What are the top 10 most expensive products?
top_10_expensive_products<-Ajio_data %>%
  select(product_name,price_usd,discount_usd) %>%
  filter(!is.na(price_usd), !is.na(discount_usd))%>%
  arrange(desc(price_usd))
# View the result
View(top_10_expensive_products)

#What are the top 10 most cheapest products?
top_10_cheapest_products<-Ajio_data %>%
  select(product_name,price_usd,discount_usd) %>%
  filter(!is.na(price_usd), !is.na(discount_usd))%>%
  arrange(price_usd)
# View the result
View(top_10_cheapest_products)

#Find the top 10 Highest-Brand Name by product type
top_10_highest_brand_name<-Ajio_data%>%
  select(product_type,brand_name,price_usd)%>%
  filter(!is.na(brand_name), !is.na(price_usd))%>%
  arrange(desc(brand_name))%>%
  slice(1:10)
#view the result
View(top_10_highest_brand_name)

#Find the Top 10 lowest-Brand Name by product type
top_10_lowest_brand_name<-Ajio_data%>%
  select(product_type,brand_name,price_usd)%>%
  filter(!is.na(brand_name), !is.na(price_usd))%>%
  arrange(brand_name)%>%
  slice(1:10)
#view the result
View(top_10_lowest_brand_name)

#What is the correlation between price and discount
correlation <- cor(Ajio_data$price_usd, Ajio_data$discount_usd, method 
                   = "pearson")
print(correlation)

#what is the range of price and discount
range_price_usd <- range(Ajio_data$price_usd)
print(paste("Range of Price:", diff(range_price_usd)))
range_discount_usd <- range(Ajio_data$discount_usd)
print(paste("Range of Discount:", diff(range_discount_usd)))

#Analyze the impact of product material on Price of product
product_material_impact<-Ajio_data %>%
  group_by(product_material)%>%
  summarize(avg_price_usd=mean(price_usd,na.rm = TRUE))
# View the result
View(product_material_impact)

#Analyze the impact of shipping time on Price of product
shipping_time_impact<-Ajio_data %>%
  group_by(shipping_time)%>%
  summarize(avg_price_usd=mean(price_usd,na.rm = TRUE))
# View the result
View(shipping_time_impact)
  
#Analyze the impact of product category on Price of product
product_category_impact<-Ajio_data %>%
  group_by(product_category)%>%
  summarize(avg_price_usd=mean(price_usd,na.rm = TRUE))
# View the result
View(product_category_impact)

#univariate Analysis
# Distribution of a numerical variable(eg-discount)
ggplot(Ajio_data, aes(x=discount_usd)) +
  geom_histogram(binwidth=0.5, fill="blue", 
                 color="red") +
  labs(title="Distribution of Discount",
       x="Discount_usd", y="Count")

# Calculate the count of each country's product
country_counts <- Ajio_data %>%
  count(country) %>%
  arrange(desc(n)) %>%
  slice(1:5)  # Keep only the top 5 country 
# View the result
View(country_counts)

# Create the bar plot for the top 5 country
ggplot(country_counts, aes(x = reorder(country, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Top 5 Country", x = "Country", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

#Bivariate Analysis
# Scatter plot between two numerical variables (price and discount)
ggplot(Ajio_data, aes(x=price_usd, y=discount_usd)) +
  geom_point(color="coral") +
  labs(title="Price_usd vs Discount_usd", x="Price_usd", y="Discount_usd")

#Calculate the count and percentage of each product category
product_category_counts <- Ajio_data %>%
  count(product_category) %>%
  mutate(percentage = n / sum(n) * 100)
print(product_category_counts)

# Create a pie chart with percentages
ggplot(product_category_counts, aes(x = "", y = percentage, 
                                    fill = product_category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percentage,0), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Proportion of Product Category") +
  theme_void()

#Faceted plot to analyze multiple subsets
#(e.g.,price vs discount across different product condition)
ggplot(Ajio_data, aes(x = price_usd, y = discount_usd)) +
  geom_point(aes(color = product_condition), alpha =0.7) +
  facet_wrap(~ product_condition) +
  labs(title = "price_usd vs. discount_usd by product_condition", 
  x = "price_usd", y = "discount_usd") +
  theme_minimal()



