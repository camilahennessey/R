#' A1 Retail Stores EDA 
#' Purpose: submission for a1
#' Camila Hennessey
#' Jan 25, 2024
#' 

#lib
library(ggplot2)
library(data.table)
library(dplyr)
library(lubridate)
library(knitr)
library(corrplot)
library(scales)

#wd
setwd("~/Google Drive/MBAN/Visualizing and Analyzing with R/Visualizing and Analyzing with R/personalFiles")

#load the data

data <- read.csv("~/Google Drive/MBAN/Visualizing and Analyzing with R/Visualizing and Analyzing with R/personalFiles/EDA Sampled Data.csv")

print(head(data))
print(summary(data))
print(glimpse(data))

# Check for Missing Values
print(sum(is.na(data)))

# Rename each column to simpler names
colnames(data) <- c("InvoiceNumber", "SaleDate", "StoreNumber", "StoreName", "Address",
                    "City", "ZipCode", "County", "CategoryName", "VendorName",
                    "ItemDescription", "Pack", "BottleVolume_ml", "StateBottleCost",
                    "StateBottleRetail", "BottlesSold", "Sale")
colnames(data)


# Ensuring all columns are named
data <- as_tibble(data, .name_repair = "unique")

# Visualize Distributions
# (Use the previously provided histogram code here)

# Convert Date to Date-Time Object for Time Series Analysis
data$DateTime <- ymd(data$SaleDate)

# Summarize Sales Over Time
time_series_sales <- data %>% group_by(DateTime) %>% summarize(TotalSales = sum(Sale))

# Time Series Decomposition
ts_data <- ts(time_series_sales$TotalSales, frequency = 12) # Adjust frequency based on your data
decomposed_ts <- decompose(ts_data)
plot(decomposed_ts)

# Time Series Decomposition
ts_data <- ts(time_series_sales$TotalSales, frequency = 12) # Adjust frequency based on your data
decomposed_ts <- decompose(ts_data)

# Plotting each component separately
par(mfrow = c(3, 1)) # Set up the plotting area to have 3 rows and 1 column

# Plot the Trend Component
plot(decomposed_ts$trend, main = "Trend Component", xlab = "Time", ylab = "Sales")
# Plot the Seasonal Component
plot(decomposed_ts$seasonal, main = "Seasonal Component", xlab = "Time", ylab = "Sales")
# Plot the Random Component
plot(decomposed_ts$random, main = "Random/Irregular Component", xlab = "Time", ylab = "Sales")

# Reset the plotting area to default (optional)
par(mfrow = c(1, 1))

# Summarize sales over time
time_series_sales <- data %>% group_by(DateTime) %>% summarize(TotalSales = sum(Sale))

# Plotting the time series
ggplot(time_series_sales, aes(x = DateTime, y = TotalSales)) +
  geom_line() +
  ggtitle("Time Series Analysis of Sales") +
  xlab("Date") +
  ylab("Total Sales") +
  theme_minimal()

# Summarize Total Liters by Vendor
vendor_liters <- data %>%
  group_by(VendorName) %>%
  summarize(TotalLiters = sum(BottlesSold * BottleVolume_ml) / 1000) %>%
  arrange(desc(TotalLiters))

# Vendor Comparison with improved axis
top_vendors <- head(vendor_liters, 5) # Adjust number of top vendors as needed

# Vendor Comparison
top_vendors <- head(vendor_liters, 5) # Adjust number of top vendors as needed

ggplot(top_vendors, aes(x = reorder(VendorName, TotalLiters), y = TotalLiters, fill = VendorName)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top Vendors by Total Liters",
    x = "Vendor Name",  # Adjust the x-axis label
    y = "Total Liters"  # Adjust the y-axis label
  ) +
  theme_bw() +  # Set a white background
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  
  )


# Consistency Checks
# Example: Check if sales value is consistent with the number of bottles sold and bottle retail price
data$CalculatedSales <- data$BottlesSold * data$StateBottleRetail


# Consistency Checks
# Example: Check if sales value is consistent with the number of bottles sold and bottle retail price
data$CalculatedSales <- data$BottlesSold * data$StateBottleRetail
inconsistent_sales <- sum(data$Sale != data$CalculatedSales)
print(paste("Number of Inconsistent Sales Records:", inconsistent_sales))

# Calculate correlation between Bottle Retail Price and Sales
correlation <- cor(data$StateBottleRetail, data$Sale)

# Create a summary table
summary_table <- data.frame(
  Variable = c("Bottle Retail Price", "Sales", "Correlation"),
  Value = c(mean(data$StateBottleRetail), mean(data$Sale), correlation)
)

# Print the summary table
print(summary_table)

# Calculate correlation between Bottle Retail Price and Sales
correlation <- cor(data$StateBottleRetail, data$Sale)

# Create a summary table
summary_table <- data.frame(
  Variable = c("Bottle Retail Price", "Sales", "Correlation"),
  Value = c(mean(data$StateBottleRetail), mean(data$Sale), correlation)
)

# Selecting numerical variables for correlation analysis
numerical_data <- data %>% select(StateBottleCost, StateBottleRetail, BottlesSold, Sale)

# Creating the correlation matrix
correlation_matrix <- cor(numerical_data)

# Plotting the correlation matrix
corrplot(correlation_matrix, method = "circle")

colnames(data)

# Convert SaleDate to Date format if it's in character format
data$SaleDate <- as.Date(data$SaleDate)

# Create additional time variables
data$Weekday <- weekdays(data$SaleDate)
data$Month <- months(data$SaleDate)
data$Season <- ifelse(month(data$SaleDate) %in% c(12, 1, 2), "Winter",
                      ifelse(month(data$SaleDate) %in% c(3, 4, 5), "Spring",
                             ifelse(month(data$SaleDate) %in% c(6, 7, 8), "Summer",
                                    "Fall")))
data$IsWeekend <- ifelse(data$Weekday %in% c("Saturday", "Sunday"), TRUE, FALSE)

colnames(data)

# Filter the data to include only "DIAGEO" transactions
# Filter the data to include only "DIAGEO" transactions
diageo_data <- data %>%
  filter(VendorName == "DIAGEO")


# Summarize sales by different time periods
monthly_sales <- data %>% group_by(Month) %>% summarize(TotalSales = sum(Sale))
weekly_sales <- data %>% group_by(Weekday) %>% summarize(TotalSales = sum(Sale))
seasonal_sales <- data %>% group_by(Season) %>% summarize(TotalSales = sum(Sale))

# Plotting the trends

# Monthly Sales Trend (Bar Plot) with Further Axis Improvements
ggplot(monthly_sales, aes(x = Month, y = TotalSales)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  ggtitle("Monthly Sales Trends") +
  xlab("Month") +  # Improve x-axis label
  ylab("Total Sales (in Dollars)") +  # Improve y-axis label
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  # Rotate and adjust x-axis labels

# Weekly Sales Trend (Bar Plot) with Enhanced Axis and Aesthetics
ggplot(weekly_sales, aes(x = factor(Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y = TotalSales)) + 
  geom_bar(stat = "identity", fill = "#0072B2") +  # Adjust fill color
  ggtitle("Weekly Sales Trends") +
  xlab("Day of the Week") +  # Improve x-axis label
  ylab("Total Sales (in Dollars)") +  # Improve y-axis label
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +  # Rotate and adjust x-axis labels
  theme(axis.title.x = element_blank(),  # Remove x-axis title
        plot.title = element_text(hjust = 0.5))  # Center plot title

# Seasonal Sales Trend with Improved Axis and Aesthetics (Black Border Removed)
ggplot(seasonal_sales, aes(x = Season, y = TotalSales, fill = Season)) + 
  geom_bar(stat = "identity") +  # Remove black border
  ggtitle("Seasonal Sales Trends") +
  xlab("Season") +  # Improve x-axis label
  ylab("Total Sales (in Dollars)") +  # Improve y-axis label
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for seasons
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +  # Horizontal x-axis labels
  theme(axis.title.x = element_blank(),  # Remove x-axis title
        plot.title = element_text(hjust = 0.5))  # Center plot title

# Aggregate sales by store
store_sales <- data %>%
  group_by(StoreName, City, County) %>%
  summarize(TotalSales = sum(Sale), .groups = 'drop') %>%
  arrange(desc(TotalSales))

# Identify top stores - let's say top 10 for this analysis
top_stores <- head(store_sales, 10)

# Seasonal Sales Trend with Further Axis Improvements
ggplot(seasonal_sales, aes(x = Season, y = TotalSales)) + 
  geom_bar(stat = "identity", fill = "#0072B2") +  # Adjust fill color
  ggtitle("Seasonal Sales Trends") +
  xlab("Season") +  # Improve x-axis label
  ylab("Total Sales (in Dollars)") +  # Improve y-axis label
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1)) +  # Horizontal x-axis labels
  theme(axis.title.x = element_blank(),  # Remove x-axis title
        plot.title = element_text(hjust = 0.5))  # Center plot title

# Visualization 2: Distribution of Top Stores across Cities
city_distribution <- top_stores %>%
  group_by(City) %>%
  summarize(NumberOfStores = n(), .groups = 'drop')

ggplot(city_distribution, aes(x = reorder(City, NumberOfStores), y = NumberOfStores, fill = City)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Distribution of Top Stores Across Cities") +
  xlab("City") +
  ylab("Number of Top Stores") +
  theme_minimal()

# Aggregate sales by liquor category
category_sales <- data %>%
  group_by(CategoryName) %>%
  summarize(TotalSales = sum(Sale), .groups = 'drop') %>%
  arrange(desc(TotalSales))

# Identify top categories - let's say top 10 for this analysis
top_categories <- head(category_sales, 10)

# Visualization 1: Total Sales by Top Liquor Categories (Bar Chart)
ggplot(top_categories, aes(x = reorder(CategoryName, TotalSales), y = TotalSales, fill = CategoryName)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Total Sales by Top Liquor Categories") +
  xlab("Category Name") +
  ylab("Total Sales") +
  theme_minimal()

# Bar Chart for Sales Composition of Top Liquor Categories Over Time
ggplot(top_categories, aes(x = CategoryName, y = TotalSales, fill = CategoryName)) +
  geom_bar(stat = "identity") +
  ggtitle("Sales Composition of Top Liquor Categories Over Time") +
  xlab("Category Name") +
  ylab("Total Sales") +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# 1. Preferences Based on Bottle Volume
# Categorizing bottle volumes (example categories: Small, Medium, Large)
data$VolumeCategory <- cut(data$BottleVolume_ml, breaks = c(0, 500, 1000, Inf), labels = c("Small", "Medium", "Large"), right = FALSE)

# Summarize sales by volume category
volume_sales <- data %>%
  group_by(VolumeCategory) %>%
  summarize(TotalSales = sum(Sale), .groups = 'drop')

# Visualization of sales by bottle volume
ggplot(volume_sales, aes(x = VolumeCategory, y = TotalSales, fill = VolumeCategory)) +
  geom_bar(stat = "identity") +
  ggtitle("Sales by Bottle Volume Category") +
  xlab("Bottle Volume Category") +
  ylab("Total Sales") +
  theme_minimal()

# 2. Preferences Based on Price Segments
# Categorizing prices into segments (example segments: Low, Medium, High)
data$PriceSegment <- cut(data$StateBottleRetail, breaks = c(0, 10, 20, Inf), labels = c("Low", "Medium", "High"), right = FALSE)

# Summarize sales by price segment
price_sales <- data %>%
  group_by(PriceSegment) %>%
  summarize(TotalSales = sum(Sale), .groups = 'drop')

# Visualization of sales by price segment
ggplot(price_sales, aes(x = PriceSegment, y = TotalSales, fill = PriceSegment)) +
  geom_bar(stat = "identity") +
  ggtitle("Sales by Price Segment") +
  xlab("Price Segment") +
  ylab("Total Sales") +
  theme_minimal()

# You can adjust these ranges based on your specific dataset
data$VolumeCategory <- cut(data$BottleVolume_ml, 
                           breaks = c(0, 250, 500, 750, 1000, Inf), 
                           labels = c("Very Small", "Small", "Medium", "Large", "Very Large"), 
                           right = FALSE)

# Summarize sales by bottle volume category
volume_sales <- data %>%
  group_by(VolumeCategory) %>%
  summarize(TotalSales = sum(Sale), .groups = 'drop') %>%
  arrange(desc(TotalSales))

# Visualization of sales by bottle volume with improved axis
ggplot(volume_sales, aes(x = reorder(VolumeCategory, TotalSales), y = TotalSales, fill = VolumeCategory)) +
  geom_bar(stat = "identity") +
  ggtitle("Consumer Preferences by Bottle Volume") +
  xlab("Bottle Volume Category") +
  ylab("Total Sales") +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +  # Format y-axis labels with commas and remove extra space
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.title.y = element_text(margin = margin(r = 10)))  # Adjust y-axis title margin

# Group data by Vendor and summarize total liters
vendor_liters <- data %>%
  group_by(VendorName) %>%
  summarize(TotalLiters = sum(BottlesSold * BottleVolume_ml) / 1000)  # Convert liters to thousands

# Load the required libraries
library(knitr)
library(dplyr)

# Group data by Vendor and summarize total liters
vendor_liters <- data %>%
  group_by(VendorName) %>%
  summarize(TotalLiters = sum(BottlesSold * BottleVolume_ml) / 1000)  # Convert liters to thousands

# Sort the summarized data in descending order of TotalLiters
vendor_liters <- vendor_liters %>%
  arrange(desc(TotalLiters))

# Print the summarized data as a table
kable(vendor_liters, caption = "Total Liters by Vendor (Descending Order)")

# Group data by Vendor and summarize total liters
vendor_liters <- data %>%
  group_by(VendorName) %>%
  summarize(TotalLiters = sum(BottlesSold * BottleVolume_ml) / 1000)  # Convert liters to thousands

# Sort the summarized data in descending order of TotalLiters
vendor_liters <- vendor_liters %>%
  arrange(desc(TotalLiters))

# Print the first 10 entries of the sorted data as a table
kable(head(vendor_liters, 10), caption = "Top 10 Vendors by Total Liters (Descending Order)")


# Group data by Vendor and summarize total liters
vendor_liters <- data %>%
  group_by(VendorName) %>%
  summarize(TotalLiters = sum(BottlesSold * BottleVolume_ml) / 1000)  # Convert liters to thousands

# Sort the summarized data in descending order of TotalLiters
vendor_liters <- vendor_liters %>%
  arrange(desc(TotalLiters))

# Select the top 10 vendors
top_10_vendors <- head(vendor_liters, 10)

# Create a bar chart to visualize the top 10 vendors by total liters
ggplot(top_10_vendors, aes(x = reorder(VendorName, TotalLiters), y = TotalLiters, fill = VendorName)) +
  geom_bar(stat = "identity") +
  ggtitle("Top 10 Vendors by Total Liters (Descending Order)") +
  xlab("Vendor Name") +
  ylab("Total Liters (in Thousands)") +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Group data by City and summarize total sales
city_sales <- data %>%
  group_by(City) %>%
  summarize(TotalSales = sum(Sale))
# Sort the dataset by TotalSales in descending order
sorted_city_sales <- city_sales %>%
  arrange(desc(TotalSales))

# Print the top 10 cities based on total sales
print(head(sorted_city_sales, 10))









