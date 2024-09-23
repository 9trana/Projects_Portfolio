#Small project that scrape Amazon page for total number of reviews, item name, and price

library(rvest)
library(dplyr)
library(ggplot2)

# URL of the Amazon search results page (adjust as needed), keyword, page numbers
finaldata<-data.frame()
for(n in 1:5){
url <- paste0("https://www.amazon.ca/s?k=true+wireless+earbuds&page=",n)

# Read the page
webpage <- read_html(url)

# Extract all parent elements that contain reviews, price, item name, and average rating
items <- webpage %>%
  html_nodes('.s-result-item')  # Adjust this selector to the correct parent container

# Initialize vectors for item names and reviews
item_names <- character(length(items))
reviews <- numeric(length(items))
prices<-character(length(items))
ratings <- numeric(length(items))

# Loop through each item and extract the name, reviews, price, and average rating
for (i in seq_along(items)) {
  item_names[i] <- items[i] %>%
    html_node('.a-size-base-plus.a-color-base.a-text-normal') %>%
    html_text(trim = TRUE)
  
  review_node <- items[i] %>%
    html_node('.a-size-base.s-underline-text')
  
  # Extract price
  price_whole <- items[i] %>%
    html_node('.a-price-whole') %>%
    html_text(trim = TRUE)
  
  price_fraction <- items[i] %>%
    html_node('.a-price-fraction') %>%
    html_text(trim = TRUE)
  
  # Concatenate whole and fraction for the price
  prices[i] <- paste0(price_whole, price_fraction)
  
  #Ensures equal length of review and price (some items have no reviews, sub with NA)
  if (is.null(review_node)) {
    reviews[i] <- NA  # Assign NA if no review element exists
  } else {
    reviews[i] <- review_node %>%
      html_text(trim = TRUE) %>%
      gsub("[^0-9]", "", .) %>%
      as.numeric()
  }
  
  #also subs in NA for average rating (if no reviews)
  rating_node <- items[i] %>%
    html_node('.a-icon-alt')  # Adjust this selector to match the rating element
  
  if (is.null(rating_node)) {
    ratings[i] <- NA  # Assign NA if no rating element exists
  } else {
    ratings[i] <- rating_node %>%
      html_text(trim = TRUE) %>%
      gsub("[^0-9.]", "", .) %>%  # Keep only numeric characters and dots
      as.numeric()
  }
}

  
# Create a data frame from our variables

result <- data.frame(
  Item_Name = item_names,
  Reviews = reviews,
  Price = prices,
  Ratings = ratings,
  stringsAsFactors = FALSE
)

#drop the review NAs (no items have missing reviews)
result_clean <- result %>%
  filter(!is.na(result$Ratings) & !is.na(Price) & !is.na(Reviews))

# Also drop items that currently have no price (unavailable for purchase for some reason)
result_cleaner<-subset(result_clean, Price!='NANA')
finaldata<-rbind(finaldata,result_cleaner)
}

#convert prices to numeric
finaldata$Price<-as.numeric(finaldata$Price)

# Step 1: Reshape the data to long format for faceting
long_data <- finaldata %>%
  pivot_longer(cols = c(Price, Reviews, Ratings),
               names_to = "variable",
               values_to = "value")

# Step 2: Create the ggplot
ggplot(long_data, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +  # Separate panels for each variable
  labs(x = "Value", y = "Frequency", title = "Histograms for Price, Reviews, and Average Rating of True Wireless Headphones") +
  theme_minimal()
