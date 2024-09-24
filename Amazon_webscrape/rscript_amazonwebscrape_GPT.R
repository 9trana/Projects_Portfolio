library(rvest)
library(dplyr)
library(ggplot2)

options(scipen = 999)

# Function to extract the necessary information from a single item
extract_item_info <- function(item) {
  # Try to extract the item name, reviews, price, and rating with NA as default
  item_name <- item %>%
    html_node('.a-size-base-plus.a-color-base.a-text-normal') %>%
    html_text(trim = TRUE) %>%
    tryCatch(., error = function(e) NA)
  
  reviews <- item %>%
    html_node('.a-size-base.s-underline-text') %>%
    html_text(trim = TRUE) %>%
    gsub("[^0-9]", "", .) %>%
    as.numeric() %>%
    tryCatch(., error = function(e) NA)
  
  price <- item %>%
    html_node('.a-price-whole') %>%
    html_text(trim = TRUE) %>%
    paste0(., item %>%
             html_node('.a-price-fraction') %>%
             html_text(trim = TRUE)) %>%
    tryCatch(., error = function(e) NA)
  
  rating <- item %>%
    html_node('.a-icon-alt') %>%
    html_text(trim = TRUE) %>%
    gsub("[^0-9.]", "", .) %>%
    as.numeric() %>%
    tryCatch(., error = function(e) NA)
  
  # Return a list of the extracted information
  return(c(Item_Name = item_name, Reviews = reviews, Price = price, Ratings = rating))
}

# Initialize an empty data frame to store results
finaldata <- data.frame()

# Loop through pages
for (n in 1:15) {
  url <- paste0("https://www.amazon.ca/s?k=true+wireless+earbuds&page=", n)
  webpage <- read_html(url)
  
  # Extract all items
  items <- webpage %>% html_nodes('.s-result-item')
  
  # Apply the extraction function to each item and combine into a data frame
  result <- do.call(rbind, lapply(items, extract_item_info))
  
  # Convert result into a clean data frame and append to finaldata
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  finaldata <- bind_rows(finaldata, result)
}

# Clean the final data frame
finaldata_clean <- finaldata %>%
  filter(!is.na(Price), !is.na(Ratings), !is.na(Reviews)) %>%
  mutate(Price = as.numeric(gsub(",", "", Price))) %>%
  filter(Price != 'NANA')

finaldata_clean$Ratings<-as.numeric(finaldata_clean$Ratings)
finaldata_clean$Reviews<-as.numeric(finaldata_clean$Reviews)


# Reshape the data for faceting
long_data <- finaldata_clean %>%
  pivot_longer(cols = c(Price, Reviews, Ratings), 
               names_to = "variable", 
               values_to = "value")

# Create the ggplot (uncleaned)
ggplot(long_data, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +  # Separate panels for each variable
  labs(x = "Value", y = "Frequency", 
       title = "Histograms for Price, Reviews, and Average Rating of True Wireless Headphones") +
  theme_minimal()

fd<-data.table(finaldata_clean)

fd<-fd[Reviews<90000&Ratings>=3] #drop to 25000, mean = 2400, max = 19209

fd2<-fd[!duplicated(fd, by = c("Item_Name"))]

# Reshape the data for faceting
long_data <- fdx %>%
  pivot_longer(cols = c(Price, Reviews, Ratings), 
               names_to = "variable", 
               values_to = "value")

# Create the ggplot (cleaned)
ggplot(long_data, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +  # Separate panels for each variable
  labs(x = "Value", y = "Frequency", 
       title = "Histograms for Price, Reviews, and Average Rating of True Wireless Headphones") +
  theme_minimal()

#get descriptives
# Step 2: Custom function to calculate the mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Step 3: Use lapply to compute mean, median, and mode for each column
result <- fdx[, lapply(.SD, function(column) {
  list(
    Mean = mean(column, na.rm = TRUE),
    Median = median(column, na.rm = TRUE),
    Mode = get_mode(column)
  )
}), .SDcols = 2:ncol(fd2)]

# Step 4: Unnest the list columns (optional, for better readability)
result_clean <- result[, lapply(.SD, unlist)]

# Print the results
print(result_clean)

buy<-fdx[Ratings>4.15 & Reviews>336]
  
