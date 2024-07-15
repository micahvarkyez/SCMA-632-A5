# Set the working directory and verify it
setwd('C:\\Users\\Dell\\Desktop\\A5')
getwd()
# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("NSSO_68.csv")

# Filtering for WB
df <- data %>%
  filter(state_1 == "WB")

# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Subsetting the data
wbnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
wbnew$Meals_At_Home <- impute_with_mean(wbnew$Meals_At_Home)

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}
print(df)

outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  wbnew <- remove_outliers(wbnew, col)
}

# Summarize consumption
wbnew$total_consumption <- rowSums(wbnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- wbnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors
district_mapping <- c("1" = "Darjiling", "2" = "Jalpaiguri", "3" = "Koch Bihar", "4" = "Uttar Dinajpur", "5" = "Dakshin Dinajpur *", "6" = "Maldah", "7" = "Murshidabad", "8" = "Birbhum", "9" = "Barddhaman", "10" = "Nadia", "11" = "North Twenty Four Parganas", "12" = "Hugli", "13" = "Bankura", "14" = "Puruliya", "15" = "Pashim Midnapur", "16" = "Haora","17" = "Kolkata", "18" = "South  Twenty Four Parganas", "19" = "Purba Midnapur")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

wbnew$District <- as.character(wbnew$District)
wbnew$Sector <- as.character(wbnew$Sector)
wbnew$District <- ifelse(wbnew$District %in% names(district_mapping), district_mapping[wbnew$District], wbnew$District)
wbnew$Sector <- ifelse(wbnew$Sector %in% names(sector_mapping), sector_mapping[wbnew$Sector], wbnew$Sector)

View(wbnew)

hist(wbnew$total_consumption, breaks = 10, col = 'blue', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in WestBengal State")

WB_consumption <- aggregate(total_consumption ~ District, data = wbnew, sum) 
View(WB_consumption)

barplot(WB_consumption$total_consumption, 
        names.arg = WB_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'blue', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7) # Adjust the size of district names if needed

library(ggplot2) 
library(sf) # mapping
library(dplyr) 
Sys.setenv("SHAPE_RESTORE_SHX" = "YES") 

data_map <- st_read("C:\\Users\\Dell\\Desktop\\A5\\WEST BENGAL_DISTRICTS.geojson") 
View(data_map)

data_map <- data_map %>% 
  rename(District = dtname) 
colnames(data_map) 
data_map_data <- merge(WB_consumption,data_map,by = "District") 
View(data_map_data)
ggplot(data_map_data) + 
  geom_sf(aes(fill =total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption_by_District") 

ggplot(data_map_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption by District") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")
