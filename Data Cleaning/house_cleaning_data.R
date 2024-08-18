# Load essential libraries for data manipulation and date handling
library(dplyr)       # For data manipulation
library(readr)       # For reading CSV files
library(lubridate)   # For date handling

# Define a function to clean the dataset
clean_dataset <- function(file_path) {
  data <- read_csv(file_path, col_names = FALSE) %>%
    set_names(c(
      "Txn_ID", "Price", "Date_of_transfer", "Postcode", "Property_type", 
      "Old/New", "Duration", "PAON", "SAON", "Street", "Locality", "Town/City", 
      "District", "County", "PPD_type", "Record_status"
    )) %>%
    mutate(
      # Convert Txn_ID to character to maintain its integrity
      Txn_ID = as.character(Txn_ID),
      # Convert Price to numeric to perform numerical operations
      Price = as.numeric(Price),
      # Convert Date_of_transfer to Date format for proper date manipulation
      Date_of_transfer = as.Date(Date_of_transfer, format = "%Y-%m-%d %H:%M"),
      # Handle missing values by replacing them with descriptive placeholders
      Postcode = ifelse(is.na(Postcode), "Unknown", Postcode),
      SAON = ifelse(is.na(SAON), "Not Applicable", SAON),
      Street = ifelse(is.na(Street), "Unknown", Street),
      Locality = ifelse(is.na(Locality), "Not Applicable", Locality)
    ) %>%
    distinct()  # Remove duplicate rows to ensure dataset uniqueness
  
  return(data)  # Return the cleaned dataset
}

# Define file paths for the datasets to be cleaned
file_paths <- c(
  "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/house/pp-House-2020.csv",
  "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/house/pp-House-2021.csv",
  "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/house/pp-House-2022.csv",
  "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/house/pp-House-2023.csv"
)

# Clean each dataset and store them in a list for further processing
cleaned_data_list <- lapply(file_paths, clean_dataset)

# Combine all years' cleaned data into a single dataset for comprehensive analysis
all_year_house_data_cleaned <- bind_rows(cleaned_data_list)

# Define file paths for saving the cleaned datasets and results
output_file_paths_year <- paste0(
  "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/house/house_data_cleaned_",
  2020:2023, ".csv"
)
output_file_path_combined <- "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/house/abhisek_house_data_cleaned.csv"
output_file_path_bristol <- "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/house/bristol-house-pricing.csv"
output_file_path_cornwall <- "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/house/cornwall-house-pricing.csv"
output_file_path_avg_2022 <- "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/house/average_house_price_2022.csv"
output_file_path_avg_2020_2023 <- "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/house/average_house_price_2020_2023.csv"

# Filter data for specific counties: Bristol and Cornwall
bristol_data <- all_year_house_data_cleaned %>% filter(County == "CITY OF BRISTOL")
cornwall_data <- all_year_house_data_cleaned %>% filter(County == "CORNWALL")

# Calculate average house prices for the year 2022 for Bristol and Cornwall
average_price_2022_bristol <- bristol_data %>%
  filter(format(Date_of_transfer, "%Y") == "2022") %>%
  summarise(Average_Price = mean(Price, na.rm = TRUE))

average_price_2022_cornwall <- cornwall_data %>%
  filter(format(Date_of_transfer, "%Y") == "2022") %>%
  summarise(Average_Price = mean(Price, na.rm = TRUE))

# Combine the average prices for 2022 into a single data frame
average_price_2022 <- data.frame(
  County = c("CITY OF BRISTOL", "CORNWALL"),
  Average_Price = c(average_price_2022_bristol$Average_Price, average_price_2022_cornwall$Average_Price)
)

# Save the average price data for 2022
write.csv(average_price_2022, output_file_path_avg_2022, row.names = FALSE)

# Prepare data for line chart: Calculate average house prices by year for both counties
line_chart_data <- all_year_house_data_cleaned %>%
  filter(County %in% c("CITY OF BRISTOL", "CORNWALL")) %>%
  mutate(Year = year(Date_of_transfer)) %>%
  group_by(County, Year) %>%
  summarise(Average_Price = mean(Price, na.rm = TRUE)) %>%
  ungroup()

# Save the data for the line chart
write.csv(line_chart_data, output_file_path_avg_2020_2023, row.names = FALSE)

# Save the cleaned datasets
for (i in seq_along(file_paths)) {
  year <- gsub(".*pp-House-(\\d{4})\\.csv", "\\1", file_paths[i])
  write.csv(cleaned_data_list[[i]], output_file_paths_year[i], row.names = FALSE)
}
write.csv(all_year_house_data_cleaned, output_file_path_combined, row.names = FALSE)
write.csv(bristol_data, output_file_path_bristol, row.names = FALSE)
write.csv(cornwall_data, output_file_path_cornwall, row.names = FALSE)
