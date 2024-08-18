# Load necessary libraries
library(tidyverse)

# Load broadband speed performance data
broadbandSpeedPerformance = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/boradband/201805_fixed_pc_performance_r03.csv")

# Data Cleaning: Impute missing values with appropriate statistics and log-transform selected numeric variables
# 1. Missing values are replaced with the median of the column. If a column has more than 90% missing values, replace with 0.
# 2. Log transformation is applied to certain numeric columns to normalize the data distribution.

broadbandSpeedPerformance = broadbandSpeedPerformance %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(is.na(.), 
                         ifelse(sum(is.na(.)) > 0.9 * length(.), 0, median(., na.rm = TRUE)), 
                         .))) %>%
  mutate(across(c(`Average data usage (GB)`,
                  `Median download speed (Mbit/s)`, `Average download speed (Mbit/s)`, 
                  `Minimum download speed (Mbit/s)`, `Maximum download speed (Mbit/s)`,
                  `Median upload speed (Mbit/s)`, `Average upload speed (Mbit/s)`, 
                  `Minimum upload speed (Mbit/s)`, `Maximum upload speed (Mbit/s)`,
                  `Average upload speed (Mbit/s) for lines 10<30Mbit/s`,
                  `Average upload speed (Mbit/s) for SFBB lines`,
                  `Number of connections 5<10 Mbit/s (number of lines)`,
                  `Number of connections 10<30 Mbit/s (number of lines)`,
                  `Number of connections >= 30 Mbit/s (number of lines)`,
                  `Average data usage (GB) for lines < 10Mbit/s`,
                  `Average data usage (GB) for Basic BB lines`,
                  `Average data usage (GB) for SFBB lines`), 
                ~ log(. + 1)))

# Load broadband coverage data
broadbandSpeedCoverage = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/boradband/201809_fixed_pc_coverage_r01.csv") %>%
  rename(postcode_space = pcds, `postcode area` = pca, `Connected Premises` = `All Matched Premises`)

# Filter data for specific postcode areas (BS, TR, PL) and combine performance and coverage data
filtered_data = list(
  BS = broadbandSpeedPerformance %>% filter(`postcode area` == "BS"),
  TR_PL = broadbandSpeedPerformance %>% filter(`postcode area` %in% c("TR", "PL")),
  BS_TR_PL = broadbandSpeedPerformance %>% filter(`postcode area` %in% c("BS", "TR", "PL"))
)

coverage_data = list(
  BS = broadbandSpeedCoverage %>% filter(`postcode area` == "BS"),
  TR_PL = broadbandSpeedCoverage %>% filter(`postcode area` %in% c("TR", "PL")),
  BS_TR_PL = broadbandSpeedCoverage %>% filter(`postcode area` %in% c("BS", "TR", "PL"))
)

# Join performance and coverage data for each region, retaining only necessary columns
cleaned_data = map2(filtered_data, coverage_data, ~ 
  inner_join(.x, .y, by = c("postcode", "postcode_space", "postcode area")) %>%
    select(postcode, postcode_space, `postcode area`, 
           `Average download speed (Mbit/s)`, `Maximum download speed (Mbit/s)`, 
           `Minimum download speed (Mbit/s)`, `Average upload speed (Mbit/s)`, 
           `Maximum upload speed (Mbit/s)`, `Minimum upload speed (Mbit/s)`,
           `Average data usage (GB)`, `All Premises`, `Connected Premises`))

# Load and join postcode to LSOA data for further geographical analysis
pscdToLsoa = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/population/Postcode to LSOA.csv") %>%
  rename(postcode_space = pcds, lsoa_area = lsoa11nm, city = ladnm) %>%
  select(postcode_space, lsoa_area, city)

cleaned_data = map(cleaned_data, ~ left_join(.x, pscdToLsoa, by = "postcode_space"))

# Save cleaned datasets to CSV files
write.csv(cleaned_data$BS, "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/broadband/bristol-broadband-speed.csv", row.names = FALSE)
write.csv(cleaned_data$TR_PL, "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/broadband/cornwall-broadband-speed.csv", row.names = FALSE)
write.csv(cleaned_data$BS_TR_PL, "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/broadband/bristol-cornwall-broadband-speed.csv", row.names = FALSE)

