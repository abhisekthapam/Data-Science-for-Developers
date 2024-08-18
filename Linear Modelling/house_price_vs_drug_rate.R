# Load necessary libraries
library(tidyverse) 
library(lubridate)
library(plotly)     

# Load datasets from specified file paths
file_paths = list(
  bristol_house = "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/house/bristol-house-pricing.csv",
  cornwall_house = "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/house/cornwall-house-pricing.csv",
  bristol_crime = "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/crime/bristol-crime-rate.csv",
  cornwall_crime = "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/crime/cornwall-crime-rate.csv",
  population = "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/population/Population2011_1656567141570.csv"
)

# Read the data from the file paths into a list
data = lapply(file_paths, read_csv)

# Assign the data from the list to individual variables
bristol_house = data$bristol_house
cornwall_house = data$cornwall_house
bristol_crime = data$bristol_crime
cornwall_crime = data$cornwall_crime
population_2011 = data$population

# Calculate the annual growth rate for population projection
annual_growth_rate = 1.00561255390388033 ^ (1 / (2023 - 2011))

# Project population from 2011 to 2022 using the annual growth rate
population_2022 = population_2011 %>%
  mutate(Population = Population * 1.00561255390388033 / annual_growth_rate)

# Function to preprocess crime data by extracting postcodes and formatting year
preprocess_crime_data = function(crime_data) {
  crime_data %>%
    mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d"),
           Year = ymd(paste0(Year, "-01")))
}

# Preprocess Bristol and Cornwall crime data
bristol_crime = preprocess_crime_data(bristol_crime)
cornwall_crime = preprocess_crime_data(cornwall_crime)

# Function to calculate the drug offense rate per 10,000 population
calculate_drug_offense_rate = function(crime_data, population_data, city_name) {
  crime_data %>%
    filter(year(Year) == 2022,                  
           `Crime type` == "Drugs",              
           city == city_name) %>%               
    left_join(population_data, by = "Postcode") %>% 
    filter(!is.na(Population)) %>%               
    group_by(postcode_space) %>%                 
    summarise(drug_offenses = n(),              
              population = first(Population),    
              drug_offense_rate = drug_offenses / population * 10000)  
}

# Calculate drug offense rates for Bristol and Cornwall in 2022
bs_drug_rate = calculate_drug_offense_rate(bristol_crime, population_2022, "Bristol, City of")
cw_drug_rate = calculate_drug_offense_rate(cornwall_crime, population_2022, "Cornwall")

# Function to create a linear model and generate an interactive Plotly plot
create_model_and_plot = function(house_data, drug_rate_data, title) {
  house_data_2022 = house_data %>% filter(year(Date_of_transfer) == 2022)
  merged_data = inner_join(house_data_2022, drug_rate_data, by = c("Postcode" = "postcode_space")) %>%
    select(Price, drug_offense_rate)  
  cor_coeff = merged_data %>% summarise(corCoeff = cor(Price, drug_offense_rate)) %>% pull(corCoeff)
  model = lm(Price ~ drug_offense_rate, data = merged_data)
  intercept = coef(model)[1]
  slope = coef(model)[2]
  
  plot_ly(merged_data, x = ~drug_offense_rate, y = ~Price, type = 'scatter', mode = 'markers', 
          marker = list(color = 'blue', size = 6)) %>%
    add_lines(x = ~drug_offense_rate, y = ~intercept + slope * drug_offense_rate, 
              line = list(color = 'red')) %>%
    layout(title = paste0(title, "\nCorrelation Coefficient: ", round(cor_coeff, 4)),
           xaxis = list(title = "Drug Offense Rate"),
           yaxis = list(title = "House Price"),
           hovermode = "closest")  # Enable hover for data points
}
plot_bristol = create_model_and_plot(bristol_house, bs_drug_rate, "Impact of Drug Offense on House Price in Bristol (2022)")
plot_cornwall = create_model_and_plot(cornwall_house, cw_drug_rate, "Impact of Drug Offense on House Price in Cornwall (2022)")

# Display the plots
plot_bristol
plot_cornwall
