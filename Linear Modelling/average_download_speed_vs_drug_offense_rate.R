# Load necessary library
library(tidyverse) 
library(lubridate) 
library(plotly)    

# Load datasets
bristol_broadband = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/broadband/bristol-broadband-speed.csv")
cornwall_broadband = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/broadband/cornwall-broadband-speed.csv")
bristol_crime = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/crime/bristol-crime-rate.csv")
cornwall_crime = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/crime/cornwall-crime-rate.csv")
population_2011 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/population/Population2011_1656567141570.csv")

# Convert 2011 population to 2023 using a growth factor
population_2023 = population_2011 %>% 
  mutate(Population = Population * 1.00561255390388033)

# Convert 2023 population to 2022 using annual growth rate
years_between = 2023 - 2011
annual_growth_rate = 1.00561255390388033 ^ (1 / years_between)
population_2022 = population_2023 %>% 
  mutate(Population = Population / annual_growth_rate)

# Process Bristol crime data: Extract postcode and format Year
bristol_crime_processed = bristol_crime %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))

# Process Cornwall crime data: Extract postcode and format Year
cornwall_crime_processed = cornwall_crime %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))

# Filter and calculate drug offense rate for Bristol (2022)
bristol_drug_offense_rate_2022 = bristol_crime_processed %>% 
  filter(year(Year) == 2022) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Drugs") %>% 
  filter(city == "Bristol, City of") %>% 
  filter(!is.na(Population)) %>% 
  group_by(postcode_space) %>%
  summarise(
    drug_offenses = n(),
    population = first(Population),
    drug_offense_rate = drug_offenses / population * 10000
  )

# Filter and calculate drug offense rate for Cornwall (2022)
cornwall_drug_offense_rate_2022 = cornwall_crime_processed %>% 
  filter(year(Year) == 2022) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Drugs") %>% 
  filter(city == "Cornwall") %>% 
  filter(!is.na(Population)) %>% 
  group_by(postcode_space) %>%
  summarise(
    drug_offenses = n(),
    population = first(Population),
    drug_offense_rate = drug_offenses / population * 10000
  )

# Linear modeling for Bristol: Drug offense rate vs. download speed
bristol_model_data = inner_join(bristol_broadband, bristol_drug_offense_rate_2022, by = "postcode_space") %>% 
  select(`Average download speed (Mbit/s)`, drug_offense_rate)

# Calculate correlation for Bristol
bristol_correlation = bristol_model_data %>% 
  summarise(corCoeff = cor(drug_offense_rate, `Average download speed (Mbit/s)`))

# Fit linear model for Bristol
bristol_model = lm(drug_offense_rate ~ `Average download speed (Mbit/s)`, data = bristol_model_data)
bristol_intercept = coef(bristol_model)[1]
bristol_slope = coef(bristol_model)[2]

# Plot Bristol data using plotly
plotly_bristol = plot_ly(
  data = bristol_model_data,
  x = ~`Average download speed (Mbit/s)`,
  y = ~drug_offense_rate,
  type = 'scatter',
  mode = 'markers',
  marker = list(color = 'blue')
) %>%
  add_lines(
    x = ~`Average download speed (Mbit/s)`,
    y = ~bristol_intercept + bristol_slope * `Average download speed (Mbit/s)`,
    line = list(color = 'red')
  ) %>%
  layout(
    title = "Impact of Internet Speed on Drug Offense Rates in Bristol (2022)",
    xaxis = list(title = "Average Download Speed (Mbit/s)"),
    yaxis = list(title = "Drug Offense Rate per 10,000 People")
  )

# Linear modeling for Cornwall: Drug offense rate vs. download speed
cornwall_model_data = inner_join(cornwall_broadband, cornwall_drug_offense_rate_2022, by = "postcode_space") %>% 
  select(`Average download speed (Mbit/s)`, drug_offense_rate)

# Calculate correlation for Cornwall
cornwall_correlation = cornwall_model_data %>% 
  summarise(corCoeff = cor(drug_offense_rate, `Average download speed (Mbit/s)`))

# Fit linear model for Cornwall
cornwall_model = lm(drug_offense_rate ~ `Average download speed (Mbit/s)`, data = cornwall_model_data)
cornwall_intercept = coef(cornwall_model)[1]
cornwall_slope = coef(cornwall_model)[2]

# Plot Cornwall data using plotly
plotly_cornwall = plot_ly(
  data = cornwall_model_data,
  x = ~`Average download speed (Mbit/s)`,
  y = ~drug_offense_rate,
  type = 'scatter',
  mode = 'markers',
  marker = list(color = 'blue')
) %>%
  add_lines(
    x = ~`Average download speed (Mbit/s)`,
    y = ~cornwall_intercept + cornwall_slope * `Average download speed (Mbit/s)`,
    line = list(color = 'red')
  ) %>%
  layout(
    title = "Impact of Internet Speed on Drug Offense Rates in Cornwall (2022)",
    xaxis = list(title = "Average Download Speed (Mbit/s)"),
    yaxis = list(title = "Drug Offense Rate per 10,000 People")
  )

# Display plots
plotly_bristol
plotly_cornwall
