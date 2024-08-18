# Load necessary libraries for data manipulation, visualization, and analysis
library(tidyverse)
library(plotly)
library(lubridate)
library(fmsb) # For creating radar charts

# Load cleaned datasets for crime data in Bristol and Cornwall
bristol_crime_summary = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/crime/bristol-crime-summary.csv")
cornwall_crime_summary = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/crime/cornwall-crime-summary.csv")
bristol_crime_rate = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/crime/bristol-crime-rate.csv")
cornwall_crime_rate = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/crime/cornwall-crime-rate.csv")

# Load and examine the population dataset from 2011
population_2011 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/population/Population2011_1656567141570.csv")
dim(population_2011) 
colSums(is.na(population_2011))

# Project the 2011 population to 2023 by applying an annual growth rate
population_2023 = population_2011 %>% 
  mutate(Population = Population * 1.00561255390388033)

# Calculate the population for 2022 by reversing the annual growth projection
years_diff = 2023 - 2011
annual_growth = 1.00561255390388033 ^ (1 / years_diff)
population_2022 = population_2023 %>% 
  mutate(Population = Population / annual_growth)
dim(population_2022) # Verify dimensions of the adjusted 2022 population dataset

# Convert 'Year' column to Date format for both Bristol and Cornwall crime rate datasets
bristol_crime_rate = bristol_crime_rate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))
str(bristol_crime_rate) # Inspect the structure of the Bristol crime rate dataset

cornwall_crime_rate = cornwall_crime_rate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))

# Combine crime rate datasets from Bristol and Cornwall
bc_crime_rate = bind_rows(bristol_crime_rate, cornwall_crime_rate)

# Analyze drug offence rates for 2022
drug_offence_rate = bc_crime_rate %>% 
  filter(year(Year) == 2022) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Drugs") %>% 
  filter(city == "Bristol, City of" | city == "Cornwall") 
colSums(is.na(drug_offence_rate))

# Refine the dataset by removing rows with NA population values
drug_offence_rate = drug_offence_rate %>% 
  filter(!is.na(Population))

# Create and visualize a box plot for drug offence rates in Bristol and Cornwall
drug_offence_rate_box_plot = drug_offence_rate %>% 
  group_by(Postcode, city) %>%
  summarise(drug_offenses = n(),
            population = first(Population)) %>%
  mutate(drug_offense_rate = drug_offenses / population * 10000) %>%
  ggplot(aes(x = city, y = drug_offense_rate)) +
  geom_boxplot(color = "darkblue", fill = "lightblue") +
  labs(title = "Distribution of Drug Offence Rates in Bristol and Cornwall (2022)",
       x = "County",
       y = "Drug Offence Rate per 10,000 people") +
  coord_cartesian(ylim = c(0, 50)) +
  theme_minimal() +
  scale_x_discrete(labels = c("Bristol, City of" = "Bristol",
                              "Cornwall" = "Cornwall")) +
  theme(plot.title = element_text(size = 12))
ggplotly(drug_offence_rate_box_plot)

# Analyze vehicle crime rates in Bristol for November 2022
bs_vehicle_crime_rate = bristol_crime_rate %>% 
  filter(year(Year) == 2022 & month(Year) == 11) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Vehicle crime") %>% 
  filter(city == "Bristol, City of") %>% 
  filter(!is.na(Population))
dim(bs_vehicle_crime_rate)
colSums(is.na(bs_vehicle_crime_rate)) 

# Summarize vehicle crime rates by LSOA
bs_vehicle_crime_rate_summary = bs_vehicle_crime_rate %>%
  group_by(`LSOA name`) %>%
  summarise(total_vehicle_crimes = n(), 
            population = first(Population),
            vehicle_crime_rate_per_10000 = (total_vehicle_crimes / population) * 10000) %>%
  ungroup()

# Sample 20 LSOAs for radar chart visualization
set.seed(123)
bs_sampled_lsoas = bs_vehicle_crime_rate_summary %>%
  sample_n(20)

# Prepare data for radar chart by pivoting and scaling
bs_radar_data = bs_sampled_lsoas %>%
  pivot_wider(names_from = `LSOA name`, values_from = vehicle_crime_rate_per_10000, values_fill = list(vehicle_crime_rate_per_10000 = 0)) %>%
  as.data.frame()

# Add max values for radar chart scaling
bs_max_values = rep(max(bs_radar_data[-1], na.rm = TRUE), ncol(bs_radar_data))
bs_radar_data = rbind(rep(0, ncol(bs_radar_data)), bs_max_values, bs_radar_data)
dim(bs_radar_data) # Verify dimensions of the radar data

# Setup and plot radar chart for vehicle crime rates
colnames(bs_radar_data) = c("Min", "Max", paste("Area", 1:(ncol(bs_radar_data) - 2), sep = " "))

radarchart(bs_radar_data, 
           axistype = 1, 
           pcol = c("darkgreen", "orange"), 
           pfcol = c("lightgreen", "lightorange"), 
           plwd = 2, 
           cglcol = "black", 
           cglty = 1, 
           axislabcol = "black", 
           vlcex = 0.8, 
           title = "Vehicle Crime Rate per 10,000 People in Bristol (Nov 2022)",
           cex.main = 1)

# Analyze and visualize burglary crime rates in Bristol and Cornwall for November 2022
robbery_crime_rate = bc_crime_rate %>% 
  filter(year(Year) == 2022 & month(Year) == 11) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Burglary") %>% 
  filter(city == "Bristol, City of" | city == "Cornwall") %>% 
  filter(!is.na(Population))
dim(robbery_crime_rate) 
colSums(is.na(robbery_crime_rate))

# Create a pie chart showing the proportion of burglary crimes in Bristol and Cornwall
robbery_crime_rate %>%
  group_by(city) %>%
  summarise(total_burglary_crimes = n(), 
            population = first(Population),
            burglary_crime_rate_per_10000 = (total_burglary_crimes / population) * 10000) %>% 
  ungroup() %>%
  mutate(percentage = total_burglary_crimes / sum(total_burglary_crimes) * 100,
         label = paste0(round(percentage, 1), "%")) %>% 
  ggplot(aes(x = "", y = burglary_crime_rate_per_10000, fill = city)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Burglary Crime Rate per 10,000 People in Bristol and Cornwall (Nov 2022)",
       fill = "County") +
  scale_fill_manual(values = c("Bristol, City of" = "lightcoral", "Cornwall" = "lightsteelblue")) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(size = 12))

# Create a pie chart for burglary crime rates specifically in Bristol for November 2022
bs_robbery_crime_rate = bristol_crime_rate %>% 
  filter(year(Year) == 2022 & month(Year) == 11) %>% 
  filter(`Crime type` == "Burglary") %>%
  left_join(population_2022, by = "Postcode") %>% 
  filter(!is.na(Population))
dim(bs_robbery_crime_rate) 
colSums(is.na(bs_robbery_crime_rate))

# Summarize burglary crime rates by LSOA
bs_robbery_crime_rate_summary = bs_robbery_crime_rate %>%
  group_by(`LSOA name`) %>%
  summarise(total_burglary_crimes = n(), 
            population = first(Population),
            burglary_crime_rate_per_10000 = (total_burglary_crimes / population) * 10000) %>%
  ungroup()

# Sample 20 LSOAs for radar chart visualization
set.seed(123)
bs_sampled_lsoas_robbery = bs_robbery_crime_rate_summary %>%
  sample_n(20)

# Prepare data for radar chart by pivoting and scaling
bs_radar_data_robbery = bs_sampled_lsoas_robbery %>%
  pivot_wider(names_from = `LSOA name`, values_from = burglary_crime_rate_per_10000, values_fill = list(burglary_crime_rate_per_10000 = 0)) %>%
  as.data.frame()

# Add max values for radar chart scaling
bs_max_values_robbery = rep(max(bs_radar_data_robbery[-1], na.rm = TRUE), ncol(bs_radar_data_robbery))
bs_radar_data_robbery = rbind(rep(0, ncol(bs_radar_data_robbery)), bs_max_values_robbery, bs_radar_data_robbery)
dim(bs_radar_data_robbery)

# Setup and plot radar chart for burglary crime rates
colnames(bs_radar_data_robbery) = c("Min", "Max", paste("Area", 1:(ncol(bs_radar_data_robbery) - 2), sep = " "))

radarchart(bs_radar_data_robbery, 
           axistype = 1, 
           pcol = c("darkblue", "orange"), 
           pfcol = c("lightblue", "lightorange"), 
           plwd = 2, 
           cglcol = "black", 
           cglty = 1, 
           axislabcol = "black", 
           vlcex = 0.8, 
           title = "Burglary Crime Rate per 10,000 People in Bristol (Nov 2022)",
           cex.main = 1)
# Filter and prepare data for robbery crime rates in Bristol and Cornwall for November 2022
robbery_crime_rate = bc_crime_rate %>% 
  filter(year(Year) == 2022 & month(Year) == 11) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Burglary") %>% 
  filter(city == "Bristol, City of" | city == "Cornwall") %>% 
  filter(!is.na(Population))

# Display dimensions and check for missing values in the dataset
dim(robbery_crime_rate)
colSums(is.na(robbery_crime_rate))

# Create a pie chart showing the proportion of burglary crime rates in Bristol and Cornwall
robbery_crime_rate %>%
  group_by(city) %>%
  summarise(total_burglary_crimes = n(), 
            population = first(Population),
            burglary_crime_rate_per_10000 = (total_burglary_crimes / population) * 10000) %>% 
  ungroup() %>%
  mutate(percentage = total_burglary_crimes / sum(total_burglary_crimes) * 100,
         label = paste0(round(percentage, 1), "%")) %>% 
  ggplot(aes(x = "", y = burglary_crime_rate_per_10000, fill = city)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Burglary Crime Rate per 10,000 People in Bristol and Cornwall (Nov 2022)",
       fill = "County") +
  scale_fill_manual(values = c("Bristol, City of" = "lightcoral", "Cornwall" = "lightsteelblue")) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(size = 12))

# Filter and prepare data for burglary crime rates in Bristol for November 2022
bs_robbery_crime_rate = bristol_crime_rate %>% 
  filter(year(Year) == 2022 & month(Year) == 11) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Burglary") %>% 
  filter(city == "Bristol, City of") %>% 
  filter(!is.na(Population))

# Display dimensions and check for missing values in the Bristol burglary crime dataset
dim(bs_robbery_crime_rate)
colSums(is.na(bs_robbery_crime_rate))

# Create a pie chart showing the proportion of burglary crime rates in different LSOAs within Bristol for November 2022
bs_robbery_crime_rate %>%
  group_by(`LSOA name`) %>%
  summarise(total_burglary_crimes = n(), 
            population = first(Population),
            burglary_crime_rate_per_10000 = (total_burglary_crimes / population) * 10000) %>% 
  ungroup() %>%
  mutate(percentage = total_burglary_crimes / sum(total_burglary_crimes) * 100,
         label = paste0(round(percentage, 1), "%")) %>% 
  ggplot(aes(x = "", y = burglary_crime_rate_per_10000, fill = `LSOA name`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Burglary Crime Rate per 10,000 People in Bristol (Nov 2022)",
       fill = "LSOA") +
  scale_fill_viridis_d() +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(size = 12))

# Filter and prepare data for drug offence rates in Bristol and Cornwall for the year 2022
drug_offence_rate = bc_crime_rate %>% 
  filter(year(Year) == 2022) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Drugs") %>% 
  filter(city == "Bristol, City of" | city == "Cornwall") %>% 
  filter(!is.na(Population)) %>%
  group_by(Year, city) %>%
  summarise(drug_offenses = n(),
            population = first(Population)) %>%
  mutate(drug_offense_rate = drug_offenses / population * 10000) %>%
  ungroup()

# Create a line chart showing the drug offence rates over the year 2022 for Bristol and Cornwall
drug_offence_rate_line_chart = plot_ly(data = drug_offence_rate, 
                                        x = ~Year, 
                                        y = ~drug_offense_rate, 
                                        color = ~city, 
                                        type = 'scatter', 
                                        mode = 'lines+markers') %>%
  layout(title = "Line Plot: Drug Offence Rates per 10,000 People in Bristol and Cornwall (2022)",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Drug Offence Rate per 10,000 People"),
         legend = list(title = list(text = "City")),
         hovermode = "closest")
drug_offence_rate_line_chart
