# Load the necessary library for data manipulation
library(tidyverse)

# Read crime rate data for Bristol and Cornwall
bristolCrimeRate = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/crime/bristol-crime-rate.csv")
cornwallCrimeRate = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/crime/cornwall-crime-rate.csv")

# Read school performance data for Bristol and Cornwall
bristolSchools = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/schools/bristol-schools.csv")
cornwallSchools = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/schools/cornwall-schools.csv")

# Filter Bristol schools data for the years 2022 and 2023
bristolSchools_2022_2023 = bristolSchools %>% 
  filter(Year == 2022 | Year == 2023) 

# Check for missing values in the Bristol schools data
colSums(is.na(bristolSchools_2022_2023))

# Clean Bristol schools data by removing rows with specific values and converting ATT8SCR to numeric
bristolSchools_2022_2023 = bristolSchools_2022_2023 %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR))

# Filter Cornwall schools data for the years 2022 and 2023
cornwallSchools_2022_2023 = cornwallSchools %>% 
  filter(Year == 2022 | Year == 2023)

# Check for missing values in the Cornwall schools data
colSums(is.na(cornwallSchools_2022_2023))

# Clean Cornwall schools data by removing rows with specific values and converting ATT8SCR to numeric
cornwallSchools_2022_2023 = cornwallSchools_2022_2023 %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR))

# Read population data for 2011 according to postcodes
population_2011 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/population/Population2011_1656567141570.csv")

# Adjust population data to reflect 2023 estimates
population_2023 = population_2011 %>% 
  mutate(Population = Population * 1.00561255390388033)

# Convert 2023 population estimates back to 2022
years_between = 2023-2011
annual_growth_rate = 1.00561255390388033 ^ (1/years_between)
population_2022 = population_2023 %>% 
  mutate(Population = Population/annual_growth_rate)

# Combine 2022 and 2023 population data and compute average population
population_2022_2023 = left_join(population_2022, population_2023, by = "Postcode") %>%
  rename(population_2022 = Population.x, population_2023 = Population.y) %>%
  mutate(avg_population = (population_2022 + population_2023) / 2)

# Clean and prepare crime rate data for Bristol
bristolCrimeRate = bristolCrimeRate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))

# Clean and prepare crime rate data for Cornwall
cornwallCrimeRate = cornwallCrimeRate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))

# Calculate drug offence rates for Bristol in 2022 and 2023
bs_drugOffenceRate_2022_2023 = bristolCrimeRate %>% 
  filter(year(Year) %in% c(2022, 2023)) %>% 
  left_join(population_2022_2023, by = "Postcode") %>% 
  filter(`Crime type` == "Drugs") %>% 
  filter(city == "Bristol, City of") %>% 
  group_by(postcode_space) %>%
  summarise(drug_offences = n(), avg_population = first(avg_population)) %>%
  mutate(drug_offence_rate = drug_offences / avg_population * 10000)

# Check dimensions and missing values for Bristol drug offence rate data
dim(bs_drugOffenceRate_2022_2023)
colSums(is.na(bs_drugOffenceRate_2022_2023))

# Remove rows with missing average population for Bristol data
bs_drugOffenceRate_2022_2023 = bs_drugOffenceRate_2022_2023 %>% 
  filter(!is.na(avg_population))

# Calculate drug offence rates for Cornwall in 2022 and 2023
cw_drugOffenceRate_2022_2023 = cornwallCrimeRate %>% 
  filter(year(Year) %in% c(2022, 2023)) %>% 
  left_join(population_2022_2023, by = "Postcode") %>% 
  filter(`Crime type` == "Drugs") %>% 
  filter(city == "Cornwall") %>% 
  group_by(postcode_space) %>%
  summarise(drug_offences = n(), avg_population = first(avg_population)) %>%
  mutate(drug_offence_rate = drug_offences / avg_population * 10000)

# Check dimensions and missing values for Cornwall drug offence rate data
dim(cw_drugOffenceRate_2022_2023)
colSums(is.na(cw_drugOffenceRate_2022_2023))

# Remove rows with missing average population for Cornwall data
cw_drugOffenceRate_2022_2023 = cw_drugOffenceRate_2022_2023 %>% 
  filter(!is.na(avg_population))

# Linear modelling
# Join Bristol school performance data with Bristol drug offence rate data
bs_att8scr_drugRate_2022_2023 = inner_join(
  bristolSchools_2022_2023, bs_drugOffenceRate_2022_2023,
  by = c("POSTCODE" = "postcode_space")) %>% 
  select(ATT8SCR, drug_offence_rate)

# Join Cornwall school performance data with Cornwall drug offence rate data
cw_att8scr_drugRate_2022_2023 = inner_join(
  cornwallSchools_2022_2023, cw_drugOffenceRate_2022_2023,
  by = c("POSTCODE" = "postcode_space"))
