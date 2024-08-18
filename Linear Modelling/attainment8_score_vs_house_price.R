#Load necessay library
library(tidyverse)  
library(lubridate)  
library(plotly)      

# Load the cleaned Bristol and Cornwall house pricing and schools data
bristolHousePrice <- read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/house/bristol-house-pricing.csv")
cornwallHousePrice <- read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/house/cornwall-house-pricing.csv")
bristolSchools <- read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/schools/bristol-schools.csv")
cornwallSchools <- read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/schools/cornwall-schools.csv")

# Filter and summarize the Bristol house prices for 2022-2023
bristolHousePrice_2022_2023 <- bristolHousePrice %>% 
  filter(year(Date_of_transfer) %in% c(2022, 2023)) %>%   
  group_by(Postcode) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE), .groups = 'drop') 

# Filter and summarize the Cornwall house prices for 2022-2023
cornwallHousePrice_2022_2023 <- cornwallHousePrice %>% 
  filter(year(Date_of_transfer) %in% c(2022, 2023)) %>%   
  group_by(Postcode) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE), .groups = 'drop') 

# Filter and summarize the Bristol schools' Attainment 8 scores for 2022-2023
bristolSchools_2022_2023 <- bristolSchools %>% 
  filter(Year %in% c(2022, 2023)) %>%   
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>%   
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>%   
  group_by(POSTCODE) %>%
  summarise(Avg_ATT8SCR = mean(ATT8SCR, na.rm = TRUE), .groups = 'drop')  

# Filter and summarize the Cornwall schools' Attainment 8 scores for 2022-2023
cornwallSchools_2022_2023 <- cornwallSchools %>% 
  filter(Year %in% c(2022, 2023)) %>%   
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>%   
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>%   
  group_by(POSTCODE) %>%
  summarise(Avg_ATT8SCR = mean(ATT8SCR, na.rm = TRUE), .groups = 'drop')  

# Linear Modeling: Bristol
# Combine Bristol house prices and school scores for correlation analysis
bs_housePrice_att8scr_2022_2023 <- inner_join(bristolHousePrice_2022_2023, bristolSchools_2022_2023, 
                                              by = c("Postcode" = "POSTCODE")) %>% 
  select(Avg_Price, Avg_ATT8SCR)

# Calculate correlation between house prices and school scores in Bristol
bs_correlation <- bs_housePrice_att8scr_2022_2023 %>% 
  summarise(corCoeff = cor(Avg_Price, Avg_ATT8SCR)) %>% 
  pull(corCoeff)

# Create linear regression model for Bristol data
bsModel_housePrice_att8scr_2022_2023 <- lm(Avg_Price ~ Avg_ATT8SCR, 
                                           data = bs_housePrice_att8scr_2022_2023)

# Extract intercept and slope for the regression line
bsIntercept_housePrice_att8scr_2022_2023 <- coef(bsModel_housePrice_att8scr_2022_2023)[1]
bsSlope_housePrice_att8scr_2022_2023 <- coef(bsModel_housePrice_att8scr_2022_2023)[2]

# Visualize the Bristol data and regression line using Plotly
plotly::plot_ly(bs_housePrice_att8scr_2022_2023, x = ~Avg_ATT8SCR, y = ~Avg_Price, type = 'scatter', mode = 'markers',
                marker = list(color = 'blue')) %>%
  add_lines(x = ~Avg_ATT8SCR, y = ~ (bsIntercept_housePrice_att8scr_2022_2023 + bsSlope_housePrice_att8scr_2022_2023 * Avg_ATT8SCR),
            line = list(color = 'red')) %>%
  layout(
    title = "Influence of Attainment 8 Scores on House Price in Bristol (2022-2023)",
    xaxis = list(title = "Attainment 8 Scores (average)"),
    yaxis = list(title = "House Price (average)")
  )

# Linear Modeling: Cornwall
# Combine Cornwall house prices and school scores for correlation analysis
cw_housePrice_att8scr_2022_2023 <- inner_join(cornwallHousePrice_2022_2023, cornwallSchools_2022_2023, 
                                              by = c("Postcode" = "POSTCODE")) %>% 
  select(Avg_Price, Avg_ATT8SCR)

# Calculate correlation between house prices and school scores in Cornwall
cw_correlation <- cw_housePrice_att8scr_2022_2023 %>% 
  summarise(corCoeff = cor(Avg_Price, Avg_ATT8SCR)) %>% 
  pull(corCoeff)

# Create linear regression model for Cornwall data
cwModel_housePrice_att8scr_2022_2023 <- lm(Avg_Price ~ Avg_ATT8SCR, 
                                           data = cw_housePrice_att8scr_2022_2023)

# Extract intercept and slope for the regression line
cwIntercept_housePrice_att8scr_2022_2023 <- coef(cwModel_housePrice_att8scr_2022_2023)[1]
cwSlope_housePrice_att8scr_2022_2023 <- coef(cwModel_housePrice_att8scr_2022_2023)[2]

# Visualize the Cornwall data and regression line using Plotly
plotly::plot_ly(cw_housePrice_att8scr_2022_2023, x = ~Avg_ATT8SCR, y = ~Avg_Price, type = 'scatter', mode = 'markers',
                marker = list(color = 'blue')) %>%
  add_lines(x = ~Avg_ATT8SCR, y = ~ (cwIntercept_housePrice_att8scr_2022_2023 + cwSlope_housePrice_att8scr_2022_2023 * Avg_ATT8SCR),
            line = list(color = 'red')) %>%
  layout(
    title = "Influence of Attainment 8 Scores on House Price in Cornwall (2022-2023)",
    xaxis = list(title = "Attainment 8 Scores (average)"),
    yaxis = list(title = "House Price (average)")
  )
