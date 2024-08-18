# Load necessary library
library(tidyverse)
library(plotly)

# Load Bristol and Cornwall broadband speed data
bristolBroadbandSpeed = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/broadband/bristol-broadband-speed.csv")
cornwallBroadbandSpeed = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/broadband/cornwall-broadband-speed.csv")

# Load Bristol and Cornwall schools data
bristolSchools = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/schools/bristol-schools.csv")
cornwallSchools = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/schools/cornwall-schools.csv")

# Filter and clean 2022 Bristol schools data
bristolSchools2022 = bristolSchools %>% 
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR))

# Filter and clean 2022 Cornwall schools data
cornwallSchools2022 = cornwallSchools %>% 
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR))

# Linear modeling for Bristol: Attainment 8 Score vs. Average Download Speed
bs_att8scr_downloadSpeed_2022 = bristolSchools2022 %>% 
  inner_join(bristolBroadbandSpeed, by = c("POSTCODE" = "postcode_space")) %>% 
  select(ATT8SCR, `Average download speed (Mbit/s)`)

# Calculate correlation coefficient for Bristol data
bs_att8scr_downloadSpeed_2022 %>% 
  summarise(corCoeff = cor(ATT8SCR, `Average download speed (Mbit/s)`))

# Build linear model for Bristol
bsModel_att8scr_downloadSpeed = lm(ATT8SCR ~ `Average download speed (Mbit/s)`, data = bs_att8scr_downloadSpeed_2022)

# Extract intercept and slope for Bristol model
bsIntercept_att8scr_downloadSpeed = coef(bsModel_att8scr_downloadSpeed)[1]
bsSlope_att8scr_downloadSpeed = coef(bsModel_att8scr_downloadSpeed)[2]

# Plotting Bristol data using Plotly
plot_ly(bs_att8scr_downloadSpeed_2022, x = ~`Average download speed (Mbit/s)`, y = ~ATT8SCR, type = 'scatter', mode = 'markers', marker = list(color = 'blue')) %>%
  add_lines(x = ~`Average download speed (Mbit/s)`, y = ~fitted(bsModel_att8scr_downloadSpeed), line = list(color = 'red')) %>%
  layout(title = "Impact of Average Download Speed on Attainment 8 Score in Bristol Schools (2022)",
         xaxis = list(title = "Average Download Speed (Mbit/s)"),
         yaxis = list(title = "Attainment 8 Score"))

# Linear modeling for Cornwall: Attainment 8 Score vs. Average Download Speed
cw_att8scr_downloadSpeed_2022 = cornwallSchools2022 %>% 
  inner_join(cornwallBroadbandSpeed, by = c("POSTCODE" = "postcode_space")) %>% 
  select(ATT8SCR, `Average download speed (Mbit/s)`)

# Calculate correlation coefficient for Cornwall data
cw_att8scr_downloadSpeed_2022 %>% 
  summarise(corCoeff = cor(ATT8SCR, `Average download speed (Mbit/s)`))

# Build linear model for Cornwall
cwModel_att8scr_downloadSpeed = lm(ATT8SCR ~ `Average download speed (Mbit/s)`, data = cw_att8scr_downloadSpeed_2022)

# Extract intercept and slope for Cornwall model
cwIntercept_att8scr_downloadSpeed = coef(cwModel_att8scr_downloadSpeed)[1]
cwSlope_att8scr_downloadSpeed = coef(cwModel_att8scr_downloadSpeed)[2]

# Plotting Cornwall data using Plotly
plot_ly(cw_att8scr_downloadSpeed_2022, x = ~`Average download speed (Mbit/s)`, y = ~ATT8SCR, type = 'scatter', mode = 'markers', marker = list(color = 'blue')) %>%
  add_lines(x = ~`Average download speed (Mbit/s)`, y = ~fitted(cwModel_att8scr_downloadSpeed), line = list(color = 'red')) %>%
  layout(title = "Impact of Average Download Speed on Attainment 8 Score in Cornwall Schools (2022)",
         xaxis = list(title = "Average Download Speed (Mbit/s)"),
         yaxis = list(title = "Attainment 8 Score"))
