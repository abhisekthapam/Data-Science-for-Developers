# Load necessary libraries
library(plotly)   
library(readr)    
library(dplyr) 

# Load the cleaned data from CSV files
file_path_avg_2022 = "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/house/average_house_price_2022.csv"
file_path_avg_2020_2023 = "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/house/average_house_price_2020_2023.csv"

# Read the cleaned data for average house prices in 2022
average_price_2022 = read_csv(file_path_avg_2022)
# Read the cleaned data for average house prices from 2020 to 2023
line_chart_data = read_csv(file_path_avg_2020_2023)

# Create a Boxplot to visualize the distribution of average house prices in 2022 for both counties
boxplot_fig = plot_ly(average_price_2022, y = ~Average_Price, x = ~County, type = "box", color = ~County, 
                       colors = c("#1f77b4", "#ff7f0e")) %>%
  layout(title = "Average House Price in 2022 (Boxplot) - City of Bristol and Cornwall",
         yaxis = list(title = "Price (in currency units)"),
         xaxis = list(title = "County"))
boxplot_fig

# Create a Bar Chart to compare the average house prices in 2022 between the two counties
barchart_fig = plot_ly(average_price_2022, x = ~County, y = ~Average_Price, type = 'bar', color = ~County,
                        colors = c("#1f77b4", "#ff7f0e")) %>%
  layout(title = "Average House Price in 2022 (Bar Chart) - City of Bristol and Cornwall",
         yaxis = list(title = "Average Price (in currency units)"),
         xaxis = list(title = "County"))
barchart_fig

# Create a Line Chart to visualize the trend of average house prices from 2020 to 2023 for both counties
line_chart_fig = plot_ly(line_chart_data, x = ~Year, y = ~Average_Price, color = ~County, type = 'scatter', mode = 'lines+markers',
                          colors = c("#1f77b4", "#ff7f0e")) %>%
  layout(title = "Average House Price from 2020 to 2023",
         yaxis = list(title = "Average Price (in currency units)"),
         xaxis = list(title = "Year"))
line_chart_fig
