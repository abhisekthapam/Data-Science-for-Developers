# Load necessary libraries for data manipulation and visualization
library(plotly) 
library(readr)    
library(dplyr)    

# Define file paths for broadband speed data for Bristol and Cornwall
file_path_bristol = "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/broadband/bristol-broadband-speed.csv"
file_path_cornwall = "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/broadband/cornwall-broadband-speed.csv"

# Load the broadband speed data for Bristol and Cornwall from CSV files
bristol_data = read_csv(file_path_bristol)
cornwall_data = read_csv(file_path_cornwall)

bristol_data = bristol_data %>% mutate(County = "CITY OF BRISTOL")
cornwall_data = cornwall_data %>% mutate(County = "CORNWALL")

# Combine Bristol and Cornwall datasets into a single data frame for unified analysis
combined_data = bind_rows(bristol_data, cornwall_data)
combined_data = combined_data %>%
  rename(Download_Speed = `Average download speed (Mbit/s)`,
         Max_Download_Speed = `Maximum download speed (Mbit/s)`)

#Boxplot to visualize the distribution of average download speeds across Bristol and Cornwall
boxplot_fig = plot_ly(combined_data, y = ~Download_Speed, x = ~County, type = "box", color = ~County, 
                       colors = c("#1f77b4", "#ff7f0e")) %>%
  layout(title = "Average Download Speeds in Both Counties (Boxplot)",
         yaxis = list(title = "Download Speed (Mbit/s)"),
         xaxis = list(title = "County"))
boxplot_fig

# Path for the combined broadband speed data (if available) for bar charts
file_path_combined = "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/broadband/bristol-cornwall-broadband-speed.csv"

# Load the combined broadband speed data from the file
combined_broadband_data = read_csv(file_path_combined)
combined_broadband_data = combined_broadband_data %>%
  rename(
    Average_Download_Speed = `Average download speed (Mbit/s)`,
    Maximum_Download_Speed = `Maximum download speed (Mbit/s)`
  )

# Add a County column to the combined dataset for filtering based on postcode area
combined_broadband_data = combined_broadband_data %>%
  mutate(County = if_else(grepl("Bristol", `postcode area`), "CITY OF BRISTOL", "CORNWALL"))

# Filter data for Bristol and Cornwall separately for bar chart visualization
bristol_data_combined = combined_broadband_data %>% filter(County == "CITY OF BRISTOL")
cornwall_data_combined = combined_broadband_data %>% filter(County == "CORNWALL")

# This chart shows the distribution of average download speeds in Bristol
avg_download_speed_fig_bristol = plot_ly(bristol_data_combined, x = ~Average_Download_Speed, type = 'bar', 
                                          name = 'Average Download Speed', 
                                          marker = list(color = '#1f77b4')) %>%
  layout(title = "Average Download Speed - Bristol",
         xaxis = list(title = "Average Download Speed (Mbit/s)"),
         yaxis = list(title = "Count"))

# This chart shows the distribution of average download speeds in Cornwall
avg_download_speed_fig_cornwall = plot_ly(cornwall_data_combined, x = ~Average_Download_Speed, type = 'bar', 
                                           name = 'Average Download Speed', 
                                           marker = list(color = '#ff7f0e')) %>%
  layout(title = "Average Download Speed - Cornwall",
         xaxis = list(title = "Average Download Speed (Mbit/s)"),
         yaxis = list(title = "Count"))

# This chart displays the distribution of maximum download speeds in Bristol
max_download_speed_fig_bristol = plot_ly(bristol_data_combined, x = ~Maximum_Download_Speed, type = 'bar', 
                                          name = 'Maximum Download Speed', 
                                          marker = list(color = '#1f77b4')) %>%
  layout(title = "Maximum Download Speed - Bristol",
         xaxis = list(title = "Maximum Download Speed (Mbit/s)"),
         yaxis = list(title = "Count"))

# This chart displays the distribution of maximum download speeds in Cornwall
max_download_speed_fig_cornwall = plot_ly(cornwall_data_combined, x = ~Maximum_Download_Speed, type = 'bar', 
                                           name = 'Maximum Download Speed', 
                                           marker = list(color = '#ff7f0e')) %>%
  layout(title = "Maximum Download Speed - Cornwall",
         xaxis = list(title = "Maximum Download Speed (Mbit/s)"),
         yaxis = list(title = "Count"))

# Display the bar charts for average and maximum download speeds
avg_download_speed_fig_bristol
avg_download_speed_fig_cornwall
max_download_speed_fig_bristol
max_download_speed_fig_cornwall
