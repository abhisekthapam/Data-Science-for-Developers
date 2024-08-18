# Load necessary libraries
library(tidyverse)  
library(lubridate) 
library(plotly)     

# Load cleaned datasets
file_paths = list(
  bristol_housing = "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/house/bristol-house-pricing.csv",
  cornwall_housing = "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/house/cornwall-house-pricing.csv",
  bc_housing = "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/house/average_house_price_2022.csv",
  bristol_broadband = "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/broadband/bristol-broadband-speed.csv",
  cornwall_broadband = "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/broadband/cornwall-broadband-speed.csv",
  bc_broadband = "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/broadband/bristol-cornwall-broadband-speed.csv"
)

data_list = lapply(file_paths, read_csv)

# Assign loaded data to variables
bristol_housing = data_list$bristol_housing
cornwall_housing = data_list$cornwall_housing
bc_housing = data_list$bc_housing
bristol_broadband = data_list$bristol_broadband
cornwall_broadband = data_list$cornwall_broadband
bc_broadband = data_list$bc_broadband

# Define function to process data and create plot
create_plot = function(housing_data, broadband_data, title) {
  # Merge datasets and filter for 2022
  merged_data = inner_join(
    housing_data, 
    broadband_data,
    by = c("Postcode" = "postcode_space")
  ) %>% 
    filter(year(Date_of_transfer) == 2022) %>% 
    select(Price, `Average download speed (Mbit/s)`)
  
  # Create linear model
  model = lm(Price ~ `Average download speed (Mbit/s)`, data = merged_data)
  intercept = coef(model)[1]
  slope = coef(model)[2]
  
  # Create scatter plot with Plotly
  plot = plot_ly(
    data = merged_data,
    x = ~`Average download speed (Mbit/s)`,
    y = ~Price,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'blue', size = 8),
    name = 'Data Points'
  ) %>%
    add_lines(
      x = c(min(merged_data$`Average download speed (Mbit/s)`), 
            max(merged_data$`Average download speed (Mbit/s)`)),
      y = intercept + slope * 
            c(min(merged_data$`Average download speed (Mbit/s)`), 
              max(merged_data$`Average download speed (Mbit/s)`)),
      line = list(color = 'red'),
      name = 'Trend Line'
    ) %>%
    layout(
      title = title,
      xaxis = list(title = "Average Download Speed (Mbit/s)"),
      yaxis = list(title = "House Price")
    )
  
  return(plot)
}

# Generate and display plots
plot_bristol = create_plot(bristol_housing, bristol_broadband, "Impact of Average Download Speed on House Price in Bristol (2022)")
plot_cornwall = create_plot(cornwall_housing, cornwall_broadband, "Impact of Average Download Speed on House Price in Cornwall (2022)")

# Display the plots
plot_bristol
plot_cornwall
