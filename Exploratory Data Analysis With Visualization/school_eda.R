# Load libraries
library(tidyverse)
library(plotly)  

# Load cleaned datasets
bristol_schools = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/schools/bristol-schools.csv")
cornwall_schools = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/schools/cornwall-schools.csv")

# Clean and combine data, convert ATT8SCR to numeric, and compute averages
cleaned_data = bind_rows(
  bristol_schools %>% mutate(County = "Bristol"),
  cornwall_schools %>% mutate(County = "Cornwall")
) %>%
  filter(Year == 2022 & !ATT8SCR %in% c("SUPP", "NE")) %>%
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>%
  group_by(School_Name = SCHNAME, County) %>%
  summarise(Average_ATT8 = mean(ATT8SCR, na.rm = TRUE), .groups = "drop")

# Create box plot for Attainment 8 scores
box_plot = cleaned_data %>%
  ggplot(aes(x = County, y = Average_ATT8, fill = County)) +
  geom_boxplot() +
  labs(title = "Distribution of Average Attainment 8 Scores in Bristol and Cornwall (2022)",
       x = "County",
       y = "Average Attainment 8 Score") +
  scale_fill_manual(values = c("Bristol" = "skyblue", "Cornwall" = "salmon")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
ggplotly(box_plot, tooltip = "text")  

# Create line plot for Bristol schools' Attainment 8 scores
line_plot_bristol = cleaned_data %>%
  filter(County == "Bristol") %>%
  ggplot(aes(x = School_Name, y = Average_ATT8, group = 1, color = County)) +
  geom_line() +
  geom_point() + 
  labs(title = "Average Attainment 8 Scores in Bristol (2022)",
       x = "School",
       y = "Average Attainment 8 Score") +
  scale_color_manual(values = "skyblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
        plot.title = element_text(size = 12),
        axis.text.y = element_text(size = 10))
ggplotly(line_plot_bristol, tooltip = "text")

# Create line plot for Cornwall schools' Attainment 8 scores
line_plot_cornwall = cleaned_data %>%
  filter(County == "Cornwall") %>%
  ggplot(aes(x = School_Name, y = Average_ATT8, group = 1, color = County)) +
  geom_line() +
  geom_point() + 
  labs(title = "Average Attainment 8 Scores in Cornwall (2022)",
       x = "School",
       y = "Average Attainment 8 Score") +
  scale_color_manual(values = "salmon") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
        plot.title = element_text(size = 12),
        axis.text.y = element_text(size = 10))
ggplotly(line_plot_cornwall, tooltip = "text")
