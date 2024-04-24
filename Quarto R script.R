# libraries
library(tidyverse)
library(ggplot2)
library(maps)
library(countrycode)

# Data Sets
data_indicators <- read.csv("unicef_indicator_1.csv")
data_metadata <- read.csv("unicef_metadata.csv")


# Data Transformation
# Defining the columns needed for this report
meta_columns <- c("country", "year", "Population..total", "GDP.per.capita..constant.2015.US..", "Life.expectancy.at.birth..total..years.")
indicator_columns <- c("country", "time_period", "indicator", "obs_value", "sex")

# Preparing the data
cleaned_metadata <- data_metadata %>%
  select(all_of(meta_columns)) %>%
  filter(!is.na(country), !is.na(year), !is.na(Population..total), !is.na(GDP.per.capita..constant.2015.US..), !is.na(Life.expectancy.at.birth..total..years.))

cleaned_indicator <- data_indicators %>%
  select(all_of(indicator_columns)) %>%
  filter(!is.na(country), !is.na(time_period), !is.na(indicator), !is.na(obs_value), !is.na(sex)) %>%
  rename(year = time_period)

# Joining the data and adding continent column
combined_data <- inner_join(cleaned_indicator, cleaned_metadata, by = c("country", "year")) %>%
  filter(!sex %in% c("Male", "Female")) %>%
  mutate(continent = countrycode(country, "country.name", "continent"))


#Education World map

world_map <- map_data("world")
map_data <- merge(world_map, combined_data, by.x = "region", by.y = "country", all.x = TRUE)

# plot
ggplot(map_data, aes(x = long, y = lat, group = group, fill = obs_value)) +
  geom_polygon(color = NA) +  # Remove borders by setting color = NA
  scale_fill_gradient2(low = "red", mid = "lightgreen", high = "green", 
                       midpoint = median(map_data$obs_value, na.rm = TRUE), 
                       na.value = "grey",  
                       name = "Obs Value") +
  labs(title = "Education World Map",  
       fill = "Observation Value") +  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.position = "right",
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.background = element_rect(fill = "#f8f8f8"),
    axis.text.x = element_blank(),  
    axis.text.y = element_blank(),  
    axis.ticks = element_blank(),  
    axis.title.x = element_blank(),  
    axis.title.y = element_blank()   
  )

# Bar Chart(average between continents)

continent_summary <- combined_data %>%
  filter(year == 2018) %>%
  group_by(continent) %>%
  summarize(
    average_obs = mean(obs_value, na.rm = TRUE),  
    count = n(),                                 
    .groups = 'drop'                             
  ) %>%
  mutate(continent = fct_reorder(continent, average_obs))  

# bar chart
ggplot(continent_summary, aes(x = continent, y = average_obs, fill = continent)) +
  geom_col() +  # Create columns
  scale_fill_manual(values = c("slategray", "darkslategray", "lightblue4", "steelblue", "cadetblue")) +
  labs(
    title = "Educational Disparity by Continent in 2018",
    subtitle = "Visualizing average observation values across continents",
    x = "Continent",
    y = "Average Observation Value"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    text = element_text(size = 14, color = "gray20"),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "gray20"),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray40"),
    plot.background = element_rect(fill = "#f0f0f0"),
    panel.background = element_rect(fill = "#f8f8f8"),
    legend.position = "none"  
  )

# scatter plot
# obs and gdp
ggplot(data = combined_data, aes(x = GDP.per.capita..constant.2015.US.., y = obs_value, color = continent)) +
  geom_point(alpha = 0.8, size = 3) +  
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  
  scale_color_manual(values = c("deepskyblue3", "darkorange2", "mediumseagreen", "orchid3", "gold3")) +  
  labs(title = "Observation Value vs GDP by Continent",
       subtitle = "Analyzing correlations between GDP and education outcomes",
       x = "GDP (constant 2015 US$)",
       y = "Observation Value",
       color = "Continent") +
  theme_minimal(base_family = "Helvetica") +
  theme(
    text = element_text(size = 14, color = "gray20"),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "gray20"),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray40"),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.background = element_rect(fill = "#f8f8f8"),
    legend.position = "right",
    plot.margin = margin(1, 1, 1, 1, "cm")  
  )

#gdp and life expectency

ggplot(data = combined_data, aes(x = GDP.per.capita..constant.2015.US.., y = Life.expectancy.at.birth..total..years., color = continent)) +
  geom_point(alpha = 0.8, size = 3) +  
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  
  scale_color_manual(values = c("deepskyblue3", "darkorange2", "mediumseagreen", "orchid3", "gold3")) +  
  labs(title = "Life Expectancy vs GDP by Continent",
       subtitle = "Analyzing correlations between GDP and Life Expectancy",
       x = "GDP (constant 2015 US$)",
       y = "Life Expectancy",
       color = "Continent") +
  theme_minimal(base_family = "Helvetica") +
  theme(
    text = element_text(size = 14, color = "gray20"),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "gray20"),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray40"),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.background = element_rect(fill = "#f8f8f8"),
    legend.position = "right",
    plot.margin = margin(1, 1, 1, 1, "cm")  
  )

# Time series chart
continent_data <- combined_data %>%
  filter(year >= 2015, year <= 2018) %>%
  group_by(continent, year) %>%
  summarize(average_obs = mean(obs_value, na.rm = TRUE), .groups = 'drop')

ggplot(data = continent_data, aes(x = year, y = average_obs, color = continent, group = continent)) +
  geom_line(size = 1.2) +  # Line size slightly thicker for visibility
  labs(title = "Educational Trends by Continent",
       subtitle = "Annual changes in average education values from 2015 to 2018",
       x = "Year",
       y = "Average Education Observation Value",
       color = "Continent") +
  scale_color_manual(values = c("deepskyblue3", "darkorange2", "mediumseagreen", "orchid3", "gold3")) +  # Custom color palette to match other charts
  theme_minimal(base_family = "Helvetica") +  # Consistent font family as other visuals
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "#f0f0f0", color = NA),  # Light grey background
    panel.background = element_rect(fill = "#f8f8f8"),  # Slightly different shade for the panel
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )