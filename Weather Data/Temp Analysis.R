

# Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)


# Set working directory

setwd("C:\\Users\\romin\\Desktop\\My Thesis\\SQM CSVs\\Weather Data")

# Create a data name

stx_weather <- read.csv("Climate Data.csv")


weather_clean <-stx_weather %>%
  select(
    Name, 
    Date, 
    Tmax=TMAX..Maximum.Temperature.,
    Tmin=TMIN..Minimum.Temperature.)


weather_monthly_per_year <- weather_clean %>%
  # 1. Convert Date column to true Date format
  mutate(Date = as.Date(Date)) %>% 
  mutate(
    Year = year(Date),
    Month = month(Date)) %>%
  group_by(Year, Month) %>% 
  summarise(
    Avg_Max_Temp = mean(Tmax, na.rm = TRUE),
    Avg_Min_Temp = mean(Tmin, na.rm = TRUE),
    .groups = "drop")

# View the detailed multi-year breakdown
View(weather_monthly_per_year)


ggplot(weather_monthly_per_year, aes(x = Avg_Max_Temp, y = Avg_Min_Temp)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  labs(
    title = NULL,
    x = "TMAX (°C)",
    y = "TMIN (°C)"
  ) +
  theme_minimal()


weather_long <- weather_monthly_per_year %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
  pivot_longer(
    cols = c(Avg_Max_Temp, Avg_Min_Temp),
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(weather_long, aes(x = Date, y = Value, color = Variable)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "Monthly Average TMAX and TMIN Over Time",
    x = "Date",
    y = "Temperature (°C)",
    color = "Variable"
  ) +
  theme_minimal()

























  