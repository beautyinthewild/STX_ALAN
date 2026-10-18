
### EEMP Turtle Data from 2019 - 2023

# Set Working Directory, Load libraries and CSV File

library(tidyverse)
library(dplyr)
library(ggplot2)


setwd("C:\\Users\\romin\\Desktop\\My Thesis\\SQM CSVs\\Raw Turtle Data")

eemp_data <-read.csv("EEMP Turtle Data.csv")

eemp_data_clean <- eemp_data %>%
  filter(Species %in% c("CM", "EI")) %>% #remove DC (leatherback)
  mutate(
    Probable.Lays = as.numeric(Probable.Lays),
    Dry.Runs      = as.numeric(Dry.Runs),
    Tracks        = as.numeric(Tracks),
    Total_NNEs    = rowSums(across(c(Probable.Lays, Dry.Runs, Tracks)), na.rm = TRUE)
  ) # add all the activities into one column called NNEs


# Calculate for sea turtle density
## Formula: nest density = total number of nests or nnes / beach length (km)

eemp_data_clean_2 <- eemp_data_clean%>%
  select(-Probable.Lays, -Dry.Runs, -Tracks)%>% #remove these columns to clean up
  mutate(
    Nests           = as.numeric(Nests),
    Total_NNEs      = as.numeric(Total_NNEs),
    Beach.Length.km = as.numeric(Beach.Length.km)) %>%
  mutate(
    Nest_Density = Nests / Beach.Length.km, #apply formula
    NNEs_Density = Total_NNEs / Beach.Length.km) #apply formula


eemp_plot_clean <-eemp_data_clean_2 %>%
  select(-Nests, -Beach.Length.km, -Total_NNEs) # keep nest density and nnes density


# Faceted line plot

ggplot(eemp_data_clean_2, aes(x = Year, y = Nest_Density, color = Species)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~ East.End.Marine.Park) +
  labs(title = "Nest Density Across Years by Species and Location",
       y = "Nest Density (nests/km)")


# Bar plot for each species
ggplot(eemp_data_clean_2, aes(x = East.End.Marine.Park,
                                   y = Nest_Density,
                                   fill = Species)) +
  geom_col(position = "dodge") +
  labs(title = "Mean Nest Density (2019–2023)",
       y = "Mean Nest Density (nests/km)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  

eemp_plot_test <- eemp_data_clean_2 %>%
  select(East.End.Marine.Park, Year, Species, Nest_Density, NNEs_Density) %>%
  pivot_longer(cols = c(Nest_Density, NNEs_Density),
               names_to = "Metric",
               values_to = "Density")


#Scatter Plot/Facet Wrap

ggplot(eemp_data_clean_2, aes(x = Nest_Density, y = NNEs_Density, color = Species)) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~ East.End.Marine.Park) +
  labs(title = "Nest Density vs NNEs Density",
       x = "Nest Density (nests/km)",
       y = "NNEs Density (activities/km)")

ggplot(eemp_data_clean_2, aes(x = Year, y = East.End.Marine.Park, fill = Nest_Density)) +
  geom_tile(na.rm = TRUE) +
  facet_wrap(~ Species) +
  labs(title = "Nest Density Heatmap",
       fill = "Density")


