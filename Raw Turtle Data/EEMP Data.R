
### EEMP Turtle Data from 2019 - 2023

# Set Working Directory, Load libraries and CSV File

library(tidyverse)
library(dplyr)

setwd("C:\\Users\\romin\\Desktop\\My Thesis\\SQM CSVs\\Raw Turtle Data")

eemp_data <-read.csv("EEMP Turtle Data.csv")

# Get nest and nnes averages across years for greens and hawksbills
# Formula: Density = Total number of nests or nne's divided by beach km
# clean NA's

beach_density <- eemp_data %>%
  filter(Year >= 2019 & Year <= 2023) %>%
  filter(Species %in% c("CM", "EI"))%>% #use %in% to filter multiple rows
  mutate(
    Nests = as.numeric(Nests),
    NNEs = as.numeric(NNEs),
    Beach.Length.km = as.numeric(Beach.Length.km)) %>%
  group_by(East.End.Marine.Park, Species, Beach.Length.km) %>%
   summarise(
    Avg_Nests_Across_Years = mean(Nests, na.rm = TRUE),
    Avg_NNEs_Across_Years  = mean(NNEs, na.rm = TRUE), 
    .groups = "drop")%>%
  mutate(
    Density_of_Avg_Nests = Avg_Nests_Across_Years / Beach.Length.km,
    Density_of_Avg_NNEs  = Avg_NNEs_Across_Years / Beach.Length.km)
  

# View the final dataset
print(beach_density)

# Create a CSV file
write.csv("Beach_Turtle_Density.csv", row.names = FALSE)

  


