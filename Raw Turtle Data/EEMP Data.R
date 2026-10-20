
### EEMP Turtle Data from 2019 - 2023

# Set Working Directory, Load libraries and CSV File

library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)

# This file is saved in a sub folder
# Enter this code in the console: 
# setwd("C:\\Users\\romin\\Desktop\\My Thesis\\SQM CSVs")

eemp_data <-read.csv("Raw Turtle Data/EEMP Turtle Data.csv")


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
  filter(Year != 2024) %>%
  mutate(
    Nests           = as.numeric(Nests),
    Total_NNEs      = as.numeric(Total_NNEs),
    Beach.Length.km = as.numeric(Beach.Length.km)) %>%
  mutate(
    Nest_Density = Nests / Beach.Length.km, #apply formula
    NNEs_Density = Total_NNEs / Beach.Length.km,
    pl_density= Probable.Lays / Beach.Length.km,
    dry_density= Dry.Runs / Beach.Length.km,
    tracks_density= Tracks / Beach.Length.km)


eemp_year <-eemp_data_clean_2%>%
  group_by(Location, Species)%>%
  summarise(
    all_nnes=mean(NNEs_Density,na.rm=T), allnests=mean(Nest_Density, na.rm=T), Total_PLs=mean(pl_density,na.rm=T), Total_Dry_Runs=mean(dry_density,na.rm=T), Total_Tracks=mean(tracks_density,na.rm=T))


### Test this out
### Reshape the data to long format
plot_data <- eemp_year %>%
  pivot_longer(
    cols = c(Total_PLs, Total_Dry_Runs, Total_Tracks), 
    names_to = "Activity_Type", 
    values_to = "Count") %>%
  mutate(Combined_Group = paste(Species, "-", Activity_Type))

#Line Plot with multiple colors

ggplot(plot_data, aes(x = Location, y = Count, 
                      color = Combined_Group, 
                      group = Combined_Group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = NULL,
    x = "East End Marine Park",
    y = "Count",
    color = "Species & Activity Type"
  ) +
  theme_minimal() +
  # a high-contrast color palette suitable for 6+ lines
  scale_color_brewer(palette = "Dark2") + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color="black"),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.text.y = element_text(color="black"),
    legend.position = "right")



# Reshape the data to isolate Total_PLs and all_nests into a long format
plot_data_nests_pl <- eemp_year %>%
  pivot_longer(
    cols = c(allnests, Total_PLs), 
    names_to = "Activity_Type", 
    values_to = "Count"
  ) %>%
  # Create a combined label for clear legend coloring
  mutate(Combined_Group = paste(Species, "-", Activity_Type))



### Code for Heat Map Plot

# Code names changed so the script below won't work.

ggplot(plot_data_nests_pl, aes(x = factor(Year), y = Location, fill = Total_PLs)) +
  geom_tile(na.rm = TRUE) +
  scale_fill_viridis_c(na.value = "white") +
  facet_wrap(~ Species) + # CM and EI
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "gray90", fill = NA, size = 0.5),
    strip.background = element_rect(fill = "gray95", color = "gray90"),
    axis.text.y = element_text(color="black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, color="black", hjust = 1)) +
  labs(title = NULL, # this removes the captain
       x = "Year", # this removes the x axis label
       fill = "Nest Density")












