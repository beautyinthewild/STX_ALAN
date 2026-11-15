
# Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)


# Set working directory

setwd("C:\\Users\\romin\\Desktop\\My Thesis\\SQM CSVs\\STAR Disorientation Data")

# Create a data name

stx_disorient <- read.csv("STAR_Disorientation Data.csv")

# Organize Property Types Across Years

property_type <-stx_disorient%>%
  select(Year, Property.Type)%>%
  filter(Property.Type!="")

# Organize Locations Across Yeats

location_reports <-stx_disorient%>%
  select(Year, Location)%>%
  filter(Location!="")%>%
  drop_na()

### Bar chart tallies property types across years
# Charts shows that disorientations occur more at DSPR and Resorts

ggplot(property_type, aes(x = as.factor(Year), fill = Property.Type)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set2") + #colorblind-safe colors
  labs(
    title = NULL,
    x = NULL,
    y = "Number of Disorientation Reports",
    fill = "Development Type"
  ) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))
  theme_minimal()
  
  
### Top 5 Locations with the highest disorientation reports
### Out of the 5 Locations 4 of them are in the West-End
  
top_locations <- stx_disorient %>%
    mutate(Location = trimws(Location)) %>%
    filter(!is.na(Location), Location != "", Location != "N/A") %>%
    count(Location, sort = TRUE) %>%
    slice_max(n, n = 5)


ggplot(top_locations, aes(x = reorder(Location, -n), y = n, fill = Location)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(option = "D") + #color blind safe colors
  labs(
    title = NULL,
    x = NULL,
    y = "Number of Disorientation Reports"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.text = element_text(color = "black"))


### Clean the data for the number of disoriented hatchlings 

sum_dm <- stx_disorient %>%
  select(Year, Species, Sum.DM) %>%
  filter(
    if_all(
      everything(),
      ~ !is.na(.) & 
        trimws(.) != "" & 
        !toupper(trimws(.)) %in% c("UNK", "N/A", "NA"))) %>%
  mutate(Sum.DM = as.numeric(Sum.DM)) # Converts Sum.DM from text to numbers


### Summarize total disorientations for Cm and Ei 
sum_dm_totals <- sum_dm %>%
  group_by(Year, Species) %>%
  summarise(Total_DM = sum(Sum.DM, na.rm = TRUE), .groups = "drop")

### Bar Chart
ggplot(sum_dm_totals, aes(x = factor(Year), y = Total_DM, fill = Species)) +
  geom_col(position = position_dodge2(width = 0.8, preserve = "single"), width = 0.7) +
  scale_fill_manual(
    values = c(
      "Cm" = "#009E73", # Colorblind-safe bluish green
      "Ei" = "#D55E00"  # Colorblind-safe orange
    ),
    labels = c("Cm" = "Green (Cm)", "Ei" = "Hawksbill (Ei)")) +
  labs(
    x = NULL,
    y = "Total Affected Hatchlings",
    fill = "Species") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.text = element_text(color = "black"))


# Mortality Across Years for Hawksbill and Green Turtle Hatchlings

mortality_years <- stx_disorient %>%
  select(Year, Species, Mortality, Disoriented) %>%
  drop_na(Year, Species, Mortality, Disoriented) %>%
  filter(Year != "", Species != "", Mortality != "", Disoriented != "")

# Cleans up the NA's and UNKs in the rows. 

mortality_clean <- stx_disorient %>%
  select(Year, Species, Mortality, Disoriented, Property.Type, Incident.Documented) %>%
  drop_na(Year, Species, Mortality, Disoriented, Property.Type,  Incident.Documented) %>%
  filter(
    Year != "", Species != "", Mortality != "", Disoriented != "",
    if_all(c(Year, Species, Mortality, Disoriented), ~ !.x %in% c("UNK", "N/A")))%>%
  mutate(Mortality=as.numeric(Mortality), Disoriented=as.numeric(Disoriented),Species=as.factor(Species),Prop=case_when(Property.Type%in% c("Hotel", "Resort")~"Tourism",TRUE~"Other"))


# Line of best fit

ggplot(mortality_clean,aes(x=Disoriented, y=Mortality, color=Species))+
  geom_point()+
  geom_smooth(method = "lm")

# Facet Wrap

ggplot(mortality_clean,aes(x=Disoriented, y=Mortality, color=Species))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Property.Type)

#New Variable
ggplot(mortality_clean,aes(x=Disoriented, y=Mortality, color=Species))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Prop)


### Summarize total counts per year and species
dual_axis_data <- mortality_clean %>%
  mutate(
    Mortality = as.numeric(Mortality),
    Disoriented = as.numeric(Disoriented)) %>%
  group_by(Year, Species) %>%
  summarise(
    Total_Mortality = sum(Mortality, na.rm = TRUE),
    Total_Disoriented = sum(Disoriented, na.rm = TRUE),
    .groups = "drop")

# Define our scale multiplier factor
scale_factor <- 3

# Dual Y axis
ggplot(dual_axis_data, aes(x = as.factor(Year), group = Species, color = Species)) +
  geom_point(aes(y = Total_Disoriented, shape = "Disoriented"), size = 4, alpha = 0.8) +
  geom_line(aes(y = Total_Disoriented), linetype = "dashed", alpha = 0.5) +
  geom_point(aes(y = Total_Mortality * scale_factor, shape = "Mortality"), size = 4, alpha = 0.8) +
  geom_line(aes(y = Total_Mortality * scale_factor), linetype = "solid", alpha = 0.5) +
  facet_wrap(~ Species) +
  scale_y_continuous(
    name = "Total Disoriented Hatchlings",
    sec.axis = sec_axis(~ . / scale_factor, name = "Total Mortalities")) +
  scale_color_manual(values = c("Cm" = "#009E73", "Ei" = "#E69F00")) +
  labs(
    title = NULL,
    x = NULL,
    shape = "Metric") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom")






