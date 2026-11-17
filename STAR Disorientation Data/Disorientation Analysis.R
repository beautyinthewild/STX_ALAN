
### Disorientation Reports from Partners

# Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggh4x)


# Set working directory

setwd("C:\\Users\\romin\\Desktop\\My Thesis\\SQM CSVs\\STAR Disorientation Data")

# Create a data name

stx_disorient <- read.csv("Disorientation Data.csv")

# Organize Property Types Across Years

property_type <-stx_disorient%>%
  select(Year, Property.Type)%>%
  filter(Property.Type!="")

# Organize Locations Across Years

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
    fill = "Development Type") +
  theme_minimal() +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))
  
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
    y = "Number of Disorientation Reports") +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.text = element_text(color = "black"))


### Total Disoriented Hatchlings Across Years 

sum_disoriented <- stx_disorient %>%
  select(Year, Species, Disoriented, Alive) %>%
  filter(Species != "", !is.na(Species)) %>%
  mutate(
    Disoriented = as.numeric(Disoriented),
    Alive = as.numeric(Alive)) %>%
  group_by(Year, Species) %>%
  summarise(
    Total_Disoriented = sum(Disoriented, na.rm = TRUE),
    Total_Alive = sum(Alive, na.rm = TRUE),
    .groups = "drop")

### Bar Chart

sum_disoriented %>%
  pivot_longer(
    cols = c(Total_Disoriented, Total_Alive),
    names_to = "Outcome",
    values_to = "Count")%>%
  ggplot(aes(x = as.factor(Year), y = Count, fill = Outcome)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Species) +
  scale_fill_brewer(
    palette = "Set2",
    labels = c("Total_Alive" = "Found Alive", "Total_Disoriented" = "Disoriented")) +
  labs(
    title = NULL,
    x = NULL,
    y = "Total Affected Hatchlings",
    fill = "Hatchling Outcome") +
  theme_minimal() +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    strip.text = element_text(color = "black", face = "bold"),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))



# Mortality Across Years for Hawksbill and Green Turtle Hatchlings

mortality_years <- stx_disorient %>%
  select(Year, Species, Mortality, Disoriented) %>%
  drop_na(Year, Species, Mortality, Disoriented) %>%
  filter(Year != "", Species != "", Mortality != "", Disoriented != "")

# Cleans up the NA's and UNKs in the rows. 

mortality_clean <- stx_disorient %>%
  select(Year, Species, Mortality, Disoriented, Property.Type)%>%
  drop_na(Year, Species, Mortality, Disoriented, Property.Type)%>%
  filter(
    Year != "", Species != "", Mortality != "", Disoriented != "",
    if_all(c(Year, Species, Mortality, Disoriented), ~ !.x %in% c("UNK", "N/A")))%>%
  mutate(Mortality=as.numeric(Mortality), Disoriented=as.numeric(Disoriented),Species=as.factor(Species),Prop=case_when(Property.Type%in% c("Hotel", "Resort")~"Tourism",TRUE~"Other"))


# Line of best fit - I think I should remove this!

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


### Dual Y axis
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



### Dual Y axis
ggplot(dual_axis_data, aes(x = as.factor(Year), group = Species, color = Species)) +
  geom_point(aes(y = Total_Disoriented, shape = "Disoriented"), size = 4, alpha = 0.8) +
  geom_line(aes(y = Total_Disoriented), linetype = "dashed", alpha = 0.5) +
  geom_point(aes(y = Total_Mortality * scale_factor, shape = "Mortality"), size = 4, alpha = 0.8) +
  geom_line(aes(y = Total_Mortality * scale_factor), linetype = "solid", alpha = 0.5) +
  facet_wrap(~ Species, scales = "free_y") +
  facetted_pos_scales(
    y = list(
      Species == "Cm" ~ scale_y_continuous(
        name = "Total Disoriented Hatchlings",
        limits = c(0, 400),
        breaks = seq(0, 400, by = 100),
        sec.axis = sec_axis(~ . / scale_factor, name = "Total Mortalities")),
      Species == "Ei" ~ scale_y_continuous(
        name = "Total Disoriented Hatchlings",
        limits = c(0, 1000),
        breaks = seq(0, 1000, by = 250),
        sec.axis = sec_axis(~ . / scale_factor, name = "Total Mortalities")))) +
  scale_color_manual(values = c("Cm" = "#009E73", "Ei" = "#E69F00")) +
  labs(
    title = NULL,
    x = NULL,
    shape = "Metric") +
  theme_minimal() +
theme(
  strip.text = element_text(face = "bold", size = 12),
  axis.text = element_text(color = "black"),
  legend.position = "bottom",
  panel.spacing = unit(2, "lines"))



