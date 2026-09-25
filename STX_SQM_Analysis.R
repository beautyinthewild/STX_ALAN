# Anthropogenic Light / Light Pollution Analysis for St. Croix, USVI

# Load libraries
library(tidyverse)
library(viridis)
library(plotrix)

# Combine files. Create a data name for each CSV.

sqm_west <-read.csv("STX_ALAN - West End.csv")
sqm_north <-read.csv("STX_ALAN - North Shore.csv")
sqm_south <-read.csv("STX_ALAN - South Shore.csv")
sqm_eemp <-read.csv("STX_ALAN - EEMP.csv")

# This codes creates folders in your working directory (YouTube).

dir.create("Data") # create this folder to keep track of your scripts
dir.create("Plots") # create this folder to drop your plot images

# Always visualize your data! Number one coding rule! Examples shown below.

view(sqm_west)
head(sqm_west)
tail(sqm_west)

# This code shows you all the column names in the data set in your console. 
# Scan the variable names (look out for uppercase and lowercase words for when
# you code your script).

colnames(sqm_west)
colnames(sqm_north)
colnames(sqm_south)
colnames(sqm_east)

# CREATE SECONDARY DATASETS. Make a new data name for all CSV files.
# Example: Type str(lw_west) in the console to make sure your data that you
# want to analyze is all numeric and not in character or else you have to 
# convert it.

lw_west <-sqm_west%>%
  select(LOCATION,POINTS, SQM..LW.)%>% #select the columns you want
  mutate(REGION="West End") # add a new variable/column

lw_north <-sqm_north%>%
  select(LOCATION,POINTS, SQM..LW.)%>%
  mutate(REGION="North Shore")
 
lw_south <-sqm_south%>%
  select(LOCATION,POINTS, SQM..LW.)%>%
  mutate(REGION="South Shore")
 
lw_eemp <-sqm_eemp%>%
  select(LOCATION,POINTS, SQM..LW.)%>%
  mutate(REGION="East End")
 
# Combine the data sets, stack them together

lw_stx <-rbind(lw_west, lw_north, lw_south, lw_eemp) 

lw_stx<-lw_stx%>%
  filter(LOCATION!="")%>% #removes blanks under location, good practice!
  mutate(LOCATION=as.factor(LOCATION),REGION=as.factor(REGION))
  
# Calculate landward averages for each nesting beach. Take the average of all 
# the points for each survey site.

stx_lw_analysis <-lw_stx%>%
  group_by(LOCATION, REGION)%>% #this will create one line per beach
  summarise(points=max(POINTS),
  lw_avg=mean(SQM..LW.,na.rm=T),
  lw_median=median(SQM..LW.,na.rm=T),
  lw_min=min(SQM..LW.),
  lw_max=max(SQM..LW.),
  lw_range=lw_max-lw_min, 
  lw_se=std.error(SQM..LW.))%>%
  ungroup()

# Top 10 Darkest and Brightest Nesting Beaches 

lw_darkest <-stx_lw_analysis%>%
  slice_max(order_by=lw_avg, n = 10) 

write.csv(lw_darkest,"Top 10 Darkest Beaches.csv")

lw_brightest <-stx_lw_analysis%>%
  slice_min(order_by=lw_avg, n=10)

write.csv(lw_brightest,"Top 10 Brightest Beaches.csv")


# Landward values for the west end.

lw_boxplot<-stx_lw_analysis%>%
  select(LOCATION, REGION, lw_avg, lw_median, lw_min, lw_max, lw_range)%>%
  filter(REGION == "West End")


# Create a plot

ggplot(lw_boxplot, aes(x=LOCATION, y=lw_avg))+
  geom_boxplot(fill = "darkblue") +
  theme_bw()+
  geom_errorbar(aes(ymin = lw_avg - lw_median, ymax = lw_avg + lw_median), 
               width = 0.2) + 
  ylim(0, 25) + 
  labs(
    x = "West End Nesting Beaches",
    y = "Light Pollution (mag/arcsecs)") +
  guides(fill=FALSE)

  

ggplot(lw_boxplot, aes(x = LOCATION, y = lw_avg)) +
  geom_point(color = "black", size = 2) +
  theme_bw() +
  ylim(0, 25) + 
  labs(
    x = "West End Nesting Beaches",
    y = "Average Light Pollution (mag/arcsecs)"
  ) +
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(lw_boxplot, aes(x = LOCATION, y = lw_avg, color = LOCATION)) +
  geom_point(size = 3) +
  scale_color_viridis_d(option = "viridis") + # clean Viridis color scheme
  theme_bw() +
  ylim(0, 25) + # I changed the SQM to 0-25 mag/arcsec2
  labs(
    x = "West End Nesting Beaches",
    y = "Night Sky Brightness (mag/arcsecs)"
  ) +
  guides(color = FALSE) + # Hides the redundant legend to maximize your plot area
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Merge the first two datasets by LOCATION, then merge the third
sqm_west_combo <-avg_landward%>%
  left_join(avg_seaward%>%select())


# Data analysis for SQM Zenith Skies 

zenith_west <-sqm_west%>%
  select(LOCATION, POINTS, SQM.ZENITH)%>% #select the columns you want 
  mutate(REGION="West End")
  
zenith_north <-sqm_north%>%
  select(LOCATION, POINTS, SQM.ZENITH)%>% 
  mutate(REGION="North Shore")

zenith_south <-sqm_south%>%
  select(LOCATION, POINTS, SQM.ZENITH)%>%
  mutate(REGION="South Shore")

zenith_eemp <-sqm_eemp%>%
  select(LOCATION, POINTS, SQM.ZENITH)%>%
  mutate(REGION="EEMP")
  
# Combine the data sets/ stack them together
  
zenith_stx <-rbind(zenith_west, zenith_north, zenith_south, zenith_eemp) 

zenith_stx_clean<-zenith_stx%>%
  filter(LOCATION!="")%>% #removes NA's 
  filter(SQM.ZENITH!="")%>% #removes NA's 
  mutate(LOCATION=as.factor(LOCATION),REGION=as.factor(REGION))

# Calculate landward averages for each nesting beach. Take the average of all 
# the points for each survey site.

stx_zenith_analysis <-zenith_stx_clean%>%
  group_by(LOCATION, REGION)%>% #this will create one line per beach
  summarise(points=max(POINTS),
    zenith_avg=mean(SQM.ZENITH,na.rm=T),
    zenith_median=median(SQM.ZENITH,na.rm=T),
    zenith_min=min(SQM.ZENITH),
    zenith_max=max(SQM.ZENITH),
    zenith_range=zenith_max-zenith_min)%>%
  ungroup()


# Top 10 Darkest and Brightest Zenith Skies

zenith_darkest <-stx_zenith_analysis%>%
  slice_max(order_by=zenith_avg, n = 10) 

write.csv(zenith_darkest,"Top 10 Darkest Zenith Skies.csv")

zenith_brightest <-stx_zenith_analysis%>%
  slice_min(order_by=zenith_avg, n=10)

write.csv(lw_brightest,"Top 10 Brightest Zenith Skies.csv")


# Data analysis for SQM Seaward Horizon 

seaward_west <-sqm_west%>%
  select(LOCATION, POINTS, SQM..SW.)%>%
  mutate(REGION="West End")
  
seaward_north <-sqm_north%>%
  select(LOCATION, POINTS, SQM..SW.)%>%
  mutate(REGION="North Shore")

seaward_south <-sqm_south%>%
  select(LOCATION, POINTS, SQM..SW.)%>%
  mutate(REGION="South Shore")

seaward_eemp <-sqm_eemp%>%
  select(LOCATION, POINTS, SQM..SW.)%>%
  mutate(REGION="EEMP")

# Combine the data sets/ stack them together

seaward_stx <-rbind(seaward_west, seaward_north, seaward_south, seaward_eemp)

# Remove any NA's in the dataset

seaward_stx_clean<-seaward_stx%>%
  filter(LOCATION!="")%>% #removes NA's 
  filter(SQM..SW.!="")%>% #removes NA's 
  mutate(LOCATION=as.factor(LOCATION),REGION=as.factor(REGION))


stx_seaward_analysis <-seaward_stx_clean%>%
  group_by(LOCATION, REGION)%>% #this will create one line per beach
  summarise(points=max(POINTS),
            seaward_avg=mean(SQM..SW.,na.rm=T),
            seaward_median=median(SQM..SW.,na.rm=T),
            seaward_min=min(SQM..SW.),
            seaward_max=max(SQM..SW.),
            seaward_range=seaward_max-seaward_min)%>%
            ungroup()

# Top 10 Darkest and Brightest Seaward Skies

seaward_darkest <-stx_seaward_analysis%>%
  slice_max(order_by=seaward_avg, n = 10) 

write.csv(seaward_darkest,"Top 10 Darkest Seaward Skies.csv")

seaward_brightest <-stx_seaward_analysis%>%
  slice_min(order_by=seaward_avg, n=10)

write.csv(seaward_brightest,"Top 10 Brightest Seaward Skies.csv")



# Combine landward, seaward and zenith averages to make a plot.

avg_landward <-stx_lw_analysis%>%
  select(LOCATION,REGION,lw_avg)

avg_seaward <-stx_seaward_analysis%>%
  select(LOCATION,REGION, seaward_avg)

avg_zenith<-stx_zenith_analysis%>%
  select(LOCATION,REGION, zenith_avg)

# Merge the data sets together 

combined_averages <- avg_landward %>%
  left_join(avg_seaward, by = c("LOCATION", "REGION")) %>%
  left_join(avg_zenith, by = c("LOCATION", "REGION"))

sqm_averages_west <-combined_averages%>%
  filter(REGION == "West End")
  
# Make a scatterplot for the West End

ggplot(sqm_averages_west, aes(x = LOCATION, y = lw_avg, color = LOCATION)) +
  geom_point(size = 4) +
  scale_color_viridis_d(option = "viridis", direction=-1) + # clean Viridis color scheme
  theme_bw() +
  ylim(10, 25) + # I changed the SQM to 0-25 mag/arcsec2
  labs(
    x = "West End Beaches",
    y = "Night Sky Brightness (mag/arcsecs)"
  ) +
  guides(color = FALSE) + # Hides the redundant legend to maximize your plot area
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Scatter plot with lw, sw and zenith points for Frederiksted's beaches


plot_data_west <- sqm_averages_west %>%
  pivot_longer(
    cols = c(lw_avg, seaward_avg, zenith_avg), 
    names_to = "Measurement_Type", 
    values_to = "Brightness")


ggplot(plot_data_west, aes(x = LOCATION, y = Brightness, color = Measurement_Type)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c(
      "lw_avg"      = "#FF5B00",  # orange for landward
      "seaward_avg" = "#0055FF",  # blue for seaward
      "zenith_avg"  = "#5E35B1"   # purple for zenith
    ),
    labels = c("Landward Horizon", "Seaward Horizon", "Zenith Horizon")
  ) +
  labs(
    x = "West End",
    y = "Brightness (mag/arcsec²)",
    color = NULL  # Removes the legend title box header for a cleaner look
  ) +
  ylim(10, 25) +
    theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Brightness Scale for West-End Beaches (includes lw, sw and zenith)

ggplot(plot_data_west, aes(x = LOCATION, y = Brightness, color = Brightness, shape=Measurement_Type)) +
  geom_point(size = 3.5, alpha = 0.9) +
  scale_color_viridis_c(name= "Brightness Scale", option = "viridis", direction = -1) + 
  theme_bw() +
  ylim(10, 25) + 
  labs(
    x = "West-end Beaches",
    y = "Brightness (mag/arcsecs)",
    color = "Brightness",
    shape = "Horizon Measurements",
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color="black"))


# Boxplot for each sector using landward values

ggplot(stx_lw_analysis, aes(x=REGION, y=lw_avg, color=REGION))+
  geom_boxplot()+
  geom_jitter(width=.1, alpha=.5)+
  theme_bw() +
  ylim(10, 25) + 
  labs(
    x = "",
    y = "Night Sky Brightness (mag/arcsecs)"
  ) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, face="bold")
  )+
  guides(color="none")



# Rearrange regions with code: West End, North Shore, South Shore, East End

stx_lw_analysis$REGION <- factor(stx_lw_analysis$REGION, 
                                 levels = c("West End", "North Shore", "South Shore", "East End"))


# Boxplot for each sector displayed

ggplot(stx_lw_analysis, aes(x=REGION, y=lw_avg, color=REGION))+
  geom_boxplot()+
  geom_jitter(shape = 21, fill="white", stroke = 1.5, width = .1, size = 1.8)+
  theme_bw() +
  ylim(10, 25) + 
  labs(
    x = "",
    y = "Night Sky Brightness (mag/arcsecs)"
  ) +
  theme(
    text = element_text(family = "Serif", size = 13, color="black"),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, color="black"),
    axis.text.y = element_text(color = "black"),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  guides(color="none")














