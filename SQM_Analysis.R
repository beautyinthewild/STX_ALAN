# Anthropogenic Light / Light Pollution Analysis for St. Croix, USVI

# Load libraries
library(tidyverse)
library(viridis)
library(plotrix)

# Combine files. Create a data name for each CSV.

SQM_west <-read.csv("SQM West End.csv")
SQM_north <-read.csv("SQM North Shore.csv")
SQM_south <-read.csv("SQM South Shore.csv")
SQM_eemp <-read.csv("SQM EEMP.csv")

# This codes creates folders in your working directory (YouTube).

dir.create("Data") # create this folder to keep track of your scripts
dir.create("Plots") # create this folder to drop your plot images

# Always visualize your data! Number one coding rule! Examples shown below.

view(SQM_west)
glimpse(SQM_west)
head(SQM_west)
tail(SQM_west)

# This code shows you all the column names in the data set in your console. 
# Scan the variable names (look out for uppercase and lowercase words for when
# you code your script).

colnames(SQM_west)
colnames(SQM_north)
colnames(SQM_south)
colnames(SQM_east)

# CREATE SECONDARY DATASETS. Make a new data name for all CSV files.
# Example: Type str(lw_west) in the console to make sure your data that you
# want to analyze is all numeric and not in character or else you have to 
# convert it.

lw_west <-SQM_west%>%
  select(STANDARD.NAME, LOCATION, POINTS, SQM..LW.)%>% #select columns you want
  mutate(REGION="West End") # add a new variable/column

lw_west_2 <-SQM_west%>%
  select(STANDARD.NAME, POINTS, SQM..LW.)%>% #select columns you want
  mutate(REGION="West End") # add a new variable/column

lw_north <-SQM_north%>%
  select(STANDARD.NAME, LOCATION, POINTS, SQM..LW.)%>%
  mutate(REGION="North Shore")

lw_south <-SQM_south%>%
  select(STANDARD.NAME, LOCATION, POINTS, SQM..LW.)%>%
  mutate(REGION="South Shore")

lw_eemp <-SQM_eemp%>%
  select(STANDARD.NAME, LOCATION, POINTS, SQM..LW.)%>%
  mutate(REGION="East End")

# Combine the landward values from each data set, stack them together

lw_stx <-rbind(lw_west, lw_north, lw_south, lw_eemp) 

lw_stx <-lw_stx%>%
  filter(LOCATION!="")#removes blanks under location

# Calculate landward averages for each nesting beach. Take the average of all 
# the points for each survey site.

# LW Analysis St. Croix

stx_lw_analysis <-lw_stx%>%
  group_by(LOCATION, REGION)%>%
  summarise(points=max(POINTS),
            lw_avg=mean(SQM..LW.,na.rm=T),
            lw_median=median(SQM..LW.,na.rm=T),
            lw_min=min(SQM..LW.),
            lw_max=max(SQM..LW.),
            lw_range=lw_max-lw_min, 
            lw_se=std.error(SQM..LW.),
            STANDARD.NAME=first(STANDARD.NAME))%>% #keeps the names the same
  ungroup()


### Top 10 Darkest Beaches - Local Beach Names

lw_darkest <-stx_lw_analysis%>%
  slice_max(order_by=lw_avg, n = 10) 

write.csv(lw_darkest,"LW Darkest Beaches_Standard Names.csv")

### Top 10 Brightest Beaches - Local Beach Names

lw_brightest <-stx_lw_analysis%>%
  slice_min(order_by=lw_avg, n=10)

write.csv(lw_brightest,"LW Brightest Beaches_Standard Names.csv")


# Landward values for the west end.

lw_boxplot<-stx_lw_analysis%>%
  select(LOCATION, REGION, lw_avg, lw_median, lw_min, lw_max, lw_range)%>%
  filter(REGION == "West End")


# Scatter plot of nesting beaches in the West End
## From highest to lowest brightness

ggplot(lw_boxplot, aes(x = reorder(LOCATION, -lw_avg), y = lw_avg, color = LOCATION)) + 
  geom_point(color = "black", size = 3.5) +
  theme_bw() +
  ylim(0, 25) + 
  labs(
    x = "West End Beaches",
    y = "Landward Brightness (mag/arcsecs)"
  ) +
  guides(fill = FALSE)+
  theme( 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 10)))

# Scatter Plot - Beaches in alphabetical order

ggplot(lw_boxplot, aes(x =LOCATION, y = lw_avg, color = LOCATION)) + 
  geom_point(color = "black", size = 3.5) +
  theme_bw() +
  ylim(0, 25) + 
  labs(
    x = "West End Beaches",
    y = "Landward Brightness (mag/arcsecs)"
  ) +
  guides(fill = FALSE)+
  theme( 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 10)))


### Data analysis for Zenith Skies 

zenith_west <-SQM_west%>%
  select(STANDARD.NAME, LOCATION, POINTS, SQM.ZENITH)%>% #select the columns you want 
  mutate(REGION="West End")

zenith_north <-SQM_north%>%
  select(STANDARD.NAME, LOCATION, POINTS, SQM.ZENITH)%>% 
  mutate(REGION="North Shore")

zenith_south <-SQM_south%>%
  select(STANDARD.NAME, LOCATION, POINTS, SQM.ZENITH)%>%
  mutate(REGION="South Shore")

zenith_eemp <-SQM_eemp%>%
  select(STANDARD.NAME, LOCATION, POINTS, SQM.ZENITH)%>%
  mutate(REGION="East End")

# Combine the data sets/ stack them together

zenith_stx <-rbind(zenith_west, zenith_north, zenith_south, zenith_eemp) 

zenith_stx_clean<-zenith_stx%>%
  drop_na(LOCATION, POINTS, SQM.ZENITH, REGION)

# Calculate zenith averages for each nesting beach. Take the average of all 
# the points for each site.

stx_zenith_analysis <-zenith_stx_clean%>%
  group_by(LOCATION, REGION)%>% #this will create one line per beach
  summarise(points=max(POINTS),
            zenith_avg=mean(SQM.ZENITH,na.rm=T),
            zenith_median=median(SQM.ZENITH,na.rm=T),
            zenith_min=min(SQM.ZENITH),
            zenith_max=max(SQM.ZENITH),
            zenith_range=zenith_max-zenith_min,
            STANDARD.NAME=first(STANDARD.NAME))%>%
  ungroup()


### Top 10 Darkest Zenith Skies - Local Names

zenith_darkest <-stx_zenith_analysis%>%
  slice_max(order_by=zenith_avg, n = 10) 

write.csv(zenith_darkest,"Zenith Darkest Beaches_Local Names.csv")

### Top 10 Brightest Zenith Skies - Local Names

zenith_brightest <-stx_zenith_analysis%>%
  slice_min(order_by=zenith_avg, n=10)

write.csv(lw_brightest,"Zenith Brightest Skies_Local Names.csv")

# Zenith Values for St. Croix

zenith_boxplot<-stx_zenith_analysis%>%
  select(LOCATION, REGION, zenith_avg, zenith_median, zenith_min, zenith_max, zenith_range)


### Zenith Plot for St. Croix Beaches

ggplot(zenith_boxplot, aes(x = reorder(LOCATION, -zenith_avg), y = zenith_avg, color = LOCATION)) + 
  geom_point(color = "black", size = 3.5) +
  theme_bw() +
  ylim(0, 25) + 
  labs(
    x = "Nesting Beaches",
    y = "Zenith Brightness (mag/arcsecs)"
  ) +
  guides(fill = FALSE)+
  theme(
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
axis.title.y = element_text(margin = margin(r = 10), color = "black"))


### Cleveland Dot Plot for Zenith Averages

ggplot(zenith_boxplot, aes(x = reorder(LOCATION, -zenith_avg), y = zenith_avg)) + 
  geom_segment(
    aes(xend = reorder(LOCATION, -zenith_avg), yend = 0), 
    color = "grey75",   
    linewidth = 0.6     # thin line weight
  ) +
    geom_point(color = "black", size = 3.5) + # data points
  theme_bw() +
  ylim(0, 25) + 
  labs(
    x = "Sea Turtle Beaches",
    y = "Zenith Brightness (mag/arcsecs)"
  ) +
  theme( 
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


### Data analysis for Seaward Horizon 

seaward_west <-SQM_west%>%
  select(STANDARD.NAME, LOCATION, POINTS, SQM..SW.)%>%
  mutate(REGION="West End")

seaward_north <-SQM_north%>%
  select(STANDARD.NAME, LOCATION, POINTS, SQM..SW.)%>%
  mutate(REGION="North Shore")

seaward_south <-SQM_south%>%
  select(STANDARD.NAME, LOCATION, POINTS, SQM..SW.)%>%
  mutate(REGION="South Shore")

seaward_eemp <-SQM_eemp%>%
  select(STANDARD.NAME, LOCATION, POINTS, SQM..SW.)%>%
  mutate(REGION="East End")

# Combine the data sets/ stack them together

seaward_stx <-rbind(seaward_west, seaward_north, seaward_south, seaward_eemp)

# Remove NA's in the dataset

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
            seaward_range=seaward_max-seaward_min,
  STANDARD.NAME=first(STANDARD.NAME))%>%
  ungroup()


### Top 10 Darkest Seaward Skies - Local Names

seaward_darkest <-stx_seaward_analysis%>%
  slice_max(order_by=seaward_avg, n = 10) 

write.csv(seaward_darkest,"Darkest Seaward Skies_Local Names.csv")

### Top 10 Brightest Seaward Skies - Local Names

seaward_brightest <-stx_seaward_analysis%>%
  slice_min(order_by=seaward_avg, n=10)

write.csv(seaward_brightest,"Brightest Seaward Skies_Local Names.csv")

# Seaward values for St. Croix

seaward_boxplot<-stx_seaward_analysis%>%
  select(LOCATION, REGION, seaward_avg, seaward_median, seaward_min, seaward_max, seaward_range)

### Cleveland Dot Plot Seaward Averages

ggplot(seaward_boxplot, aes(x = reorder(LOCATION, -seaward_avg), y = seaward_avg, color = LOCATION)) + 
  geom_segment(
    aes(xend = reorder(LOCATION, -seaward_avg), yend = 0), 
    color = "grey75",   # Muted grey so it doesn't distract from the dots
    linewidth = 0.6     # Clean, thin line weight
  ) +
  geom_point(color = "black", size = 3.5) +
  theme_bw() +
  ylim(0, 25) + 
  labs(
    x = "Sea Turtle Habitats",
    y = "Seaward Brightness (mag/arcsecs)"
  ) +
  guides(fill = FALSE)+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), color = "black"))


#### Combine Landward, Seaward and Zenith Averages to make a Scatter Plot.

avg_landward <-stx_lw_analysis%>%
  select(LOCATION,REGION,lw_avg)

avg_seaward <-stx_seaward_analysis%>%
  select(LOCATION,REGION, seaward_avg)

avg_zenith <-stx_zenith_analysis%>%
  select(LOCATION,REGION, zenith_avg)

# Merge the average data sets together 

combined_averages <- avg_zenith %>%
  left_join(avg_seaward, by = c("LOCATION", "REGION")) %>%
  left_join(avg_landward, by = c("LOCATION", "REGION"))


write.csv(combined_averages,"Clean Light Data.csv")


### Combine averages for landward, seaward and zenith in a plot
### Reshape the combined data into long format to do this

combined_long <- combined_averages %>%
  pivot_longer(
    cols = c(lw_avg, seaward_avg, zenith_avg), 
    names_to = "Measurement_Type", 
    values_to = "Brightness_Value")

### Vertical Plot

ggplot(combined_long, aes(x = reorder(LOCATION, -Brightness_Value), y = Brightness_Value, color = Measurement_Type)) + 
  geom_point(size = 4, alpha = 0.8) +
  theme_bw() +
  ylim(10, 25) + 
  labs(
    x = "Sea Turtle Habitats",
    y = "Nighttime Sky Brightness (mag/arcsec²)",
    color = "Measurement Direction"
  ) +
  scale_color_manual(
    values = c("lw_avg" = "#E69F00", "seaward_avg" = "#56B4E9", "zenith_avg" = "#000000"),
    labels = c("Landward Avg", "Seaward Avg", "Zenith Avg")
  ) + 
  coord_flip() + # <-- This flips the X and Y axes
  theme(
    axis.text.y = element_text(color = "black", size = 8),
    axis.title.x = element_text(margin = margin(t = 13)), 
    legend.position = "right")


### Combination for West End

sqm_averages_west <-combined_averages%>%
  filter(REGION == "West End")

# Simple scatter plot for the West End

ggplot(sqm_averages_west, aes(x = LOCATION, y = lw_avg, color = LOCATION)) +
  geom_point(size = 4, color="black") +
  scale_color_viridis_d(option = "viridis", direction=-1) + # clean Viridis color scheme
  theme_bw() +
  ylim(10, 25) + # I changed the SQM to 0-25 mag/arcsec2
  labs(
    x = "West End",
    y = "Brightness (mag/arcsec²)"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color="black"))


# Combined directions for Frederiksted's Beaches

plot_data_west <- sqm_averages_west %>%
  pivot_longer(
    cols = c(lw_avg, seaward_avg, zenith_avg), 
    names_to = "Measurement_Type", 
    values_to = "Brightness")


  ggplot(plot_data_west, aes(x = reorder(LOCATION, -Brightness), y = Brightness, fill = Measurement_Type)) + 
  geom_point(shape = 21, color = "black", size = 5, alpha = 0.8, stroke = 1.2) + 
  scale_fill_manual(
    values = c(
      "lw_avg"      = "#FFEA46",  # orange for landward
      "seaward_avg" = "#D55E00",  # blue for seaward
      "zenith_avg"  = "#00204D"   # purple for zenith
    ),
    labels = c("Landward Horizon", "Seaward Horizon", "Zenith")
  ) +
  labs(
    x = "West End Habitats",
    y = "Brightness (mag/arcsec²)",
    fill = NULL  # Removes the legend title box header for a cleaner look
  ) +
  ylim(10, 25) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 12)))


# Brightness Scale for West-End Beaches (Combination LW, SW and Zenith)

ggplot(plot_data_west, aes(x = LOCATION, y = Brightness, color = Brightness, shape=Measurement_Type)) +
  geom_point(size = 5, alpha = 0.9) +
  scale_color_viridis_c(name= "Sky Brightness Units", option = "viridis", direction = -1) + 
  theme_bw() +
  ylim(10, 25) + 
  labs(
    x = "West End Beaches",
    y = "Brightness (mag/arcsecs)",
    color = "Brightness",
    shape = "Measurement Direction",
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color="black"))



### Combination for North Shore

sqm_averages_north <-combined_averages%>%
  filter(REGION == "North Shore")

# Combined directions for North Shore. Reshape the data. 

plot_data_north <- sqm_averages_north %>%
  pivot_longer(
    cols = c(lw_avg, seaward_avg, zenith_avg), 
    names_to = "Measurement_Type", 
    values_to = "Brightness")

###Scale for North Shore Beaches (Combination LW, SW and Zenith)


ggplot(plot_data_north, aes(x = reorder(LOCATION, -Brightness), y = Brightness, color = Measurement_Type, shape = Measurement_Type)) + 
  geom_point(size = 4, alpha = 0.8) +
  theme_bw() +
  labs(
    x = "North Shore Beaches",
    y = "Brightness (mag/arcsec²)",
    color = "Measurement Type",
    shape = "Measurement Type"
  ) +
  # Using the colorblind-friendly palette 
  scale_color_manual(
    values = c("lw_avg" = "#E69F00", "seaward_avg" = "#56B4E9", "zenith_avg" = "#000000"),
    labels = c("Landward Avg", "Seaward Avg", "Zenith Avg")
  ) + 
  scale_shape_manual(
    values = c("lw_avg" = 16, "seaward_avg" = 17, "zenith_avg" = 15), # Circle, Triangle, Square
    labels = c("Landward Avg", "Seaward Avg", "Zenith Avg")
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(color = "black", size = 9),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color="black"),
    legend.position = "right"
  )

###Brightness Scale for North Shore Beaches

ggplot(plot_data_north, aes(x = reorder(LOCATION, -Brightness), y = Brightness, color = Brightness, shape = Measurement_Type)) + 
  geom_point(size = 5.5, alpha = 0.8) +
  theme_bw() +
  labs(
    x = "North Shore Beaches",
    y = "Brightness (mag/arcsec²)",
    color = "Brightness",
    shape = "Measurement Direction") +
  # Viridis continuous color scale
  scale_color_viridis_c(name = "Sky Brightness Units", option = "viridis", direction= -1) +
  scale_shape_manual(
    values = c("lw_avg" = 16, "seaward_avg" = 17, "zenith_avg" = 15),
    labels = c("Landward Avg", "Seaward Avg", "Zenith Avg")) +
theme(
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(color = "black", size = 9),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color= "black"),
    axis.title.x = element_text(margin = margin(t = 15)),
    legend.title = element_text(hjust = 0.5),
    legend.position = "right")


### Combination for South Shore

sqm_averages_south <-combined_averages%>%
  filter(REGION == "South Shore")

# Combined directions for North Shore. Reshape the data. 

plot_data_south <- sqm_averages_south %>%
  pivot_longer(
    cols = c(lw_avg, seaward_avg, zenith_avg), 
    names_to = "Measurement_Type", 
    values_to = "Brightness")


###Brightness Scale for South Shore Beaches


ggplot(plot_data_south, aes(x = reorder(LOCATION, -Brightness), y = Brightness, color = Brightness, shape = Measurement_Type)) + 
  geom_point(size = 5, alpha = 0.8) +
  theme_bw() +
  labs(
    x = "South Shore Beaches",
    y = "Brightness (mag/arcsec²)",
    color = "Brightness",
    shape = "Measurement Direction") +
  # Viridis continuous color scale
  scale_color_viridis_c(name = "Sky Brightness Units", option = "viridis", direction= -1) +
  scale_shape_manual(
    values = c("lw_avg" = 16, "seaward_avg" = 17, "zenith_avg" = 15),
    labels = c("Landward Avg", "Seaward Avg", "Zenith Avg")) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(color = "black", size = 9),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color= "black"),
    axis.title.x = element_text(margin = margin(t = 15)),
    legend.title = element_text(hjust = 0.5),
    legend.position = "right")



###Brightness Scale for East End Marine Park

### Combination for EEMP

sqm_averages_eemp <-combined_averages%>%
  filter(REGION == "East End")

# Combined directions for North Shore. Reshape the data. 

plot_data_eemp <- sqm_averages_eemp %>%
  pivot_longer(
    cols = c(lw_avg, seaward_avg, zenith_avg), 
    names_to = "Measurement_Type", 
    values_to = "Brightness")


ggplot(plot_data_eemp, aes(x = reorder(LOCATION, -Brightness), y = Brightness, color = Brightness, shape = Measurement_Type)) + 
  geom_point(size = 5.5, alpha = 0.8) +
  theme_bw() +
  labs(
    x = "East End Marine Park",
    y = "Brightness (mag/arcsec²)",
    color = "Brightness",
    shape = "Measurement Direction") +
  # Viridis continuous color scale
  scale_color_viridis_c(name = "Sky Brightness Units", option = "viridis", direction= -1) +
  scale_shape_manual(
    values = c("lw_avg" = 16, "seaward_avg" = 17, "zenith_avg" = 15),
    labels = c("Landward Avg", "Seaward Avg", "Zenith Avg")) +
  ylim(15,25)+
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(color = "black", size = 9),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color= "black"),
    axis.title.x = element_text(margin = margin(t = 15)),
    legend.title = element_text(hjust = 0.5),
    legend.position = "right")



### Rearrange regions with code: West, North, South, East 

stx_lw_analysis$REGION <- factor(stx_lw_analysis$REGION, 
                                 levels = c("West End", "North Shore", "South Shore", "East End"))


# Boxplot for each region displayed

ggplot(stx_lw_analysis, aes(x=REGION, y=lw_avg, color=REGION))+
  geom_boxplot()+
  geom_jitter(shape = 21, fill="white", stroke = 1.5, width = .1, size = 1.8)+
  theme_bw() +
  ylim(10, 25) + 
  labs(
    x = "",
    y = "Landward Brightness (mag/arcsecs)"
  ) +
  theme(
    text = element_text(family = "Serif", size = 13, color="black"),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, color="black"),
    axis.text.y = element_text(color = "black"),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  guides(color="none")


