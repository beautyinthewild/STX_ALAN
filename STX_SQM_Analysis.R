# Anthropogenic Light / Light Pollution Analysis for St. Croix, USVI

# Load libraries
library(tidyverse)

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
# Scan the variable names (look out for uppercase and lowercase words)

colnames(sqm_west)
colnames(sqm_north)
colnames(sqm_south)
colnames(sqm_east)

# CREATE SECONDARY DATASETS. Make a new data name for all CSVs
# Example: Type str(lw_west) in the console to make sure your data that you
# want to analyze is all numeric.

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
  lw_range=lw_max-lw_min)%>%
  ungroup()

# Top 10 Darkest and Brightest Nesting Beaches 

lw_darkest <-stx_lw_analysis%>%
  slice_max(order_by=lw_avg, n = 10) 

write.csv(lw_darkest,"Top 10 Darkest Beaches.csv")

lw_brightest <-stx_lw_analysis%>%
  slice_min(order_by=lw_avg, n=10)

write.csv(lw_brightest,"Top 10 Brightest Beaches.csv")



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



# Heat maps and scatter plots: Create one for each sector with Landward (LW) 
# averages for each nesting beach.










