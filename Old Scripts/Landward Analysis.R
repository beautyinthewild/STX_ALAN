
#Load libraries
library(tidyverse)

# Combine files

sqm_west <-read.csv("STX_ALAN - West End.csv")
sqm_north <-read.csv("STX_ALAN - North Shore.csv")
sqm_south <-read.csv("STX_ALAN - South Shore.csv")
sqm_eemp <-read.csv("STX_ALAN - EEMP.csv")

# Code to create folders in your working directory.

dir.create("Data") # create this folder to keep track of your scripts
dir.create("Plots") # create this folder to drop your plot images

# Always vi your data! Number one coding rule! Examples shown below.

view(sqm_west)
head(sqm_west)
tail(sqm_west)


# This code shows you all the column names in the data set in your console. 
# Scan the variable names (uppercase and lowercase!)

colnames(sqm_west)
colnames(sqm_north)
colnames(sqm_south)
colnames(sqm_east)

# CREATE SECONDARY DATASETS. Make a new data name for all CSVs
# Example: Type str(lw_west) in the console to make sure your data that you
# want to analyze is all numeric.

lw_west <-sqm_west%>%
  select(LOCATION,POINTS, SQM..LW., SQM.ZENITH)%>% #select the columns you want
  mutate(REGION="West End") # add a new variable/column

lw_north <-sqm_north%>%
  select(LOCATION,POINTS, SQM..LW., SQM.ZENITH)%>%
  mutate(REGION="North Shore")
 
lw_south <-sqm_south%>%
  select(LOCATION,POINTS, SQM..LW., SQM.ZENITH)%>%
  mutate(REGION="South Shore")
 
lw_eemp <-sqm_eemp%>%
  select(LOCATION,POINTS, SQM..LW., SQM.ZENITH)%>%
  mutate(REGION="East End")
 

lw_stx <-rbind(lw_west, lw_north, lw_south, lw_eemp) # combine the data sets
# stack them together

lw_stx<-lw_stx%>%
  filter(LOCATION!="")%>% #removes blanks under location good practice!
  mutate(LOCATION=as.factor(LOCATION),REGION=as.factor(REGION))
  
# Find the landward averages for each nesting beach. Take the average of all 
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

# Top 10 Darkest/Brightest

lw_darkest <-stx_lw_analysis%>%
  slice_max(order_by=lw_avg, n = 10) 

write.csv(lw_darkest,"Top 10 Darkest.csv")

lw_brightest <-stx_lw_analysis%>%
  slice_min(order_by=lw_avg, n=10)

write.csv(lw_brightest,"Top 10 Brightest.csv")

# Latest Version: 22 September 2026



  
  
  
  
  
  
  


