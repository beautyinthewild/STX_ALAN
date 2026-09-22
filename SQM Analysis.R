

#Load libraries
library(tidyverse)
library(tibble)

#Combine files

sqm_westend <-read.csv("STX_Light Pollution Data - West End.csv")
sqm_north <-read.csv("STX_Light Pollution Data - North Shore.csv")
sqm_south <-read.csv("STX_Light Pollution Data - South Shore.csv")
sqm_east <-read.csv("STX_Light Pollution Data - East End.csv")

#Shows you all the column names in the dataset
colnames(sqm_westend)
colnames(sqm_north)
colnames(sqm_south)
colnames(sqm_east)


#SECONDARY DATASETS

lw_west <-sqm_westend%>%
  select(LOCATION,POINTS, SQM..LW., SQM.ZENITH)%>% #select the columns you want
  mutate(REGION="West End")%>% # add a new variable/column
  filter(SQM..LW.!="NEED")%>% #the NEED word changed the value to characters, need to change it to numeric!
  mutate(SQM..LW.=as.numeric(SQM..LW.), SQM.ZENITH=as.numeric(SQM.ZENITH))

#check/type this in console to look at the characters-> str(lw_west)

lw_north <-sqm_north%>%
  select(LOCATION,Points, SQM..LW., SQM.ZENITH)%>%
  mutate(REGION="North Shore")%>%
  rename(POINTS=Points)%>%
  filter(LOCATION!="Brown Bay")

lw_south <-sqm_south%>%
  select(LOCATION,Points, SQM..LW., SQM.ZENITH)%>%
  mutate(REGION="South Shore")%>%
  rename(POINTS=Points)

lw_east <-sqm_east%>%
  select(LOCATION,Points, SQM..LW., SQM.ZENITH)%>%
  mutate(REGION="East End")%>%
  rename(POINTS=Points)

lw_stx <-rbind(lw_west, lw_north, lw_south, lw_east) #combine the datasets, stacked on top of each other

lw_stx<-lw_stx%>%
  filter(LOCATION!="")%>% #removes blanks under location
  mutate(LOCATION=as.factor(LOCATION),REGION=as.factor(REGION))
  
#Find the averages

descri_stats_lw <-lw_stx%>%
  group_by(LOCATION, REGION)%>% #this will create one line per beach
  summarise(points=max(POINTS),
  lw_avg=mean(SQM..LW.,na.rm=T),
  lw_median=median(SQM..LW.,na.rm=T),
  lw_min=min(SQM..LW.),
  lw_max=max(SQM..LW.),
  lw_range=lw_max-lw_min)%>%
  ungroup()

# Top 10 Darkest/Brightest

lw_darkest <-descri_stats_lw%>%
  slice_max(order_by=lw_avg, n = 10) 

lw_brightest <-descri_stats_lw%>%
  slice_min(order_by=lw_avg, n=10)

write.csv(lw_brightest,"Top 10 Brightest.csv")






  
  
  
  
  
  
  


