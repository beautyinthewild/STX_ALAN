# SQM analysis for South Shore

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