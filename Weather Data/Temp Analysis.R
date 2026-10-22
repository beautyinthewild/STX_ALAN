

# Load libraries
library(tidyverse)

# Create a data name.

# Enter this code in the console: 

setwd("C:\\Users\\romin\\Desktop\\My Thesis\\SQM CSVs\\Weather Data")

stx_weather <- read.csv("Climate Data.csv")


weather_clean <-stx_weather %>%
  select(Name, Date, TMAX..Maximum.Temperature.,TMIN..Minimum.Temperature.)
  