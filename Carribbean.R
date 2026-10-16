# Light Pollution Globe, Carribbean Countries
# Data Source: https://lightpollutionglobe.com/countries/

# Load libraries
library(ggplot2)
library(dplyr)
library(stringr)

caribe <- read.csv("SQM_Caribbean.csv")

# Clean out NA rows and replace percentage as a number

caribe_clean <- caribe %>%
  drop_na(Countries, Sky.Brightness, Milky.Way.Hidden) %>%
  mutate(Milky.Way.Num = as.numeric(str_replace(Milky.Way.Hidden, "%", "")))

# Scale factor to align the two axes

scale_factor <- max(caribe_clean$Milky.Way.Num, na.rm = TRUE) / 
  max(caribe_clean$Sky.Brightness, na.rm = TRUE)

ggplot(data = caribe_clean, aes(x = Countries)) +
  geom_point(aes(y = Sky.Brightness, color = "Sky Brightness"), size = 3) +
  geom_point(aes(y = Milky.Way.Num / scale_factor, color = "Milky Way Hidden (%)"), size = 3) +
  scale_y_continuous(
    name = "Sky Brightness (mag/arcsec²)",
    limits = c(0, 23),
    sec.axis = sec_axis(
      ~ . * scale_factor,
      name = "Milky Way Hidden (%)",
      breaks = seq(0, 100, 10)   # numeric 0–100% scale
    )
  ) +
  scale_color_manual(
    values = c("Sky Brightness" = "#0072B2", "Milky Way Hidden (%)" = "#CC79A7")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color="black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 15), color="black"), 
    axis.title.y.right = element_text(margin = margin(l = 15), color="black"),
    legend.title = element_blank(),
    legend.position = "top"
  ) +
  labs(x = "Caribbean Countries")