
### Clean Light Data 

# Load libraries
library(tidyverse)

# Create a data name.

stx_alan <-read.csv("Clean Light Data.csv")

# I have 46 observations (k-1) this leaves me with 45 degrees of freedom. 
# Every single points has its own unique location. 
# This leaves me with 0 degrees of freedom for the Residuals (errors).
# My analysis is broken.


###Perform one-way Anova using Landward values!

alan_anova <-aov(lw_avg ~ LOCATION, data=stx_alan)
summary(alan_anova)

# Extract standardized residuals
alan_residuals <-rstandard(alan_anova)

#Test normality
shapiro.test(alan_residuals)

# My Shapiro test failed. 


### Group locations into groups (west, north, south, east)


# Perform one-way ANOVA by REGION instead of LOCATION
alan_anova_region <- aov(lw_avg ~ REGION, data = stx_alan)
summary(alan_anova_region)

#Extract standardized residuals and test normality
alan_residuals_region <- rstandard(alan_anova_region)
shapiro.test(alan_residuals)

# My Shapiro test failed again.
# My p value is 0.25 - above the threshold. There is no difference in the landward values across St. Croix. DOF = 42

# A.I = There is no statistically significant difference in the average light values (lw_avg) between the four regions (West End, North Shore, South Shore, and East End


### one-way Anova Test - Seaward Values - 4 Regions

alan_anova_sw <-aov(seaward_avg ~ REGION, data=stx_alan)
summary(alan_anova)

# Extract standardized residuals
alan_residuals_sw <-rstandard(alan_anova_sw)

#Test normality
shapiro.test(alan_residuals_sw)

# Shapiro likes high p-value. 
# My Shapiro results: My p-value is 0.429. This means that the residuals are
# normally distributed. Because the  p-value (0.429) is much larger than 0.05, 
# I failed to reject the NULL hypothesis.


### one-way Anova Test - Zenith Values - 4 Regions

alan_anova_zenith <-aov(zenith_avg ~ REGION, data=stx_alan)
summary(alan_anova_zenith)

# Extract standardized residuals
alan_residuals_zenith <-rstandard(alan_anova_zenith)

#Test normality
shapiro.test(alan_residuals_zenith)

# One of the four regions has a significantly different average zenith sky
# brightness compared to the others. The p-value is 0.0021, its below the 
# standard threshold of 0.05, so I failed to reject the NULL hypothesis. 


# My Shapiro results: My p-value is 0.4595. This means that the residuals are
# normally distributed and robust. Because the  p-value (0.4595) is much larger 
# than 0.05, I failed to reject the NULL hypothesis.

### Post Hoc Test

TukeyHSD(alan_anova_zenith)

# From my results: The North Shore and the East End are different 
# from each other. The South Shore and the East End are also different from 
# each other (statistically significant)!

# The South Shore is slightly darker than the East End (-0.96 difference).
# The North shore is slightly darker than the East End (0.76 difference).
# The North Shore and South Shore have the same levels of sky brightness. 


# West End vs. East End (p adj = 0.141): Statistically similar.
# West End vs. South Shore (p adj = 0.203): Statistically similar.
# West End vs. North Shore (p adj = 0.465): Statistically similar.
# South Shore vs. North Shore (p adj = 0.893): Identical brightness levels.

# Summary: ANOVA showed a significant regional effect on zenith sky brightness 
# (F(3, 42) = 5.784, p = 0.0021


