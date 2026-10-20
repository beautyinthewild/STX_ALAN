(library(tidyverse))


rawdensity=read.csv("raw density.csv")

# overiding the data into numbers

rawdensity$Nests=as.numeric(rawdensity$Nests)
rawdensity$NNEs=as.numeric(rawdensity$NNEs)

# convert beach length to numeric

rawdensity$Beach.Length=as.numeric(rawdensity$Beach.Length)
rawdensity$nestsD=rawdensity$Nests/rawdensity$Beach.Length

rawdensity$NNEsDensity=rawdensity$NNEs/rawdensity$Beach.Length



nestdensity=rawdensity%>%
  group_by(Location,Species)%>%
  summarize(nest=mean(nestsD,na.rm=T),
            NNE=mean(NNEsDensity,na.rm=T))

#Change characters to numeric to replace the NA

write.csv(nestdensity,"nestdensity.csv")

library(psych)

describeBy(nestdensity,group=nestdensity$Species)


# mean = Average density across all beaches


summary(nestdensity)

#across all species across all locations

# _________________________________________________________________

# Lets make columns - 25 percent cut off

rawdensity$Location=as.factor(rawdensity$Location)
rawdensity$Species=as.factor(rawdensity$Species)


ranks=rawdensity%>%
  group_by(Species,Location)%>%
  summarise(eggs=mean(nestsD,na.rm=T)
  )


turtle_rank=ranks%>%
  mutate(high=quantile(eggs,.75,na.rm=T), #avg nesting density
         low=quantile(eggs,.25,na.rm=T),
         rank=ifelse(eggs>=high,"high density",ifelse(eggs<=low,"low density","medium density"))
  )

hist(turtle_rank$eggs)

ggplot(turtle_rank, aes(x=eggs))+
  geom_histogram(bins = 20)

# change it to a factor

turtle_rank$rank=as.factor(turtle_rank$rank)

summary(turtle_rank$rank) #counts of rows for all species

#  high density    low density     medium density           NA's 
#         144            189            242                  631 

n_distinct(turtle_rank$Location)
# we have 68 beaches

#Code for CSV!

write.csv(turtle_rank,"turtlerank.csv")

#Pivot Wider - new data name

turtlewide=turtle_rank %>% 
  pivot_wider(names_from=Species, #changes shape of dataframe
              values_from = c(eggs,high,low,rank),