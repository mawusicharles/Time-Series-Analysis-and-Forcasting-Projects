
data<-read.csv("Research_Data.csv")
head(data)
# install.packages("tidyverse")

library(tidyverse)
Research_Data<-data %>% select(lnarrivalcad, lnindustprocad, lnrepexcad, lnspexrmexuk, lnimpu, lnimfear )
head(Research_Data)

# install.packages("stargazer")
library(stargazer)
stargazer(Research_Data)# latex summary stats 

# Adding the data/time frequency 

Research_Data<-ts(Research_Data, start = c(1996, 1), frequency = 4)
