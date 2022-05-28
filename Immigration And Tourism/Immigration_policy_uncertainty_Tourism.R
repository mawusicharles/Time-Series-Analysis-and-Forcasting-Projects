# Code By Charles Mawusi 

setwd("~/Desktop/Git_Folder /Time-Series-Analysis-and-Forcasting-Projects/Immigration And Tourism") 
data<-read.csv("Research_Data.csv")
head(data)
# install.packages("tidyverse")

library(tidyverse)

# Plotting Graphs Raw data 
data_raw<- data %>% select("arrivalcad", "industprocad", "repexcad", "spexrmexuk", "impu", "imfear"  )  # selected variables to plot 
IMF <-ts(data_raw$imfear, start = c(1996, 1), frequency = 4)
IMPU <-ts(data_raw$impu, start = c(1996, 1), frequency = 4)
Tour_Cand<-ts(data_raw$arrivalcad, start = c(1996, 1), frequency = 4)
plot(cbind(IMF, IMPU), plot.type = "single", col = c("blue", "red"), ylab="IMPU and MFI Indices ", xlab=" Date", main="U.S Migration Fear and Immigration Policy Uncerainty")
plot(Tour_Cand, plot.type = "single", col = "blue", ylab="TAFC ", xlab=" Date", main="Tourist Arrivals from Canada")

# Research Variables 
library(dplyr)
Research_Data<-data %>% select("lnarrivalcad" , "lnindustprocad", "lnrepexcad", "lnspexrmexuk", "lnimpu", "lnimfear" )
head(Research_Data)
# install.packages("stargazer")
library(stargazer)
stargazer(Research_Data)# latex summary stats 

# Adding the data/time frequency 
Research_Data<-ts(Research_Data, start = c(1996, 1), frequency = 4)
head(Research_Data)
### Augmented Dickey-Fuller at levels 

# install.packages("urca")

library(urca)
# Adf Constant at levels 
Adf_stat_constant=NULL
Adf_stat_trend=NULL
PP_stat_constant=NULL
PP_stat_trend=NULL
for (i in 1:6){
  test_adf_cons<-ur.df(Research_Data[, i], type = "drift", selectlags = "AIC")
  a=test_adf_cons@teststat[1]
  Adf_stat_constant=rbind(Adf_stat_constant, a )
  #  constant and trend @levels 
  test_adf_trend<-ur.df(Research_Data[, i], type = "trend", selectlags = "AIC")
  b=test_adf_trend@teststat[1]
  Adf_stat_trend=rbind(Adf_stat_trend, b)
  # Phillip Perrons test constant @levels 
  test_PP_cons<-ur.pp(Research_Data[, i], type="Z-tau", model = "constant", lags="short")
  c=test_PP_cons@teststat[1]
  PP_stat_constant=rbind(PP_stat_constant, c)
  # Phillip Perrons test constant and trend @levels 
  test_PP_trend<-ur.pp(Research_Data[, i], type="Z-tau", model = "trend", lags="short")
  d=test_PP_trend@teststat[1]
  PP_stat_trend=rbind(PP_stat_trend, d)
}
table_constant<-cbind(colnames(Research_Data), Adf_stat_constant,PP_stat_constant, Adf_stat_trend, PP_stat_trend)
colnames(table_constant)[1:5]<-c("Variables","DF Statistics", "Phillips-Perron ","ADF Statistics", "Phillips-Perron")
table_constant

## First Difference 
Adf_stat_constant=NULL
Adf_stat_trend=NULL
PP_stat_constant=NULL
PP_stat_trend=NULL
for (i in 1:6){
  test_adf_cons<-ur.df(diff(Research_Data[, i]), type = "drift", selectlags = "AIC")
  a=test_adf_cons@teststat[1]
  Adf_stat_constant=rbind(Adf_stat_constant, a )
  #  constant and trend @levels 
  test_adf_trend<-ur.df(diff(Research_Data[, i]), type = "trend", selectlags = "AIC")
  b=test_adf_trend@teststat[1]
  Adf_stat_trend=rbind(Adf_stat_trend, b)
  # Phillip Perrons test constant @levels 
  test_PP_cons<-ur.pp(diff(Research_Data[, i]), type="Z-tau", model = "constant", lags="short")
  c=test_PP_cons@teststat[1]
  PP_stat_constant=rbind(PP_stat_constant, c)
  # Phillip Perrons test constant and trend @levels 
  test_PP_trend<-ur.pp(diff(Research_Data[, i]), type="Z-tau", model = "trend", lags="short")
  d=test_PP_trend@teststat[1]
  PP_stat_trend=rbind(PP_stat_trend, d)
}

table_constant_diff<-cbind(colnames(Research_Data), Adf_stat_constant,PP_stat_constant, Adf_stat_trend, PP_stat_trend)
table_constant_diff
colnames(table_constant_diff)[1:5]<-c("Variables","DF Statistics", "Phillips-Perron ","ADF Statistics", "Phillips-Perron")
table_constant_diff

path <- "~/Desktop/Immigration fears and Policy Uncertainty on TourismAnnals of Tourism/New Rewrite to submit 2022/R Code Results "
write.csv(table_constant, file.path(path, "ADF_PP_Levels_results.csv"), row.names=FALSE)
write.csv(table_constant_diff, file.path(path, "ADF_PP_Difference_results.csv"), row.names=FALSE)

# Bai Perron Test Best to use stata to compute the real regression 
# install.packages("tseries")
# install.packages("strucchange")

library(strucchange)
library(dplyr)

IMPUBai<-breakpoints(IMPU~1, h=10)
summary(IMPUBai)
IMFUBai<-breakpoints(IMF~1, h=10)
summary(IMFUBai)

# BDS test for linearity 
Research_Data<-as.data.frame(Research_Data)
library(tseries)

bds.test(Research_Data$lnarrivalcad , m=6)
bds.test(Research_Data$lnimpu, m=6)
bds.test(Research_Data$lnimfear, m=6)
# creating structural break dummies 
Research_Data$date_ts<-seq(as.Date("1996/1/1"), as.Date("2019/12/1"), by = "quarter")
Research_Data$IMFDummy<- ifelse(Research_Data$date_ts>="2008-07-01", 1, 0)
Research_Data$IMPUDummy<- ifelse(Research_Data$date_ts>="2010-07-01", 1, 0)
summary(Research_Data)
