
data<-read.csv("Research_Data.csv")
head(data)
# install.packages("tidyverse")

library(tidyverse)

# Plotting Graphs Raw data 
data_raw<- data %>% select(arrivalcad, industprocad, repexcad, spexrmexuk, impu, imfear  )  # selected variables to plot 
IMF <-ts(data_raw$imfear, start = c(1996, 1), frequency = 4)
IMPU <-ts(data_raw$impu, start = c(1996, 1), frequency = 4)
Tour_Cand<-ts(data_raw$arrivalcad, start = c(1996, 1), frequency = 4)
plot(cbind(IMF, IMPU), plot.type = "single", col = c("blue", "red"), ylab="IMPU and MFI Indices ", xlab=" Date", main="U.S Migration Fear and Immigration Policy Uncerainty")
plot(Tour_Cand, plot.type = "single", col = "blue", ylab="TAFC ", xlab=" Date", main="Tourist Arrivals from Canada")

# Research Variables 
Research_Data<-data %>% select(data, lnindustprocad, lnrepexcad, lnspexrmexuk, lnimpu, lnimfear )
head(Research_Data)

# install.packages("stargazer")
library(stargazer)
stargazer(Research_Data)# latex summary stats 

# Adding the data/time frequency 
Research_Data<-ts(Research_Data, start = c(1996, 1), frequency = 4)

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
