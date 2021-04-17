library(rvest)
library(dplyr)
library(forecast)
library(ggplot2)
#install.packages("fpp2")
library(fpp2)
#install.packages("seasonal")
library(seasonal)
library(lubridate)

df  = read.csv("final_data.csv")

df$Date = ymd_hms(df$Date)

##Create time series object
##install.packages("xts") 
require(xts)

# Create an XTS object to hold the time series
ts1 = xts(df$Load, order.by = df$Date)

# Subset training
# Note - in creating time series objects, the cut-off dates in the code have an 
# 8 hour difference from what the output provide as a work-around to difference
# In time zones
train = window(ts1,end= as.POSIXct('2016-12-31 15:00', format ="%Y-%m-%d %H:%M"))
test1 = window(ts1,start=as.POSIXct('2016-12-31 16:00', format ="%Y-%m-%d %H:%M"),
                  end =as.POSIXct('2017-12-31 15:00', format ="%Y-%m-%d %H:%M") )
test2 = window(ts1,start=as.POSIXct('2017-12-31 16:00', format ="%Y-%m-%d %H:%M"),
               end =as.POSIXct('2018-12-31 15:00', format ="%Y-%m-%d %H:%M") )
test3 = window(ts1,start=as.POSIXct('2018-12-31 16:00', format ="%Y-%m-%d %H:%M") )
ntest1 = df %>% filter(year(Date)=="2017") %>%nrow()
ntest2 = df %>% filter(year(Date)=="2018") %>%nrow()
ntest3 = df %>% filter(year(Date)=="2019") %>%nrow()
ntrain = df %>% filter(year(Date)=="2014" | year(Date)=="2015" | year(Date)=="2016") %>%nrow()

##Split into train and test
##Frequency of 168 used to have a period of one week - 24 hours * 7 weeks = 168
Cls1=decompose(ts(train, frequency = 168),type="multiplicative")
Cls2=stl(ts(as.numeric(train), frequency=168),s.window = "periodic",robust=T)

##Check model accuracy for different models
##Classical Time Series Decomposition
Cls2F1=forecast(Cls2,h=ntest1,method="naive",lambda="auto")
accuracy(Cls2F1,test1)
accuracy(Cls2F1,test2)
accuracy(Cls2F1,test3)

##SES Model
Cls2F2=forecast(Cls2,h=ntest1,method="ets")
accuracy(Cls2F2,test1)
accuracy(Cls2F2,test2)
accuracy(Cls2F2,test3)

##Theta Method
Theta=thetaf(train,h=ntest1)
accuracy(Theta,test1)
accuracy(Theta,test2)
accuracy(Theta,test3)

