library(gbm)
library(lubridate)
library(MLmetrics)
library(dplyr)
library(xts)
library(forecast)
getwd()

final_data = read.csv("final_data.csv")
tail(final_data)
final_data$Date = ymd_hms(final_data$Date)
new = final_data[,c('Load','Date')]

mape_dataset = data.frame(Date=as.Date(character()),MAPE = double()) 

predictions_actuals = data.frame(Date=as.Date(character()),Load = double(),
                                 Prediction = double()) 
colnames(mape_dataset) = c("Date", "MAPE")

start_date = ymd_hms("2015-01-01 08:00:00")
end_date = ymd_hms("2017-12-30 08:00:00")

for (i in 1:365){
  print(i)
  train_start_date = start_date + days(i)
  train_end_date = end_date + days(i)
  prediction_start_date = end_date + days(i)
  prediction_end_date = end_date+ days(i) + hours(40)
  
  training_data = new[(new$Date>=train_start_date) & (new$Date<=train_end_date),]
  ts1 = xts(training_data$Load, order.by = training_data$Date)
  #train_new = training_data[,'Load']
  prediction_data = new[(new$Date>prediction_start_date) & (new$Date<=prediction_end_date),]
  ts2 = xts(prediction_data$Load, order.by = prediction_data$Date)
  #pred_new = prediction_data[,'Load']
  
  train.ho.a <- naive(ts1,h=40)
  Prediction <- train.ho.a[["mean"]]
  z = Prediction[17:40]
  mape = MLmetrics::MAPE(ts2[17:40], z)
  
  #accuracy(train.ho.a,pred_new)['Test set','MAPE']
  
  temp_dataset = data.frame(matrix(NA, nrow = 1, ncol = 2))
  colnames(temp_dataset) = c("Date", "MAPE")
  temp_dataset$Date = train_end_date+hours(16)
  temp_dataset$MAPE = mape
  mape_dataset = rbind(mape_dataset, temp_dataset)
  
  prediction_data = cbind(prediction_data, Prediction = as.numeric(Prediction))
  
  prediction_data = prediction_data%>%
    filter(Date > end_date + days(i) + hours(16))%>%
    select(Date, Load, Prediction)
  
  predictions_actuals = rbind(predictions_actuals, prediction_data)
  #mape = MLmetrics::MAPE(pred_new[17:40], predictions[17:40,]$fitted)
}
mean(mape_dataset$MAPE)

#write.csv(predictions_actuals, file = "predictions2018_NaiveForecasting.csv")
#write.csv(mape_dataset, file = "MAPE2018_NaiveForecasting.csv")