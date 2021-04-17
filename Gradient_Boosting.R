library(gbm)
library(lubridate)
library(MLmetrics)
library(dplyr)

final_data = read.csv("final_data-2.csv")
final_data$Date = ymd_hms(final_data$Date)
final_data$Holiday=as.integer(as.logical(final_data$Holiday))

colnames(final_data) = c("X","Date","weekday","month","hour","year","day","Holiday","HourlyDewPointTemperature_LAX",
              "HourlyRelativeHumidity_LAX","HourlyVisibility_LAX","HourlyDewPointTemperature_WJF", 
              "HourlyRelativeHumidity_WJF","HourlyVisibility_WJF","HourlyDewPointTemperature_RIV",
              "HourlyRelativeHumidity_RIV","HourlyVisibility_RIV","HourlyDewPointTemperature_TRM",
              "HourlyRelativeHumidity_TRM","HourlyVisibility_TRM","HourlyDewPointTemperature_CQT",
              "HourlyRelativeHumidity_CQT", "HourlyVisibility_CQT","Load","Zone","mean_temp","mean_humidity",
              "mean_visibility","lag_load_48_hrs","lag_load_one_week","lag_load_one_year","lag_load_72_hrs")


mape_dataset = data.frame(Date=as.Date(character()),
                                MAPE = double()) 

predictions_actuals = data.frame(Date=as.Date(character()),
                                 Load = double(),
                                 Prediction = double()) 

colnames(mape_dataset) = c("Date", "MAPE")

start_date = ymd_hms("2016-01-01 08:00:00")
end_date = ymd_hms("2018-01-01 08:00:00")

for (i in 101:365){
  print(i)
  train_start_date = start_date+days(i)
  train_end_date = end_date+days(i)
  prediction_start_date = end_date+days(i)
  prediction_end_date = end_date+days(i)+hours(40)
  
  training_data = final_data[(final_data$Date>=train_start_date) & (final_data$Date<=train_end_date),]
  prediction_data = final_data[(final_data$Date>prediction_start_date) & (final_data$Date<=prediction_end_date),]
  
  time_series.boost= gbm(Load~weekday+month+hour+year+day+Holiday+mean_temp+mean_humidity+mean_visibility+
                           lag_load_48_hrs+lag_load_72_hrs+lag_load_one_week+lag_load_one_year,
                         data = training_data, n.trees = 7500,
                         shrinkage = 0.01, interaction.depth = 4)
  
  predictions = predict(time_series.boost, prediction_data, n.trees=7500)
  
  
  mape = MLmetrics::MAPE(prediction_data[17:40,]$Load, predictions[17:40])
  
  temp_dataset = data.frame(matrix(NA, nrow = 1, ncol = 2))
  colnames(temp_dataset) = c("Date", "MAPE")
  temp_dataset$Date = train_end_date+hours(16)
  temp_dataset$MAPE = mape
  mape_dataset = rbind(mape_dataset, temp_dataset)
  
  prediction_data = cbind(prediction_data, predictions)
  
  prediction_data = prediction_data%>%
    filter(Date > end_date+days(i)+hours(16))%>%
    select(Date, Load, predictions)
  
  predictions_actuals = rbind(predictions_actuals, prediction_data)
  
}


