library(gbm)
library(lubridate)
library(MLmetrics)
library(dplyr)
library(xts)
library(forecast)
library(ggplot2)
library(xts)

getwd()

final_data = read.csv("final_data.csv")
tail(final_data)
final_data$Date = ymd_hms(final_data$Date)
new = final_data[,c('Load','Date')]
ts1 = xts(new$Load, order.by = new$Date)
autoplot(ts1) + labs(titles = "Hourly Electricity Demand",
                     x = "year")

#Making histograms
ts1 %>% summary()
ggplot(new,aes(Load))+ geom_histogram(bins = 50, col = "red",alpha=0.5) +
  labs(y ="No. of values")

start_date = ymd_hms("2014-01-01 08:00:00")
end_date = ymd_hms("2019-08-31 23:00:00")
#newVal = as.numeric((end_date - start_date) + 1)

training_data = new[(new$Date>=start_date) & (new$Date<=end_date),]
ts1 = xts(training_data$Load,order.by = training_data$Date)

#
autoplot(ts1) + labs(titles = 'Hourly Demand',
                   x = 'Month-Year',
                   y = 'Load') 

autoplot(diff(ts1)) +
  labs(titles = "Differenced Hourly Electricity Demand",
       x = "year")
ts2 = ts(data = training_data$Load,frequency = 8760)
#ggseasonplot(ts2)

y = decompose(ts2, type = "mult")
plot(y)
z = decompose(ts2, type = "additive")
plot(z)

vb <- training_data$Load %>% msts(seasonal.periods = c(24*7,8760))
vb %>% mstl() %>% autoplot()
acf(ts1)