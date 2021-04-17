install.packages("rwunderground")
library(rwunderground)

loaddata = read.csv("loaddata.csv")

library(dplyr)
library(lubridate)
colnames(loaddata) = c("Date","Load","Zone")
loaddata$Date = mdy_hm(loaddata$Date)
loaddata$Load = as.numeric(loaddata$Load )

##Find Duplicate dates
multiload = loaddata %>% group_by(Date) %>% summarize(Total = n()) %>% arrange(desc(Total)) %>% filter(Total > 1)
multiload
##make df with duplicate values and averages
multiloaddata= inner_join(loaddata, multiload, on = Date)
multiloaddata = multiloaddata %>% group_by(Date,Zone) %>% summarize(Load = as.numeric(mean(Load)))
multiloaddata = data.frame(multiloaddata[,c(1,3,2)])

##Remove duplicates and add averages
newloaddata = loaddata[!(loaddata$Date %in% multiload$Date),]
newloaddata = rbind(newloaddata,multiloaddata)

length(unique(date(newloaddata$Date)))
length(unique(hour(newloaddata$Date)))

##Create all date and hour combinations within single period
alltimes = NULL

min(newloaddata$Date)
max(newloaddata$Date)
alltimes = data.frame(seq(as.POSIXct("2014-01-01 00:00:00"), as.POSIXct("2019-09-01 23:00:00"), by="hour"))
colnames(alltimes) = "Date"

##Combine all dates with load data
df = left_join(alltimes,newloaddata,by = "Date")

##Find missing loads
missingtimes = df %>% filter(is.na(Load) == TRUE)

##Group by date
missingtimescount = missingtimes %>% group_by(date(Date)) %>% summarize(Count = n()) %>% arrange(-Count)

write.csv(df, "new_load_data.csv")

##Filling in missing load data was performed using excel. In general, in cases with 8 or fewer missing hours in
##a row, local averages were used - For example, if at 4pm the load is 10,000 and at 7pm the load is 13,000, we 
##would fill in 5pm as 11,000 and 6pm as 12,000. The exception would be when there was missing load data at times
##where historically there are local highs or lows. In that case, we use the other method (also used when there are
##more than 8 consecutive missing points) where we replaced missing data with data from dates and times that had a
##similar temperature history, similar day of the week, and same holiday/non-holiday status.

##Read in historical temperature data for LAX
temp = read.csv("LAX-Temperature.csv")
temp$date = ymd_hms(temp$date)


##Following code was used with different dates to check temperature similarities between dates
cbind((temp%>%filter(date(date) == "2014-03-09") %>% group_by(date) %>% summarize (tmpf = mean(tmpf)) %>%
  select(date, tmpf)),

(temp%>%filter(date(date) == "2014-03-02") %>% group_by(date) %>% summarize (tmpf = mean(tmpf)) %>%
    select(date, tmpf)))
