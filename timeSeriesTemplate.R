#Code used for the basis of our time series page in the application

#Month time series
library(dplyr)
library(lubridate)

monthlyData <- read.csv(file = "~/CarlSeniorProject/rDashboard/monthlyData.csv")

monthlyData <- monthlyData %>%
  mutate(
    date = mdy(date), # Convert date from "m/d/yy" format to Date object
    year = year(date), # Extract year
    month = month(date) # Extract month
  ) %>%
  arrange(date) %>%
  select(-year, -month) # Optionally remove year and month if they are no longer needed


#Start Date 1989-10-01, end date 2016-09-01
moneyMonthlyData <- monthlyData[2]

moneyTimeSeriesMonth <- ts(moneyMonthlyData, frequency=12, start=c(1984,1))
moneyTimeSeriesMonth

##Time Series Plot
plot.ts(moneyTimeSeriesMonth)
abline(reg=lm(moneyTimeSeriesMonth~time(moneyTimeSeriesMonth)))

##Cycle Time Series
cycle(moneyTimeSeriesMonth)
plot(aggregate(moneyTimeSeriesMonth, FUN=mean))

#huge increase om december also see increase in march(tax returns season, get tax deductable)
boxplot(moneyTimeSeriesMonth~cycle(moneyTimeSeriesMonth))

##Passes Dickey-Fuller Test(Good for time series analysis)
#Use log and diff to take away the trend of upward in the data as well as unequal variance of the data
tseries::adf.test(diff(log(moneyTimeSeriesMonth)), alternative="stationary", k=0)

acf(log(moneyTimeSeriesMonth))

acf(diff(log(moneyTimeSeriesMonth)))
pacf(diff(log(moneyTimeSeriesMonth)))

fitMonth <- arima(log(moneyTimeSeriesMonth), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
predMonth <- predict(fitMonth, n.ahead = 10*12)
ts.plot(moneyTimeSeriesMonth,2.718^predMonth$pred, log="y", lty=c(1,3))