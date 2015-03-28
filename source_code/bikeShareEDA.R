library(rpart)
library(randomForest)
library(tseries)
source('./source/bikeShareMunge.R')
#####################################################
### Before running the script
### please set the directory at <your path>/Bike-Share_Demand_Public
#####################################################
#setwd("")
rm(list = setdiff(ls(), lsf.str()))
df <- read.csv('./data/train.csv')
daylight <- read.csv('./data/daylight.csv')
df_ <- dataMunge(df)

######################################################
### Time Series Analyses
### 1.  acf plot
### 2.  Box Pierce test / serial correlation between
###     lag terms
### 3.  adf test / pp test / no unit root present
######################################################

count <- df_$count
png('./plot/fig2.png')
acf(count,lag.max=150)
dev.off()
Box.test(count,type=c('Box-Pierce'))
adf.test(count,alternative='stationary',k=24)
pp.test(count)

######################################################
### Time Series Analyses 
### 1.  group by day,month,year
### 2.  group by month,year
### 3.  plot the time series
######################################################

a1 <- aggregate(df_['count'], by= c(df_['day'],df_['month'],df_['year']),FUN=sum)
a2 <- aggregate(df_['count'], by= c(df_['month'],df_['year']),FUN=sum)

t1 <- ts(a1$count,frequency=7) 
t2 <- ts(a2$count,frequency=12,start=c(2011,1),end=c(2012,12)) 
plot(t1)
plot(t2)

######################################################
### Time Series Analyses 
### plot the acf and pacf of t1 and t2
######################################################

png('./plot/fig3.png',width=800,height=600)
par(mfrow=c(2,2))
acf(t1,50,main="TS of count group by day")
pacf(t1,50,main="TS of count group by day")
acf(t2,100, main="TS of count group by month")
pacf(t2,100,main="TS of count group by month")
dev.off()

######################################################
### univariate data analysis
######################################################

# Data Fields
#     datetime - hourly date + timestamp  
#     season -  1 = spring, 2 = summer, 3 = fall, 4 = winter 
#     holiday - whether the day is considered a holiday
#     workingday - whether the day is neither a weekend nor holiday
#     weather - 
#         1: Clear, Few clouds, Partly cloudy, Partly cloudy 
#         2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist 
#         3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds 
#         4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 
#     temp - temperature in Celsius
#     atemp - "feels like" temperature in Celsius
#     humidity - relative humidity
#     windspeed - wind speed
#     casual - number of non-registered user rentals initiated
#     registered - number of registered user rentals initiated
#     count - number of total rentals

plot(aggregate(df_['count'], by=c(df_['atemp']),FUN=sum),main='Count by atemp',xlab='Atemp',ylab='Count')
plot(aggregate(df_['count'], by=c(df_['atemp']),FUN=mean),main='Mean by atemp',xlab='Atemp',ylab='Mean')
# higher temperatures lead to more rentals until too high
# less predictive power as it gets higher

plot(aggregate(df_['count'],by=c(df_['holiday']),FUN=sum),main='Count by holiday',xlab='Holiday',ylab='Count')
plot(aggregate(df_['count'],by=c(df_['holiday']),FUN=mean),main='Mean by holiday',xlab='Holiday',ylab='Mean',ylim=c(0,200))
# far fewer examples with holidays

plot(aggregate(df_['count'], by= c(df_['hour']),FUN=sum),main='Count by hour',xlab='Hour',ylab='Count')
plot(aggregate(df_['count'], by= c(df_['hour']),FUN=mean),main='Mean by hour',xlab='Hour',ylab='Mean')
# rentals peak in the morning and afternoon
# fairly steady during day

plot(aggregate(df_['count'],by=c(df_['humidity']),FUN=sum),main='Count by humidity',xlab='Humidity',ylab='Count')
plot(aggregate(df_['count'],by=c(df_['humidity']),FUN=mean),main='Mean by humidity',xlab='Humidity',ylab='Mean')
# lower humidities a good predictor, but less information as it increases
# good correlation between average humidity and count if humidity>~20
#    so, there's probably another factor at play here

plot(aggregate(df_['count'],by=c(df_['month']),FUN=sum),main='Count by month',xlab='Month',ylab='Count')
plot(aggregate(df_['count'],by=c(df_['month']),FUN=mean),main='Mean by month',xlab='Month',ylab='Mean')
plot(aggregate(df_['count'],by=c(df_['season']),FUN=sum),main='Count by season',xlab='Season',ylab='Count')
plot(aggregate(df_['count'],by=c(df_['season']),FUN=mean),main='Mean by season',xlab='Season',ylab='Mean')
# more popular in late spring to fall
# means and counts line up

plot(aggregate(df_['count'],by=c(df_['temp']),FUN=sum),main='Count by temp',xlab='Temp',ylab='Count')
plot(aggregate(df_['count'],by=c(df_['temp']),FUN=mean),main='Mean by temp',xlab='Temp',ylab='Mean')
# mean number of bicycles seems to be highly correlated with temp
# similar to humidity where mean is highly correlated but cont isn't as much so

plot(aggregate(df_['count'], by= c(df_['weather']),FUN=sum),main='Count by weather',xlab='Weather',ylab='Count')
plot(aggregate(df_['count'], by= c(df_['weather']),FUN=mean),main='Mean by weather',xlab='Weather',ylab='Mean',ylim=c(0,220))
# on average, 4 and 2 are the same, just fewer instances of 4 in the data
# 3 is simply less popular
# weather - 
#   1: Clear, Few clouds, Partly cloudy
#   2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
#   3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds 
#   4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 

plot(aggregate(df_['count'], by= c(df_['windspeed']),FUN=sum),main='Count by windspeed',xlab='Windspeed',ylab='Count')
plot(aggregate(df_['count'], by= c(df_['windspeed']),FUN=mean),main='Mean by windspeed',xlab='Windspeed',ylab='Mean')
# windspeed doesn't look like too much of a factor unless it gets too high

plot(aggregate(df_['count'], by= c(df_['workingday']),FUN=sum),main='Count by workingday',xlab='Workingday',ylab='Count')
plot(aggregate(df_['count'], by= c(df_['workingday']),FUN=mean),main='Mean by workingday',xlab='Workingday',ylab='Mean',ylim=c(0,250))
# working days about equal with non-working days
# t.test 
#   H0: Difference in means==0
#   Ha: Difference in means!=0
t.test(df_[df$workingday==0,]$count,df_[df$workingday==1,]$count,alternative=c("two.sided"))
# Fail to Reject H0: means are equal
cor(df_$count,df$workingday) # = 0.01159
# I tested thoroughly because this goes against what I would have thought.

plot(aggregate(df_['count'], by= c(df_['year']),FUN=sum),main='Count by year',xlab='Year',ylab='Count',ylim=c(0,1500000))
plot(aggregate(df_['count'], by= c(df_['year']),FUN=mean),main='Mean by year',xlab='Year',ylab='Mean',ylim=c(0,250))
# more popular in 2012

# add day of week
df_$weekday <- as.factor(weekdays(as.Date(paste(df_$year,df_$month,df_$day,sep="-"))))

plot(aggregate(df_['count'], by= c(df_['weekday']),FUN=sum),main='Count by weekday',xlab='Weekday',ylab='Count',ylim=c(0,350000))
plot(aggregate(df_['count'], by= c(df_['weekday']),FUN=mean),main='Mean by weekday',xlab='Weekday',ylab='Mean',ylim=c(0,222))

# Just look at January
count.ts <- ts(df_$count)
count.ts.jan.2011 <- ts(df_[df_$year=='2011' & df_$month=='01',]$count)
plot(count.ts.jan.2011)

acf(count.ts.jan.2011,lag.max=26)
# repeats at 23
acf(count.ts,lag.max=26)
# repeats at 24 hours

pacf(count.ts.jan.2011)
# two significant lags
pacf(count.ts)


