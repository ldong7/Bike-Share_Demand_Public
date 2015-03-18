# https://www.kaggle.com/c/bike-sharing-demand/forums/t/9423/rules
# "Your model should only use information which was available prior 
# to the time for which it is forecasting."

library(rpart)
library(car)
library(stargazer)
library(tseries)
library(forecast)
library(astsa)
library(PerformanceAnalytics)
library(vars)
library(FinTS)
library(fUnitRoots)
library(gbm)
library(randomForest)
library(ggplot2)
library(grid)
set.seed(64)
rm(list = setdiff(ls(), lsf.str()))
setwd('~/Desktop/module3//MSAN_630/bikeSharingDemand/') # Daniel
setwd('~/github/bikeSharingDemand/') # Steven

######################################################
### Data ingestion
######################################################

df <- read.csv('./data/train.csv')
daylight <- read.csv('./data/daylight.csv')

######################################################
### Data transformation
### 1. extract the year, month, day and hour
### 2. remove the datetime column
### 3. Merge with Daylight file
### 4. Add Features
### 5. Create Factors
### 6. Add Lags
### 7. Add Lag Diffs
######################################################

datetime_ <- as.character(df[,1])

m <- sapply( datetime_, f <- function(x){ 
  year <- substr(x,1,4)
  month <- substr(x,6,7)
  day <- substr(x,9,10)
  hour <- substr(x,12,13)
  return(c(year,month,day,hour))
})
m_ <- t(m)
dimnames(m_)[[2]] <- c("year","month","day","hour")
dimnames(m_)[[1]] <- seq(1,nrow(df))
df_ <- cbind(df,m_)
df_ <- df_[-c(1)] # remove the datatime

head(df_,10)
summary(df_)

# Dataframe for Features (Steven)
df.all <- cbind(df,m_)
df.all <- df.all[-c(1)]

## Transform Daylight ##
# Add character month
daylight$char.month <- "fail"
daylight[daylight$month<10,]$char.month <- paste0("0",daylight[daylight$month<10,]$month)
daylight[daylight$month>=10,]$char.month <- as.character(daylight[daylight$month>=10,]$month)
daylight$char.month <- factor(daylight$char.month)
unique(daylight$char.month)

# Add character day
daylight$char.day <- "fail"
daylight[daylight$day<10,]$char.day <- paste0("0",daylight[daylight$day<10,]$day)
daylight[daylight$day>=10,]$char.day <- as.character(daylight[daylight$day>=10,]$day)
daylight$char.day <- factor(daylight$char.day)
unique(daylight$char.day)

# Factor Year
daylight$char.year <- factor(daylight$year)

# update names
old.names <- c("year","month","day","sunrise","sunset","length","char.month","char.day","char.year")
new.names <- c("old.year","old.month","old.day","sunrise","sunset","day.hours","month","day","year")
names(daylight) <- new.names

# add daylight data
df.all <- merge(x=df.all,y=daylight,by=c('year','month','day'),x.all=TRUE)

# add is.daylight: boolean for whether daylight is present (based on sunrise and sunset)
df.all$is.daylight <- 0
df.all$numeric.hour <- as.numeric(df.all$hour)-1 # add numeric hour
df.all[df.all$numeric.hour>=df.all$sunrise & df.all$numeric.hour<=df.all$sunset,]$is.daylight <- 1

# Add is.working.hour
df.all$is.working.hour <- 0
df.all[df.all$workingday==1 & df.all$numeric.hour>=9 & df.all$numeric.hour<=16,]$is.working.hour <- 1

# Add Part_Of_Day with 4 values Morning, Midday, Afternoon, Night 
#  idea from http://machinelearningmastery.com/discover-feature-engineering-how-to-engineer-features-and-how-to-get-good-at-it/
df.all$day.part <- "morning"
df.all[5<=df.all$numeric.hour & df.all$numeric.hour<=11,]$day.part <- "morning"
df.all[12<=df.all$numeric.hour & df.all$numeric.hour<=15,]$day.part <- "midday"
df.all[16<=df.all$numeric.hour & df.all$numeric.hour<=19,]$day.part <- "afternoon"
df.all[20<=df.all$numeric.hour | df.all$numeric.hour<=4,]$day.part <- "night"

# drop redundant columns: sunrise, sunset, old.*, numeric.hour
df.all <- df.all[-c(16,17,18,19,20,23)]

# Create Factors
df.all$season <- as.factor(df.all$season)
df.all$holiday <- as.factor(df.all$holiday)
df.all$workingday <- as.factor(df.all$workingday)
df.all$weather <- as.factor(df.all$weather)
df.all$day.part <- as.factor(df.all$day.part)
df.all$is.working.hour <- as.factor(df.all$is.working.hour)
df.all$is.daylight <- as.factor(df.all$is.daylight)
df.all$hour <- factor(df.all$hour)

# create lagged features (to include 1,2,3, and 24 hour lags)
df.all$count.1  <- c(rep(NA, 1), head(df$count, -1))
df.all$count.2  <- c(rep(NA, 2), head(df$count, -2))
df.all$count.3  <- c(rep(NA, 3), head(df$count, -3))
df.all$count.24 <- c(rep(NA,24), head(df$count,-24))

## Create NAs where needed (due to disjoint data) ##
# for each first day of the month, count.24 => NA
df.all[df.all$day=='01',]$count.24 <- NA

# for each first hour of the month, (count.1,count.2,count.3) => NA
df.all[df.all$hour=='00' & df.all$day=='01',]$count.1 <- NA
df.all[df.all$hour=='00' & df.all$day=='01',]$count.2 <- NA
df.all[df.all$hour=='00' & df.all$day=='01',]$count.3 <- NA

# for each second hour of the month, (count.2,count.3) => NA
df.all[df.all$hour=='01' & df.all$day=='01',]$count.2 <- NA
df.all[df.all$hour=='01' & df.all$day=='01',]$count.3 <- NA

# for each third hour of the month, (count.3) => NA
df.all[df.all$hour=='02' & df.all$day=='01',]$count.3 <- NA

# Add Diff and diff(diff)=acceleration
df.all$diff <- (df.all$count.1-df.all$count.2)
df.all$accel <- (df.all$count.1-df.all$count.2)-(df.all$count.2-df.all$count.3)

# Rather than dropping NAs, should we put in mean, median, or mode?

# add days of week
df.all$weekday <- as.factor(weekdays(as.Date(paste(df.all$year,df.all$month,df.all$day,sep="-"))))

# add is.sunday
df.all$is.sunday <- 0
df.all[df.all$weekday=="Sunday",]$is.sunday <- 1

######################################################
### Exploratory Data Analyses
### 1. correlation matrix: registered and casual highly
###    correlated with the count
### 2. casual + registered = count
### 
######################################################

df2 <- df_[-c(12,13,14,15)]
cor(df2)

df_$total <- df_$registered + df_$casual
pairs(~count + total, data =df_)
png('./plot/fig1.png')
scatterplot.matrix( ~ count + total, data =df_)
dev.off()

######################################################
### Time Series Analyses
### 1.  acf plot
### 2.  adf test / pp test / no unit root present
### 
######################################################

#plot(ts(df_$count[1:120],frequency=24))
head(df_)
count <- df_$count
png('./plot/fig2.png')
acf(count,lag.max=150,main="Daily Seasonality in Number of Bicycles Rented",width=640,height=480)
dev.off()
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

png('./plot/fig3.png',width=800,height=600)
par(mfrow=c(2,2))
acf(t1,50,main="TS of count group by day")
pacf(t1,50,main="TS of count group by day")
acf(t2,100, main="TS of count group by month")
pacf(t2,100,main="TS of count group by month")
dev.off()
adf.test(t1,k=10)
pp.test(t1)
adf.test(t2$count)
par(mfrow=c(1,1))


png('./plot/fig_eda_1.png',width=1200,height=900)
g5 <- ggplot(df_,aes(hour,count)) + geom_boxplot() + coord_flip() + theme_set(theme_gray(base_size = 40)) + theme(plot.margin=unit(c(2.5,2.5,2,2),"cm")) + ggtitle("Central Tendency of Bicycles Rented By Hour")
g5
dev.off()

######################################################
### More data transformation
### convert categorical data into factor variable
######################################################

df_$season <- as.factor(df_$season)
df_$holiday <- as.factor(df$holiday)
df_$workingday <- as.factor(df$workingday)
df_$weather <- as.factor(df$weather)
str(df_)

######################################################
### Fitting model
### 
### 
######################################################

head(df_,10)
g1 <- gbm( count ~ . - registered - casual - total, data = df_, distribution='gaussian')
summary(g1)
str(g1)


######################################################
### Understanding the Data
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
head(df_['weekday'])

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

## Create Random Forest (with Regression Trees) ##
# Remove Day.  (These days will not be in the test set.)

# everything
rf1 <- randomForest(
  count ~ 
    atemp      + count.1 + count.2  + count.24  + count.3
  + day        + holiday + hour     + humidity  + month
  + registered + season  + temp     + weather   + windspeed
  + workingday + year,
  data=df.all,
  na.action=na.omit
)
plot(rf1)
plot(rf1$importance)
mean(rf1$mse) # Mean Squared Error = 381.28
mean(rf1$rsq) # Pseudo R Squared   = 0.988

# without registered
rf2 <- randomForest(
  count ~ 
    atemp  + count.1 + count.2  + count.24  + count.3
  + day    + holiday + hour     + humidity  + month
  + season + temp    + weather  + windspeed + workingday 
  + year,
  data=df.all,
  na.action=na.omit
)
plot(rf2)
plot(rf2$importance)
names(rf2)
mean(rf2$mse) # Mean Squared Error = 1472.748
mean(rf2$rsq) # Pseudo R Squared   = 0.955

# Try dropping Day, since that shouldn't matter
rf3 <- randomForest(
  count ~ 
    atemp   + count.1  + count.2   + count.24   + count.3
  + holiday + hour     + humidity  + month      + season 
  + temp    + weather  + windspeed + workingday + year,
  data=df.all,
  na.action=na.omit
)
plot(rf3)
plot(rf3$importance)
rf3$importance
names(rf3)
mean(rf3$mse) # Mean Squared Error = 1272.749
mean(rf3$rsq) # Pseudo R Squared   = 0.9614496
# MSE Decreased and R^2 Increased without Day
# So, leave it out

# Try dropping Holiday, since it contributes least
rf4 <- randomForest(
  count ~ 
    atemp   + count.1   + count.2    + count.24 + count.3
  + hour    + humidity  + month      + season   + temp    
  + weather + windspeed + workingday + year,
  data=df.all,
  na.action=na.omit
)
plot(rf4)
plot(rf4$importance)
rf4$importance
names(rf4)
mean(rf4$mse) # Mean Squared Error = 1321.176
mean(rf4$rsq) # Pseudo R Squared   = 0.9599828
# But, it does contribute, so keep it in

# Try dropping lags, to see if the benefit from extra rows
#   overcomes the lack of information from missing lags
rf5 <- randomForest(
  count ~ 
    atemp  + holiday + hour     + humidity  + month      
  + season + temp    + weather  + windspeed + workingday 
  + year,
  data=df.all
)
plot(rf5)
plot(rf5$importance)
rf5$importance
names(rf5)
mean(rf5$mse) # Mean Squared Error = 3065.953
mean(rf5$rsq) # Pseudo R Squared   = 0.9065552
# Keep them in!

##### After All Features 2015-03-01 #####

# Baseline with everything
rf6 <- randomForest(
  count ~ 
    atemp    + count.1     + count.2         + count.24   + count.3
  + day      + day.hours   + day.part        + holiday    + hour
  + humidity + is.daylight + is.working.hour + month      + season
  + temp     + weather     + windspeed       + workingday + year,
  data=df.all,
  na.action=na.omit
)
plot(rf6)
plot(rf6$importance)
mean(rf6$mse) # Mean Squared Error = 1363.423
mean(rf6$rsq) # Pseudo R Squared   = 0.9587

# Try dropping day, since that worked well before
rf7 <- randomForest(
  count ~ 
    atemp       + count.1         + count.2    + count.24 + count.3
  + day.hours   + day.part        + holiday    + hour     + humidity
  + is.daylight + is.working.hour + month      + season   + temp     
  + weather     + windspeed       + workingday + year,
  data=df.all,
  na.action=na.omit
)
plot(rf7)
plot(rf7$importance)
mean(rf7$mse) # Mean Squared Error = 1204.28
mean(rf7$rsq) # Pseudo R Squared   = 0.9635235

# Should drop atemp (since it's highly correlated with atemp and temp is more highly correlated with count)
pairs(~temp+atemp+count,data=df.all)
cor(df.all$count,df.all$temp)
cor(df.all$count,df.all$atemp)
cor(df.all$temp,df.all$atemp)

# drop atemp
rf8 <- randomForest(
  count ~ 
    count.1         + count.2    + count.24 + count.3  + day.hours   
  + day.part        + holiday    + hour     + humidity + is.daylight
  + is.working.hour + month      + season   + temp     + weather  
  + windspeed       + workingday + year,
  data=df.all,
  na.action=na.omit
)
plot(rf8)
plot(rf8$importance)
rf8$importance
mean(rf8$mse) # Mean Squared Error = 1203.779
mean(rf8$rsq) # Pseudo R Squared   = 0.9635386

# check correlations
cor(df.all[c('count','count.1','count.2','count.24','count.3','day.hours','humidity','temp','windspeed')],use="complete.obs")

# Counts 1-3 are highly correlated with each other; try dropping 2 and 3 (since 1 is most highly correlated with count)
rf9 <- randomForest(
  count ~ 
    count.1  + count.24 + day.hours   + day.part        + holiday    
  + hour     + humidity + is.daylight + is.working.hour + month      
  + season   + temp     + weather     + windspeed       + workingday 
  + year,
  data=df.all,
  na.action=na.omit
)
plot(rf9)
plot(rf9$importance)
rf9$importance
mean(rf9$mse) # Mean Squared Error = 1300.482
mean(rf9$rsq) # Pseudo R Squared   = 0.9606096
# Not much of a loss, and we have to get rid of them anyways


# check correlations
cor(df.all[c('count','day.hours','temp')],use="complete.obs")
# Temp and day.hours are highly correlated.  Temp has stronger correlation
# with Count, so drop day.hours

######### RUNNING #########
# drop day.hours
rf10 <- randomForest(
  count ~ 
    count.1  + count.24    + day.part        + holiday    + hour     
  + humidity + is.daylight + is.working.hour + month      + season   
  + temp     + weather     + windspeed       + workingday + year,
  data=df.all,
  na.action=na.omit
)
plot(rf10)
plot(rf10$importance)
mean(rf10$mse) # Mean Squared Error = 1269.599
mean(rf10$rsq) # Pseudo R Squared   = 0.961545

# check correlations
cor(df.all[c('count','count.1','count.24')],use="complete.obs")
# Count.1 and Count.24 are highly correlated.  Count.1 has stronger correlation
# with Count, so drop count.24

# drop count.24
rf11 <- randomForest(
  count ~ 
    count.1     + day.part        + holiday    + hour   + humidity 
  + is.daylight + is.working.hour + month      + season + temp
  + weather     + windspeed       + workingday + year,
  data=df.all,
  na.action=na.omit
)
plot(rf11)
plot(rf11$importance)
mean(rf11$mse) # Mean Squared Error = 1178.544
mean(rf11$rsq) # Pseudo R Squared   = 0.9641099

# check correlations
cor(df.all[c('count','count.1','humidity','temp','windspeed','diff','accel')],use="complete.obs")

# add diff, accel
rf12 <- randomForest(
  count ~ 
    count.1     + day.part        + holiday    + hour   + humidity 
  + is.daylight + is.working.hour + month      + season + temp
  + weather     + windspeed       + workingday + year   + diff
  + accel,
  data=df.all,
  na.action=na.omit
)
plot(rf12)
plot(rf12$importance)
mean(rf12$mse) # Mean Squared Error = 1183.449
mean(rf12$rsq) # Pseudo R Squared   = 0.9639902

# rm accel
rf13 <- randomForest(
  count ~ 
    count.1     + day.part        + holiday    + hour   + humidity 
  + is.daylight + is.working.hour + month      + season + temp
  + weather     + windspeed       + workingday + year   + diff,
  data=df.all,
  na.action=na.omit
)
plot(rf13)
plot(rf13$importance)
mean(rf13$mse) # Mean Squared Error = 1145.095
mean(rf13$rsq) # Pseudo R Squared   = 0.9651453

# check correlations
cor(df.all[c('count','count.1','humidity','temp','windspeed','diff')],use="complete.obs")

# rm windspeed
rf14 <- randomForest(
  count ~ 
    count.1     + day.part        + holiday + hour   + humidity 
  + is.daylight + is.working.hour + month   + season + temp
  + weather     + workingday      + year    + diff,
  data=df.all,
  na.action=na.omit
)
plot(rf14)
plot(rf14$importance)
mean(rf14$mse) # Mean Squared Error = 1148.622
mean(rf14$rsq) # Pseudo R Squared   = 0.965038
# slightly worse, so keep it in

## Best ##
# add weekday
rf15 <- randomForest(
  count ~ 
    count.1     + day.part        + holiday    + hour   + humidity 
  + is.daylight + is.working.hour + month      + season + temp
  + weather     + windspeed       + workingday + year   + diff
  + weekday,
  data=df.all,
  na.action=na.omit
)
plot(rf15)
rf15$importance
plot(rf15$importance)
mean(rf15$mse) # Mean Squared Error = 1101.717
mean(rf15$rsq) # Pseudo R Squared   = 0.9664657

# add is.sunday
rf16 <- randomForest(
  count ~ 
    count.1     + day.part        + holiday    + hour   + humidity 
  + is.daylight + is.working.hour + month      + season + temp
  + weather     + windspeed       + workingday + year   + diff
  + weekday     + is.sunday,
  data=df.all,
  na.action=na.omit
)
plot(rf16)
rf16$importance
plot(rf16$importance)
mean(rf16$mse) # Mean Squared Error = 1111.39
mean(rf16$rsq) # Pseudo R Squared   = 0.9661713

# rm weekday (to see if they're correlated)
rf17 <- randomForest(
  count ~ 
    count.1     + day.part        + holiday    + hour   + humidity 
  + is.daylight + is.working.hour + month      + season + temp
  + weather     + windspeed       + workingday + year   + diff
  + is.sunday,
  data=df.all,
  na.action=na.omit
)
plot(rf17)
rf17$importance
plot(rf17$importance)
mean(rf17$mse) # Mean Squared Error = 1136.464
mean(rf17$rsq) # Pseudo R Squared   = 0.9654081



#########################################################
### Data transform on the testing set
### Can we rowbind the training and testing data?
#########################################################

df.all <- merge(x=df.all,y=daylight,by=c('year','month','day'),x.all=TRUE)

## FROM HERE 

test <- read.csv('./data/test.csv')

datetime2_ <- as.character(test[,1])
m2 <- sapply( datetime2_, f <- function(x){ 
  year <- substr(x,1,4)
  month <- substr(x,6,7)
  day <- substr(x,9,10)
  hour <- substr(x,12,13)
  return(c(year,month,day,hour))
})
m2_ <- t(m2)
dimnames(m2_)[[2]] <- c("year","month","day","hour")
dimnames(m2_)[[1]] <- seq(1,nrow(test))
test_ <- cbind(test,m2_)
test_ <- test_[-c(1)]

test_$season <- as.factor(test_$season)
test_$holiday <- as.factor(test_$holiday)
test_$workingday <- as.factor(test_$workingday)
test_$weather <- as.factor(test_$weather)

######################################################
### add sunrise sunset day.hours
######################################################

test.all <- merge(x=test_,y=daylight,by=c('year','month','day'),x.all=TRUE)

# add is.daylight: boolean for whether daylight is present (based on sunrise and sunset)
test.all$is.daylight <- 0
test.all$numeric.hour <- as.numeric(test.all$hour)-1 # add numeric hour
test.all[test.all$numeric.hour>=test.all$sunrise & test.all$numeric.hour<=test.all$sunset,]$is.daylight <- 1

# Add is.working.hour
test.all$is.working.hour <- 0
test.all[test.all$workingday==1 & test.all$numeric.hour>=9 & test.all$numeric.hour<=16,]$is.working.hour <- 1

# Add Part_Of_Day with 4 values Morning, Midday, Afternoon, Night 
#  idea from http://machinelearningmastery.com/discover-feature-engineering-how-to-engineer-features-and-how-to-get-good-at-it/
test.all$day.part <- "morning"
test.all[5<=test.all$numeric.hour & test.all$numeric.hour<=11,]$day.part <- "morning"
test.all[12<=test.all$numeric.hour & test.all$numeric.hour<=15,]$day.part <- "midday"
test.all[16<=test.all$numeric.hour & test.all$numeric.hour<=19,]$day.part <- "afternoon"
test.all[20<=test.all$numeric.hour | test.all$numeric.hour<=4,]$day.part <- "night"

test.all$day.part <- as.factor(test.all$day.part)
test.all$is.working.hour <- as.factor(test.all$is.working.hour)
test.all$is.daylight <- as.factor(test.all$is.daylight)
test.all$hour <- factor(test.all$hour)


testx <- test.all

# write dfx to csv file
write.table(testx,file="testFinal.csv",sep=",",row.names=FALSE)


## TILL HERE

#########################################################
### split the dataset into 24 data frame
#########################################################

dfx <- df.all[c("count","count.1","day.part","holiday","hour","humidity","is.daylight","is.working.hour","month","season","temp","weather","windspeed","workingday","year","diff")]
dfx$my <- paste(dfx$year,dfx$month)
df_addlags <- split(dfx,dfx$my)
str(df_addlags,max.level=1)

# write dfx to csv file
write.table(dfx,file="trainFinal.csv",sep=",",row.names=FALSE)

df_modified1 <- do.call(rbind, df_addlags[1])
df_modified2 <- do.call(rbind, df_addlags[c(1:2)])
df_modified3 <- do.call(rbind, df_addlags[c(1:3)])
df_modified4 <- do.call(rbind, df_addlags[c(1:4)])
df_modified5 <- do.call(rbind, df_addlags[c(1:5)])
df_modified6 <- do.call(rbind, df_addlags[c(1:6)])
df_modified7 <- do.call(rbind, df_addlags[c(1:7)])
df_modified8 <- do.call(rbind, df_addlags[c(1:8)])
df_modified9 <- do.call(rbind, df_addlags[c(1:9)])
df_modified10 <- do.call(rbind, df_addlags[c(1:10)])
df_modified11 <- do.call(rbind, df_addlags[c(1:11)])
df_modified12 <- do.call(rbind, df_addlags[c(1:12)])
df_modified13 <- do.call(rbind, df_addlags[c(1:13)])
df_modified14 <- do.call(rbind, df_addlags[c(1:14)])
df_modified15 <- do.call(rbind, df_addlags[c(1:15)])
df_modified16 <- do.call(rbind, df_addlags[c(1:16)])
df_modified17 <- do.call(rbind, df_addlags[c(1:17)])
df_modified18 <- do.call(rbind, df_addlags[c(1:18)])
df_modified19 <- do.call(rbind, df_addlags[c(1:19)])
df_modified20 <- do.call(rbind, df_addlags[c(1:20)])
df_modified21 <- do.call(rbind, df_addlags[c(1:21)])
df_modified22 <- do.call(rbind, df_addlags[c(1:22)])
df_modified23 <- do.call(rbind, df_addlags[c(1:23)])
df_modified24 <- do.call(rbind, df_addlags[c(1:24)])


rf01_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified1 ,na.action=na.omit)
rf02_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified2 ,na.action=na.omit)
rf03_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified3 ,na.action=na.omit)
rf04_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified4 ,na.action=na.omit)
rf05_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified5 ,na.action=na.omit)
rf06_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified6 ,na.action=na.omit)
rf07_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified7 ,na.action=na.omit)
rf08_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified8 ,na.action=na.omit)
rf09_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified9 ,na.action=na.omit)
rf10_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified10 ,na.action=na.omit)
rf11_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified11 ,na.action=na.omit)
rf12_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified12 ,na.action=na.omit)
rf13_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified13 ,na.action=na.omit)
rf14_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified14 ,na.action=na.omit)
rf15_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified15 ,na.action=na.omit)
rf16_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified16 ,na.action=na.omit)
rf17_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified17 ,na.action=na.omit)
rf18_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified18 ,na.action=na.omit)
rf19_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified19 ,na.action=na.omit)
rf20_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified20 ,na.action=na.omit)
rf21_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified21 ,na.action=na.omit)
rf22_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified22 ,na.action=na.omit)
rf23_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified23 ,na.action=na.omit)
rf24_ <- randomForest(count ~ count.1     + day.part        + holiday    + hour   + humidity + is.daylight + is.working.hour + month      + season + temp+ weather     + windspeed       + workingday + year   + diff,data=df_modified24 ,na.action=na.omit)

save(rf01_,rf02_,rf03_,rf04_,rf05_,rf06_,rf07_,rf08_,rf09_,rf10_,rf11_,rf12_,rf13_,rf14_,rf15_,rf16_,rf17_,rf18_,rf19_,rf20_,rf21_,rf22_,rf23_,rf24_,file='rfxx_.RData')

#########################################################
### start the prediction work
#########################################################

testx <- test.all
testx$my <- paste(test_$year,test_$month)
testxx <- split(testx,testx$my)


head(test1)

######################################################
### Predicting - testing
######################################################


df_modifiedx_ <- df_modified1[-c(ncol(df_modified1))]
test1 <- testxx[[1]]
p_ <- c()
for ( i in seq(1:nrow(test1))){
  
    test_temp <- test1[i,c("day.part","holiday","hour","humidity","is.daylight","is.working.hour","month","season","temp","weather","windspeed","workingday","year")]
    
    test_temp$count.1 <- df_modifiedx_$count[nrow(df_modifiedx_)] # count.1
    test_temp$diff <- df_modifiedx_$count[nrow(df_modifiedx_)] - df_modifiedx_$count[nrow(df_modifiedx_)-1] # diff
    test_temp$count <- 0
    test_temp <- test_temp[c("count","count.1","day.part","holiday","hour","humidity","is.daylight","is.working.hour","month","season","temp","weather","windspeed","workingday","year","diff")]
    #print(str(test_temp))
    test_temp$count <- predict(rf01_,newdata = test_temp)  
    
    df_modifiedx_ <- rbind(df_modifiedx_,test_temp)
    p_ <- c(p_, test_temp$count)
  }  

######################################################
### Predicting - all
######################################################
rfxx <- list(rf01_,rf02_,rf03_,rf04_,rf05_,rf06_,rf07_,rf08_,rf09_,rf10_,rf11_,rf12_,rf13_,rf14_,rf15_,rf16_,rf17_,rf18_,rf19_,rf20_,rf21_,rf22_,rf23_,rf24_)
p_ <- c()
for ( i in seq(1:length(df_addlags))){
  
  df_modifiedx_ <- df_addlags[[i]] # get the training set
  df_modifiedx_ <- df_modifiedx_[-c(ncol(df_modifiedx_))] # rm "my"
  test1 <- testxx[[i]] # get the test set
  
  for ( j in seq(1:nrow(test1))){
    
    test_temp <- test1[j,c("day.part","holiday","hour","humidity","is.daylight","is.working.hour","month","season","temp","weather","windspeed","workingday","year")]
    
    test_temp$count.1 <- df_modifiedx_$count[nrow(df_modifiedx_)] # count.1
    test_temp$diff <- df_modifiedx_$count[nrow(df_modifiedx_)] - df_modifiedx_$count[nrow(df_modifiedx_)-1] # diff
    test_temp$count <- 0
    test_temp <- test_temp[c("count","count.1","day.part","holiday","hour","humidity","is.daylight","is.working.hour","month","season","temp","weather","windspeed","workingday","year","diff")]
    #print(str(test_temp))
    test_temp$count <- predict(rfxx[[i]],newdata = test_temp)  
    
    df_modifiedx_ <- rbind(df_modifiedx_,test_temp)
    p_ <- c(p_, test_temp$count)
  }  
  }

######################################################
### Submission
######################################################

sbmn <- read.csv('./submission/sampleSubmission (2).csv')
sbmn$count <- p_
write.csv(sbmn,'./submission/submission3.csv',row.names=FALSE)



######################################################
### dummitize
######################################################

nrow(dfx)
dfs.names <- names(dfx)

sapply(dfx,is.numeric)
# dummitize
unique(dfx$year)
unique(dfx$month)
unique(dfx$hour)
unique(dfx$season)
unique(dfx$weather)

# year
dfx$y.2011 <- 0
dfx[dfx$year=="2011",]$y.2011 <- 1
dfx$y.2012 <- 0
dfx[dfx$year=="2012",]$y.2012 <- 1

# month
dfx$m.01 <- 0
dfx[dfx$month=="01",]$m.01 <- 1
dfx$m.02 <- 0
dfx[dfx$month=="02",]$m.02 <- 1
dfx$m.03 <- 0
dfx[dfx$month=="03",]$m.03 <- 1
dfx$m.04 <- 0
dfx[dfx$month=="04",]$m.04 <- 1
dfx$m.05 <- 0
dfx[dfx$month=="05",]$m.05 <- 1
dfx$m.06 <- 0
dfx[dfx$month=="06",]$m.06 <- 1
dfx$m.07 <- 0
dfx[dfx$month=="07",]$m.07 <- 1
dfx$m.08 <- 0
dfx[dfx$month=="08",]$m.08 <- 1
dfx$m.09 <- 0
dfx[dfx$month=="09",]$m.09 <- 1
dfx$m.10 <- 0
dfx[dfx$month=="10",]$m.10 <- 1
dfx$m.11 <- 0
dfx[dfx$month=="11",]$m.11 <- 1
dfx$m.12 <- 0
dfx[dfx$month=="12",]$m.12 <- 1

# hour
dfx$h.00 <- 0
dfx[dfx$hour=="00",]$h.00 <- 1
dfx$h.01 <- 0
dfx[dfx$hour=="01",]$h.01 <- 1
dfx$h.02 <- 0
dfx[dfx$hour=="02",]$h.02 <- 1
dfx$h.03 <- 0
dfx[dfx$hour=="03",]$h.03 <- 1
dfx$h.04 <- 0
dfx[dfx$hour=="04",]$h.04 <- 1
dfx$h.05 <- 0
dfx[dfx$hour=="05",]$h.05 <- 1
dfx$h.06 <- 0
dfx[dfx$hour=="06",]$h.06 <- 1
dfx$h.07 <- 0
dfx[dfx$hour=="07",]$h.07 <- 1
dfx$h.08 <- 0
dfx[dfx$hour=="08",]$h.08 <- 1
dfx$h.09 <- 0
dfx[dfx$hour=="09",]$h.09 <- 1
dfx$h.10 <- 0
dfx[dfx$hour=="10",]$h.10 <- 1
dfx$h.11 <- 0
dfx[dfx$hour=="11",]$h.11 <- 1
dfx$h.12 <- 0
dfx[dfx$hour=="12",]$h.12 <- 1
dfx$h.13 <- 0
dfx[dfx$hour=="13",]$h.13 <- 1
dfx$h.14 <- 0
dfx[dfx$hour=="14",]$h.14 <- 1
dfx$h.15 <- 0
dfx[dfx$hour=="15",]$h.15 <- 1
dfx$h.16 <- 0
dfx[dfx$hour=="16",]$h.16 <- 1
dfx$h.17 <- 0
dfx[dfx$hour=="17",]$h.17 <- 1
dfx$h.18 <- 0
dfx[dfx$hour=="18",]$h.18 <- 1
dfx$h.19 <- 0
dfx[dfx$hour=="19",]$h.19 <- 1
dfx$h.20 <- 0
dfx[dfx$hour=="20",]$h.20 <- 1
dfx$h.21 <- 0
dfx[dfx$hour=="21",]$h.21 <- 1
dfx$h.22 <- 0
dfx[dfx$hour=="22",]$h.22 <- 1
dfx$h.23 <- 0
dfx[dfx$hour=="23",]$h.23 <- 1

# season
dfx$s.spring <- 0
dfx[dfx$season=="1",]$s.spring <- 1
dfx$s.summer <- 0
dfx[dfx$season=="2",]$s.summer <- 1
dfx$s.fall <- 0
dfx[dfx$season=="3",]$s.fall <- 1
dfx$s.winter <- 0
dfx[dfx$season=="4",]$s.winter <- 1

# weather
dfx$w.clear <- 0
dfx[dfx$weather=="1",]$w.clear <- 1
dfx$w.mist <- 0
dfx[dfx$weather=="2",]$w.mist <- 1
dfx$w.light <- 0
dfx[dfx$weather=="3",]$w.light <- 1
dfx$w.heavy <- 0
dfx[dfx$weather=="4",]$w.heavy <- 1

# create separate data frame
df.dummy <- dfx

# change NAs to 0 (This is just test data.)
df.dummy[is.na(df.dummy)] <- 0

df.dummy <- df.dummy[-c(3,5,9,10,12,15)]
head(df.dummy)

# create training indices
sample.indices <- sample(nrow(df.dummy),floor(nrow(df.dummy)*.2))
test.indices

# save data
df.dummy[sample.indices,]

# convert factor columns to numeric columns
df.dummy$holiday <- as.numeric(df.dummy$holiday)
df.dummy$is.daylight <- as.numeric(df.dummy$is.daylight)
df.dummy$is.working.hour <- as.numeric(df.dummy$is.working.hour)
df.dummy$workingday <- as.numeric(df.dummy$workingday)

# create files
write.table(df.dummy[-sample.indices,],file="testJunk.csv",sep=",",row.names=FALSE)
write.table(df.dummy[sample.indices,],file="trainJunk.csv",sep=",",row.names=FALSE)

