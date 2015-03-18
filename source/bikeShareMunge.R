
dataMunge <- function(df){
  
  ######################################################
  ### import the daylight.csv
  ######################################################
  
  daylight <- read.csv('./data/daylight.csv')
  
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
  #df.all[5<=df.all$numeric.hour & df.all$numeric.hour<=11,]$day.part <- "morning"
  #df.all[12<=df.all$numeric.hour & df.all$numeric.hour<=15,]$day.part <- "midday"
  #df.all[16<=df.all$numeric.hour & df.all$numeric.hour<=19,]$day.part <- "afternoon"
  #df.all[20<=df.all$numeric.hour | df.all$numeric.hour<=4,]$day.part <- "night"
  
  df.all[6<=df.all$numeric.hour & df.all$numeric.hour<=11,]$day.part <- "morning"
  df.all[12<=df.all$numeric.hour & df.all$numeric.hour<=17,]$day.part <- "midday"
  df.all[18<=df.all$numeric.hour & df.all$numeric.hour<=23,]$day.part <- "afternoon"
  df.all[0<=df.all$numeric.hour & df.all$numeric.hour<=5,]$day.part <- "night"
  
  
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
  
  return(df.all)
}  

dataMunge.test <- function(test){
  
  daylight <- read.csv('./data/daylight.csv')
  
  ######################################################
  ### steven please comment
  ######################################################
  
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
  test.all$weekday <- as.factor(weekdays(as.Date(paste(test.all$year,test.all$month,test.all$day,sep="-"))))
  
  return(test.all)
}  