library(rpart)
library(car)
library(randomForest)

###################################################
### remove the local environment variable 
### setwd the directory to where the repository is
### source the munge function
###################################################

source('./source/bikeShareMunge.R')
rm(list = setdiff(ls(), lsf.str()))

###################################################
### import the data munge the it !!
###################################################

df <- read.csv('./data/train.csv')
daylight <- read.csv('./data/daylight.csv')
df.all <- dataMunge(df)

################################################################
## Best
################################################################

set.seed(30)

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
mean(rf15$mse) # Mean Squared Error = 1095.36
mean(rf15$rsq) # Pseudo R Squared   = 0.9664657

