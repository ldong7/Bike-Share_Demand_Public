library(rpart)
library(randomForest)

set.seed(30)
rm(list = setdiff(ls(), lsf.str()))
#####################################################
### Before running the script
### please set the directory at <your path>/Bike-Share_Demand_Public
#####################################################

source('./source/bikeShareMunge.R')

######################################################
### Data ingestion
######################################################

df <- read.csv('./data/train.csv')
test <- read.csv('./data/test.csv')
daylight <- read.csv('./data/daylight.csv')

df.all <- dataMunge(df)
test.all <- dataMunge.test(test)

#########################################################
### split the training set into 24 data frame
#########################################################

dfx <- df.all[c("count","count.1","day.part","holiday","hour","humidity","is.daylight","is.working.hour","month","season","temp","weather","windspeed","workingday","year","diff","weekday")]
dfx$my <- paste(dfx$year,dfx$month)
df_addlags <- split(dfx,dfx$my)
str(df_addlags,max.level=1)


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

################################################################
### model training not necessary need to run
### can call the saved R image
################################################################

rf01_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified1 ,na.action=na.omit)
rf02_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified2 ,na.action=na.omit)
rf03_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified3 ,na.action=na.omit)
rf04_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified4 ,na.action=na.omit)
rf05_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified5 ,na.action=na.omit)
rf06_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified6 ,na.action=na.omit)
rf07_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified7 ,na.action=na.omit)
rf08_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified8 ,na.action=na.omit)
rf09_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified9 ,na.action=na.omit)
rf10_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified10 ,na.action=na.omit)
rf11_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified11 ,na.action=na.omit)
rf12_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified12 ,na.action=na.omit)
rf13_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified13 ,na.action=na.omit)
rf14_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified14 ,na.action=na.omit)
rf15_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified15 ,na.action=na.omit)
rf16_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified16 ,na.action=na.omit)
rf17_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified17 ,na.action=na.omit)
rf18_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified18 ,na.action=na.omit)
rf19_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified19 ,na.action=na.omit)
rf20_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified20 ,na.action=na.omit)
rf21_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified21 ,na.action=na.omit)
rf22_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified22 ,na.action=na.omit)
rf23_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified23 ,na.action=na.omit)
rf24_ <- randomForest(count ~ count.1 + day.part + holiday + hour + humidity + is.daylight + is.working.hour + month + season + temp + weather + windspeed + workingday + year + diff + weekday,data=df_modified24 ,na.action=na.omit)


save(rf01_,rf02_,rf03_,rf04_,rf05_,rf06_,rf07_,rf08_,rf09_,rf10_,rf11_,rf12_,rf13_,rf14_,rf15_,rf16_,rf17_,rf18_,rf19_,rf20_,rf21_,rf22_,rf23_,rf24_,file='./model/rfxx_v2.RData')

################################################################
### reload model from R image
################################################################

load('./models/rfxx_v2.RData')
rfxx <- list(rf01_,rf02_,rf03_,rf04_,rf05_,rf06_,rf07_,rf08_,rf09_,rf10_,rf11_,rf12_,rf13_,rf14_,rf15_,rf16_,rf17_,rf18_,rf19_,rf20_,rf21_,rf22_,rf23_,rf24_)

################################################################
### predict the test set
################################################################

testx <- test.all
testx$my <- paste(test.all$year,test.all$month)
testxx <- split(testx,testx$my)

p_ <- c()
for ( i in seq(1:length(df_addlags))){
  
  df_modifiedx_ <- df_addlags[[i]] # get the training set
  df_modifiedx_ <- df_modifiedx_[-c(ncol(df_modifiedx_))] # rm "my"
  test1 <- testxx[[i]] # get the test set
  
  for ( j in seq(1:nrow(test1))){
    
    test_temp <- test1[j,c("day.part","holiday","hour","humidity","is.daylight","is.working.hour","month","season","temp","weather","windspeed","workingday","year","weekday")]
    
    test_temp$count.1 <- df_modifiedx_$count[nrow(df_modifiedx_)] # count.1
    test_temp$diff <- df_modifiedx_$count[nrow(df_modifiedx_)] - df_modifiedx_$count[nrow(df_modifiedx_)-1] # diff
    test_temp$count <- 0
    test_temp <- test_temp[c("count","count.1","day.part","holiday","hour","humidity","is.daylight","is.working.hour","month","season","temp","weather","windspeed","workingday","year","diff","weekday")]
    
    #print(str(test_temp))
    test_temp$count <- predict(rfxx[[i]],newdata = test_temp)  
    
    df_modifiedx_ <- rbind(df_modifiedx_,test_temp)
    p_ <- c(p_, test_temp$count)
  }  
}

#########################################################
### create submission File
#########################################################

sbmn <- read.csv('./submission/sampleSubmission (2).csv')
sbmn$count <- p_
write.csv(sbmn,'./submission/submission4.csv',row.names=FALSE)


