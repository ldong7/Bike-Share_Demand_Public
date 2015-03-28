__author__ = 'L. Dong, S. Rea, D. Kuo'


# import libraries
from pyspark import SparkContext, SparkConf
from pyspark.mllib.regression import LabeledPoint, LinearRegressionWithSGD
from pyspark.mllib.tree import RandomForest
from pyspark.mllib.tree import DecisionTree
from pyspark.mllib.stat import Statistics
import numpy as np


# functions to Load and parse the data
def parsePoint(line):
    values = [float(x) for x in line.split(',')]
    return LabeledPoint(values[0], values[1:])

def parseVec(line):
    values = [float(x) for x in line.split(',')]
    return values


# define your "username" and "password" (need to use your own)
AWS_ACCESS_KEY_ID='usernameusernameusernameusername'
AWS_SECRET_ACCESS_KEY='passwordpasswordpasswordpassword'


# initialize a spark context
sc = SparkContext()

# load the training data from your AWS S3 bucket and repartition the data into 40 datasets and keep them in memory
traindata = sc.textFile("s3n://"+AWS_ACCESS_KEY_ID+":"+AWS_SECRET_ACCESS_KEY+"@sparkec2/train.csv").repartition(40).cache()
# use map operation to map the all rows into a vector
trainvecData = traindata.map(parseVec)
# use map operation to map the first column of each row to be the label, and the rest into a vector, combined they become a tuple called LabelPoint in Spark
trainparsedData = traindata.map(parsePoint)

# load the test data from your AWS S3 bucket and repartition the data into 40 datasets and keep them in memory
testdata = sc.textFile("s3n://"+AWS_ACCESS_KEY_ID+":"+AWS_SECRET_ACCESS_KEY+"@sparkec2/test.csv").repartition(40).cache()
# use map operation to map the all rows into a vector
testvecData = testdata.map(parseVec)
# use map operation to map the first column of each row to be the label, and the rest into a vector, combined they become a tuple called LabelPoint in Spark
testparsedData = testdata.map(parsePoint)


	
# Train a RandomForest model.
#  Empty categoricalFeaturesInfo indicates all features are continuous.
#  numTres is the number of tree used in the model
#  Setting featureSubsetStrategy="auto" lets the algorithm choose what feature each tree use
#  impurity is variance for regression
#  maxDepth is the maximum depth of each tree
model1 = RandomForest.trainRegressor(trainparsedData
									, categoricalFeaturesInfo={}
									, numTrees=1000
									, featureSubsetStrategy="auto"
									, impurity='variance'
									, maxDepth=13
									, maxBins=32)


# evaluate the training error
# first make the prediction and create a new "vector" of all the predictions
trainpredictions = model1.predict(trainparsedData.map(lambda x: x.features))
# then you column bind the prediction and actual values into a new RDD
trainlabelsAndPredictions = trainparsedData.map(lambda lp: lp.label).zip(trainpredictions)
# use map operation to compute MSE
trainMSE1 = trainlabelsAndPredictions.map(lambda (v, p): (v - p) * (v - p)).sum() / float(trainparsedData.count())

# use the the Statistics library to obtain the variance
summary = Statistics.colStats(trainvecData)
variance = summary.variance()[0]
# compute the pseudo R-square
train_Rsqr1 = 1 - trainMSE1/float(variance)


# evaluate the testing error
# first make the prediction and create a new "vector" of all the predictions
testpredictions = model1.predict(testparsedData.map(lambda x: x.features))
# then you column bind the prediction and actual values into a new RDD
testlabelsAndPredictions = testparsedData.map(lambda lp: lp.label).zip(testpredictions)
# use map operation to compute MSE
testMSE1 = testlabelsAndPredictions.map(lambda (v, p): (v - p) * (v - p)).sum() / float(testparsedData.count())

# use the the Statistics library to obtain the variance
summary = Statistics.colStats(testvecData)
variance = summary.variance()[0]
# compute the pseudo R-square
test_Rsqr1 = 1 - testMSE1/float(variance)





# Train a DecisionTree model.
# Empty categoricalFeaturesInfo indicates all features are continuous.
# use variance as impurity for regression
# maxDepth is the maximum number of level for each tree
model2 = DecisionTree.trainRegressor(trainparsedData
									, categoricalFeaturesInfo={}
									, impurity='variance'
									, maxDepth=8
									, maxBins=32)


# evaluate the training error
# first make the prediction and create a new "vector" of all the predictions
trainpredictions = model2.predict(trainparsedData.map(lambda x: x.features))
# then you column bind the prediction and actual values into a new RDD
trainlabelsAndPredictions = trainparsedData.map(lambda lp: lp.label).zip(trainpredictions)
# use map operation to compute MSE
trainMSE2 = trainlabelsAndPredictions.map(lambda (v, p): (v - p) * (v - p)).sum() / float(trainparsedData.count())

# use the the Statistics library to obtain the variance
summary = Statistics.colStats(trainvecData)
variance = summary.variance()[0]
# compute the pseudo R-square
train_Rsqr2 = 1 - trainMSE2/float(variance)


# evaluate the testing error
# first make the prediction and create a new "vector" of all the predictions
testpredictions = model2.predict(testparsedData.map(lambda x: x.features))
# then you column bind the prediction and actual values into a new RDD
testlabelsAndPredictions = testparsedData.map(lambda lp: lp.label).zip(testpredictions)
# use map operation to compute MSE
testMSE2 = testlabelsAndPredictions.map(lambda (v, p): (v - p) * (v - p)).sum() / float(testparsedData.count())

# use the the Statistics library to obtain the variance
summary = Statistics.colStats(testvecData)
variance = summary.variance()[0]
# compute the pseudo R-square
test_Rsqr2 = 1 - testMSE2/float(variance)



# print all the results
print 'random forest:'
print 'Train Mean Squared Error = ' + str(trainMSE1)
print 'train R-Sqare = ' + str(train_Rsqr1)

print 'Test Mean Squared Error = ' + str(testMSE1)
print 'test R-Sqare = ' + str(test_Rsqr1)



print 'decision tree:'
print 'Train Mean Squared Error = ' + str(trainMSE2)
print 'train R-Sqare = ' + str(train_Rsqr2)

print 'Test Mean Squared Error = ' + str(testMSE2)
print 'test R-Sqare = ' + str(test_Rsqr2)


