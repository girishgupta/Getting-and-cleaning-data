#loading required libraries
library(RCurl)
library(plyr)
library(stringr)

url = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
filename = 'getdata-projectfiles-UCI HAR Dataset.zip';

#Check if file already exits else download it
if(!file.exists(filename)) {

  download.file(url, destfile='getdata-projectfiles-UCI HAR Dataset.zip', method = 'curl')
  
}

#Unzip to start with fresh files
unzip(filename)

#Changing work directory to unzip folder for easy access.
setwd('UCI HAR Dataset')

#Reading activity labels
activityLabels = read.table('activity_labels.txt')

#Setting proper column headers for activity table to enable join later
names(activityLabels) = c('activityCode','activityDes')

#Reading feature labels in R
featureLabel = read.table('features.txt')
names(featureLabel) = c('featureCode','feature')


# Reading the test data into R
testdata_x <- read.table('./test/X_test.txt');

testdata_y <- read.table('./test/y_test.txt');
names(testdata_y) = 'activityCode'

subject_test <- read.table('./test/subject_test.txt');
names(subject_test) <- 'subject'

# Reading train data into R
traindata_x <- read.table('./train/X_train.txt');

traindata_y <- read.table('./train/y_train.txt');
names(traindata_y) = 'activityCode'

subject_train <- read.table('./train/subject_train.txt');
names(subject_train) <- 'subject'

                        

testData<-cbind(testdata_x,testdata_y,subject_test);
trainData<-cbind(traindata_x,traindata_y,subject_train);
completeData <- rbind(testData,trainData);

#Include activity decriptive names
completeData <- join(completeData,activityLabels);

#cleaning feature names
dummyCol = featureLabel$feature;

#Changing angle (x,y) to angle_between_x_and_y
dummyCol = gsub('\\(([a-z])(.*)\\,([a-z])(.*)\\)','_between_\\1\\2_and_\\3\\4',dummyCol);
#Changing abc-mean to abcMean 
dummyCol = gsub('-([a-z])(.*)','\\U\\1\\E\\2',dummyCol,perl=T)
#Changing Mean()-1,2 to MeanFor1,2 
dummyCol = gsub('\\(\\)-','For',dummyCol);
#Changing For1,16 to For1To16
dummyCol = gsub('([0-90-9])(.*)\\,([0-90-9])(.*)','\\1\\2To\\3\\4',dummyCol)
#Removing leftover -
dummyCol = gsub('-','For',dummyCol)
#Removing leftover ( and )
dummyCol = gsub('\\(',"",dummyCol)
dummyCol = gsub('\\)',"",dummyCol)
#Removing leftover commas
dummyCol = gsub('\\,','And',dummyCol)

featureLabel$feature = dummyCol

#Adding extra columns for compatibility
extraCol = data.frame(c(562:564),c('activityCode','subject','activityDes'))
names(extraCol) = names(featureLabel)
#combine two
featureLabel = rbind(featureLabel,extraCol)


#Assign the descriptive names
names(completeData) = featureLabel$feature;

#Remove extra column on activity column
completeData$activityCode = NULL

rm(dummyCol,extraCol)

#Getting colums related to mean and Std
meanStdDataset = completeData[,grep('([Mm]ean|[Ss]td)+',names(completeData))]

