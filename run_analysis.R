## Name: run_analysis.R
## Course project 02/03/2019

##Task:
##The data linked to from the course website represent data collected from the accelerometers 
##from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

##   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## Here are the data for the project:

##   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

##You should create one R script called run_analysis.R that does the following.

## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##Activity labels

##1 WALKING
##2 WALKING_UPSTAIRS
##3 WALKING_DOWNSTAIRS
##4 SITTING
##5 STANDING
##6 LAYING

##download and unzip file 
##first check if the file exists in data
if(!file.exists("./data")){dir.create("./data")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/ProjectData.zip")

## Unzip Project Data to /data directory
unzip(zipfile="./data/ProjectData.zip",exdir="./data")

##check files in data
list.files("./data")

##check files in UCI HAR Dataset folder
list.files("./data/UCI HAR Dataset")

##check files in each "test" and "train" folders
list.files("./data/UCI HAR Dataset/test")
list.files("./data/UCI HAR Dataset/train")
##create a dataframe for both files

##create data tables for each set "test" and "train"
##there are 3 txt files in each set 

##read table for test set
testDataSubject <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
testX <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
testY <- read.table("./data/UCI HAR Dataset/test/y_test.txt")

##read table for train set
trainDataSubject <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
trainX <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
trainY <- read.table("./data/UCI HAR Dataset/train/y_train.txt")

##then we read activity labels and featured
activityLabelsData <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
featuresData <- read.table("./data/UCI HAR Dataset/features.txt")

##out of curiosity let's check the names and head
names(featuresData)
## [1] "V1" "V2"
names(activityLabelsData)
##[1] "V1" "V2"

head(featuresData)
##V1                V2
##1  1 tBodyAcc-mean()-X
##2  2 tBodyAcc-mean()-Y
##3  3 tBodyAcc-mean()-Z
##4  4  tBodyAcc-std()-X
##5  5  tBodyAcc-std()-Y
##6  6  tBodyAcc-std()-Z

head(activityLabelsData)
##V1                 V2
##1  1            WALKING
##2  2   WALKING_UPSTAIRS
##3  3 WALKING_DOWNSTAIRS
##4  4            SITTING
##5  5           STANDING
##6  6             LAYING

## Now we need to assign activity names
##there are variables 1 and 2 in activity labels table, let's assign names to them
colnames(activityLabelsData) <- c("activityID", "activityType")
##check the names
names(activityLabelsData)
names(testX)
names(testY)

##Appropriately label the data set with descriptive variable names.

##we assign names to test set
colnames(testDataSubject) <-"subjectID"
colnames(testY) <-"activityID"
colnames(testX) <- featuresData[,2]

##we assign names to train set
colnames(trainDataSubject) <-"subjectID"
colnames(trainY) <-"activityID"
colnames(trainX) <- featuresData[,2]

##view the data sets
View(testX)
View(trainX)

##merging data in one data set
mergedTest <- cbind(testY, testDataSubject, testX)
mergedTrain <- cbind(trainY, trainDataSubject, trainX)
FinalData <- rbind(mergedTest, mergedTrain)

##view merged data
View(mergedTrain)
View(mergedTest)
View(FinalData)

##descriptive activity names to name the activities in the data set
FinalDataNames <- merge(FinalData, activityLabelsData,
                        by='activityID',
                        all.x=TRUE)

## Extracts only the measurements on the mean and standard deviation for each measurement.
## set colNames
colNames <- colnames(FinalDataNames)
##use grepl to identify necessary names (returns TRUE/FALSE vector)
meanstd <- (grepl("activityId" , colNames) | 
                grepl("subjectId" , colNames) | 
                grepl("mean.." , colNames) | 
                grepl("std.." , colNames) |
                grepl("activityType", colNames)
)

##we will subset the data with mean and standard deviation for each measurement
FinalDataMS <- FinalDataNames[ , meanstd == TRUE]

##view the final data set
View(FinalDataMS)

## From the data set in step 4, creates a second, independent tidy data set with the average 
## of each variable for each activity and each subject.

## create second dataset using aggregate, then order it
## remember to use FinalDataNames, not the FinaldataMS

SecondDataSet <- aggregate(. ~subjectID + activityID, FinalDataNames, mean)
SecondDataSet <- SecondDataSet[order(SecondDataSet$subjectID, SecondDataSet$activityID),]

## View the results
View(SecondDataSet)

##write it as a txt file
write.table(SecondDataSet, "SecondDataSet.txt", row.name=FALSE)

##check that the file is there
list.files("./data/UCI HAR Dataset")