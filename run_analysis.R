library(dplyr)
library(stats)

rm(list = ls())

# Pre-processing 1 - Getting the data from URL -----------------------

currentDirectory <- getwd()

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileName <- "UCI HAR Dataset.zip"

# for debugging: do not redownload the file if it's already downloaded
if(!file.exists(fileName))
{
  download.file(fileURL, fileName, mode = "wb")
}

#unzip the downloaded file to read data

extractedDirectoryname <- "UCI HAR Dataset"

if (!dir.exists(extractedDirectoryname)) {
    unzip(fileName);
}

# Pre-processing 2 - Reading the data ----------------------------

#read test set 
testSetSubjects <- read.table(file.path(extractedDirectoryname, "test", "subject_test.txt"))
testSetActivities <- read.table(file.path(extractedDirectoryname, "test", "y_test.txt"))
testSetValues <- read.table(file.path(extractedDirectoryname, "test", "x_test.txt"))

#read train set 
trainSetSubjects <- read.table(file.path(extractedDirectoryname, "train", "subject_train.txt"))
trainSetActivities <- read.table(file.path(extractedDirectoryname, "train", "y_train.txt"))
trainSetValues <- read.table(file.path(extractedDirectoryname, "train", "x_train.txt"))

#read features
features <- read.table(file.path(extractedDirectoryname, "features.txt"), as.is = TRUE)

# read activity labels
activities <- read.table(file.path(extractedDirectoryname, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

# STEP 1 Merges the training and the test sets to create one data set -------------------

testDataset <- cbind(testSetSubjects, testSetValues, testSetActivities)
trainDataset <- cbind(trainSetSubjects, trainSetValues, trainSetActivities)

combinedDataset <- rbind(testDataset, trainDataset)

colnames(combinedDataset) <- c("Subject", features[, 2], "Activity")

# discard other tables to save memory
rm(trainSetActivities, trainSetSubjects, trainSetValues,
   testSetActivities, testSetSubjects, testSetValues,
   testDataset, trainDataset)


# STEP 2 Extracts only the measurements on the mean and standard deviation for each measurement -------------

# extract subject, activity, mean, std. dev columns

columns <- grepl("Subject|Activity|mean|std", colnames(combinedDataset))
combinedDataset <- combinedDataset[, columns]
rm(columns) # discard in memory after being used

# STEP 3 Uses descriptive activity names to name the activities in the data set ---------------

# replace activities with named factor levels
combinedDataset$Activity <- factor(combinedDataset$Activity, levels = activities[, 1], labels = activities[, 2])

# STEP 4 Appropriately labels the data set with descriptive variable names ---------------------

activityColumns <- colnames(combinedDataset)

# remove special characters on all columns
activityColumns <- gsub("[\\(\\)-]", "", activityColumns)

# clean column name
activityColumns <- gsub("^f", "frequencyDomain", activityColumns)
activityColumns <- gsub("^t", "timeDomain", activityColumns)
activityColumns <- gsub("Acc", "Accelerometer", activityColumns)
activityColumns <- gsub("Gyro", "Gyroscope", activityColumns)
activityColumns <- gsub("Mag", "Magnitude", activityColumns)
activityColumns <- gsub("Freq", "Frequency", activityColumns)
activityColumns <- gsub("mean", "Mean", activityColumns)
activityColumns <- gsub("std", "StandardDeviation", activityColumns)
activityColumns <- gsub("BodyBody", "Body", activityColumns)

colnames(combinedDataset) <- activityColumns
rm(activityColumns)


# STEP 5 creates a second, independent tidy data set with the average of each variable for each activity and each subject --------------

# group by subject and activity and summarise using mean
combinedDataset <- combinedDataset %>%
    group_by(Subject, Activity) %>%
    summarise_each(funs(mean))

# write the output to a file
write.table(combinedDataset, "tidy_data.txt", row.names = FALSE,
            quote = FALSE)