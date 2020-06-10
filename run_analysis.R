#Jon Hopkins week4 - Cleaning data-Final assignemnt

#####Loading required packages#####
library(dplyr)


#####Download the dataset#####
filename <- "Coursera_DS3_Final.zip"

# Checking if archieve already exists.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

#####Creating data frames#####
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


#Step 1: Merges the training and the test sets to create one data set
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
AllData <- cbind(Subject, Y, X)

#Step 2: Extracts only the measurements on the mean and standard deviation 
#for each measurement.
AllDataMeanStd <- AllData %>% select(subject, code, contains("mean"), contains("std"))

#Step 3: Uses descriptive activity names to name the activities in the 
#data set.
AllDataMeanStd$code <- activities[AllDataMeanStd$code, 2]

#Step 4: Appropriately labels the data set with descriptive variable names.
names(AllDataMeanStd)[1] = "Subject"
names(AllDataMeanStd)[2] = "Activity"
names(AllDataMeanStd)<-gsub("Acc", "Accelerometer", names(AllDataMeanStd))
names(AllDataMeanStd)<-gsub("Gyro", "Gyroscope", names(AllDataMeanStd))
names(AllDataMeanStd)<-gsub("BodyBody", "Body", names(AllDataMeanStd))
names(AllDataMeanStd)<-gsub("Mag", "Magnitude", names(AllDataMeanStd))
names(AllDataMeanStd)<-gsub("^t", "Time", names(AllDataMeanStd))
names(AllDataMeanStd)<-gsub("^f", "Frequency", names(AllDataMeanStd))
names(AllDataMeanStd)<-gsub("tBody", "TimeBody", names(AllDataMeanStd))
names(AllDataMeanStd)<-gsub("-mean()", "Mean", names(AllDataMeanStd), ignore.case = TRUE)
names(AllDataMeanStd)<-gsub("-std()", "StdDev", names(AllDataMeanStd), ignore.case = TRUE)
names(AllDataMeanStd)<-gsub("-freq()", "Frequency", names(AllDataMeanStd), ignore.case = TRUE)
names(AllDataMeanStd)<-gsub("angle", "Angle", names(AllDataMeanStd))
names(AllDataMeanStd)<-gsub("gravity", "Gravity", names(AllDataMeanStd))
TidyDf1<-AllDataMeanStd #tidy dataframe 1 with all means and std dev

#Step 5: From the data set in step 4, creates a second, independent tidy data
#set with the average of each variable for each activity and each subject.
SumAllDataMeanStdTemp<- group_by(AllDataMeanStd, Subject, Activity)
SumAllDataMeanStd<-summarise_all(SumAllDataMeanStdTemp, funs(mean))
TidyDf2<-SumAllDataMeanStd #tidy dataframe 2 with all means and std dev summarized by subject and activity

#Tidy data set created in step 5 created with write.table() using row.name=FALSE
write.table(TidyDf2, "FinalDf.txt", row.name=FALSE) #this is the final dataframe
