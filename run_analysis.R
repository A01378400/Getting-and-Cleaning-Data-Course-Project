##Preparation
## You are going to need to use the dplyr package, 
## so it is important to load it up. 

library(dplyr)

##The next step is to download the file.
##But first we are going to check if the file has already been downloaded. 

##We assign the name of the data set to a variable.
filename <- "Coursera_DS3_Final.zip"


#We check if the file has not been downloaded.
if (!file.exists(filename)){
        
        ##The download URL is assigned.
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        
        ##The file is downloaded.
        download.file(fileURL, filename, method="curl")
}  

##The file comes in a Zip fil, so we have to un-zip it.

#We check if the folder folder exists.
if (!file.exists("UCI HAR Dataset")) { 
        unzip(filename) 
}



##Variable Assigment

##The next step is to assign all the data frames to a variable name.
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))

activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)

y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)

y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")



## Step 1

##Now that the data is downloaded and sorted is time to merge the different
##variables into one large data set. 

##We first merge the training and the test sets to create one data set.
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)

Subject <- rbind(subject_train, subject_test)

Merged_Data <- cbind(Subject, Y, X)


##Step 2

##Then we need to extract the measurements on the mean and 
## the standard deviation for each measurement

TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))



##Step 3

##Descriptive activity names are used to name the activities in the data set.

TidyData$code <- activities[TidyData$code, 2]


##Step 4

## It is important to appropriately labels the data set with 
## descriptive variable names.

names(TidyData)[2] = "activity"

names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))

names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))

names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))

names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))

names(TidyData)<-gsub("^t", "Time", names(TidyData))

names(TidyData)<-gsub("^f", "Frequency", names(TidyData))

names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))

names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)

names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)

names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)

names(TidyData)<-gsub("angle", "Angle", names(TidyData))

names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))


##Step 5

## The last stpe is to create a second, independent tidy data set 
## with the average of each variable for each activity and each subject, 
## from the data set in step 4.

FinalData <- TidyData %>%
        group_by(subject, activity) %>%
        summarise_all(funs(mean))

write.table(FinalData, "FinalData.txt", row.name=FALSE)


##At this point we can check our tidy data

str(FinalData)


## The final data is stored here:

FinalData




