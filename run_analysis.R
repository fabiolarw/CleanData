  ##You should create one R script called run_analysis.R that does the following.
  
  ##1. Merges the training and the test sets to create one data set.
  ##2. Extracts only the measurements on the mean and standard deviation for each measurement.
  ##3. Uses descriptive activity names to name the activities in the data set
  ##4. Appropriately labels the data set with descriptive variable names.
  ##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  
  ##load libraries
  library(dplyr)
  library(reshape2)
  
  ##The files are downloaded and unzipped
  if(!file.exists("runRaw")) { dir.create("runRaw")}
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl, destfile="./runRaw/run.zip", method="curl")
  
  if (!file.exists("runData")) {
    dir.create("runData")
    unzip("./runRaw/run.zip", files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = "runData", unzip = "internal",
        setTimes = FALSE)
  }
  dateDownloaded=date()
  
  ## 1. Merge phase
  ## read all tables
  x_train<- read.table("./runData/UCI HAR Dataset/train/X_train.txt")
  y_train<- read.table("./runData/UCI HAR Dataset/train/y_train.txt")
  s_train<- read.table("./runData/UCI HAR Dataset/train/subject_train.txt")
  
  x_test<- read.table("./runData/UCI HAR Dataset/test/X_test.txt")
  y_test<- read.table("./runData/UCI HAR Dataset/test/y_test.txt")
  s_test<- read.table("./runData/UCI HAR Dataset/test/subject_test.txt")
  
  features<- read.table("./runData/UCI HAR Dataset/features.txt", col.names=c("featureNumber", "functions"))
  activities <- read.table("./runData/UCI HAR Dataset/activity_labels.txt", col.names = c("activityNumber", "activityName"))
  
  #merging data
  merge_x<- rbind(x_train, x_test)
  merge_y<- rbind(y_train, y_test)
  merge_s<- rbind(s_train, s_test)
  
  ##2. Extract mean and standard deviation
  selectedColumns <- grep("-(mean|std).*", as.character(features[,2]))
  selectedColNames<-features[selectedColumns, 2]
  
  merge_x <- merge_x[selectedColumns]
  merge_all<- cbind(merge_s, merge_y, merge_x)
  colnames(merge_all) <- c("Subject", "Activity", selectedColNames)
  
  ##3. Descriptive activity names
  merge_all$Activity <-activities$activityName[merge_all$Activity]
  
  ##4. Descriptive variable names
  names(merge_all) <-gsub("Acc", " acceleration", names(merge_all))
  names(merge_all) <-gsub("Gyro", " angular acceleration", names(merge_all))
  names(merge_all) <-gsub("Jerk", " jerk", names(merge_all))
  names(merge_all) <-gsub("BodyBody", "Body", names(merge_all))
  names(merge_all) <-gsub("Mag", " Magnitude", names(merge_all))
  names(merge_all) <-gsub("std", "standard deviation", names(merge_all))
  names(merge_all) <-gsub("^t", "(Time domain) ", names(merge_all))
  names(merge_all) <-gsub("^f", "(Frequency domain) ", names(merge_all))
  names(merge_all) <-gsub("X$", "X axis", names(merge_all))
  names(merge_all) <-gsub("Y$", "Y axis", names(merge_all))
  names(merge_all) <-gsub("Z$", "Z axis", names(merge_all))
  
  ##5. Final tidy set
  meltedData <- melt(merge_all, id = c("Subject", "Activity"))
  finalTidyData <- dcast(meltedData, Subject + Activity ~ variable, mean)
  write.table(finalTidyData, "./finalTidyData.txt", row.names = FALSE, quote = FALSE)
  
  ##Create codebook
  library(codebook)
  cb <- codebook(finalTidyData, survey_repetition = "single", metadata_table = FALSE)