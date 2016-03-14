

#Here are the data for the project:

#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#You should create one R script called run_analysis.R that does the following.
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names.
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
#download.file(url, destfile="./personaldata.csv", method = "curl")
#manually unzip

#Merges the training and the test sets to create one data set.
      #read in file with descriptive names and make list, make a sublist by searching for mean or std
      DT <- read.table("./UCI HAR Dataset 2/features.txt")
      names <- data.frame(col_id = DT[,1],names = DT[,2])
      count(names)#561
      subnames <- grep(paste(c("std","mean[^freq]*$"),collapse="|"), names$names,value=F)
      subnames_str <- grep(paste(c("std","mean[^freq]*$"),collapse="|"), names$names,value=TRUE)
      subnames_str <- c(subnames_str,"subject", "activity")
#Extracts only the measurements on the mean and standard deviation for each measurement.
      #read in the training and test files, combine them, then make a dataframe of columns corresponding to subnames
      x_train_data <- read.table("./UCI HAR Dataset 2/train/X_train.txt")
      subject_train_data <- read.table("./UCI HAR Dataset 2/train/subject_train.txt")
      y_train_data <- read.table("./UCI HAR Dataset 2/train/y_train.txt")

      x_test_data <- read.table("./UCI HAR Dataset 2/test/X_test.txt")
      y_test_data <- read.table("./UCI HAR Dataset 2/test/y_test.txt")
      subject_test_data <- read.table("./UCI HAR Dataset 2/test/subject_test.txt")

      combined_subject <- rbind(subject_train_data, subject_test_data)
      combined_excercise <- rbind(y_train_data, y_test_data)
      combined_test_data <- rbind(x_train_data,x_test_data)

      head(combined_data)
      #new dataframes consisting of only columns picked by mean or std
      clean_test_data <-data.frame(sapply(subnames, function(x) combined_test_data[,x]))

      clean_data <- cbind( clean_test_data, combined_subject, combined_excercise )

      #attach names to clean_data columns from data.frame-names
      colnames(clean_data) <-  subnames_str #name the columns

      #rename activity number to name
      for (i in 1:nrow(clean_data)) {
            if (clean_data$activity[i] == "1") {
                  clean_data$activity[i]= "WALKING"
            } else if (clean_data$activity[i] =="2") {
                  clean_data$activity[i]= "WALKING_UPSTAIRS"
            } else if (clean_data$activity[i] =="3") {
                  clean_data$activity[i]= "WALKING_DOWNSTAIRS"
            } else if (clean_data$activity[i] =="4") {
                  clean_data$activity[i]= "SITTING"
            } else if (clean_data$activity[i] =="5") {
                  clean_data$activity[i]= "STANDING"
            } else if (clean_data$activity[i] =="6") {
                  clean_data$activity[i]= "LAYING"
            }
      }

      names(clean_data)

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
      library(dplyr)
      #ncol(clean_data)
      avedata <-
            clean_data %>%
            group_by(subject, activity) %>%
            summarise_each(funs(mean))
      write.csv(avedata, file = "independent_tidy_data_set.csv")
      write.table(avedata, file = "independent_tidy_data_set.txt", row.name=FALSE)
