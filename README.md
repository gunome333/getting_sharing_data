# getting_sharing_data
#course project- combining and cleaning human physical movement dataset and generating table of averages

#from class Here are the data for the project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#download file and manually unzip.-this script assumes data are stored in subdirectory /data
      url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
      download.file(url, destfile="./data/personaldata.csv", method = "curl")
      #manually unzip

#Merges the training and the test sets to create one data set.
#read in file containing the descriptive names and make list, 
      #make a sublist by searching for mean or std to be used to subset data and to name columns of dataset
      DT <- read.table("./data/UCI HAR Dataset 2/features.txt")
      names <- data.frame(col_id = DT[,1],names = DT[,2])
     
     #Extracts only the data of the mean and standard deviation for each measurement. (exclude mean frequency)
      count(names) #561
      subnames <- grep(paste(c("std","mean[^freq]*$"),collapse="|"), names$names,value=F) # by number
      subnames_str <- grep(paste(c("std","mean[^freq]*$"),collapse="|"), names$names,value=TRUE) # including names
      subnames_str <- c(subnames_str,"subject", "activity") # add 2 final columns names for subject and activity


#read in the training and test files, combine them, then make a dataframe of columns corresponding to subnames
      x_train_data <- read.table("./data/UCI HAR Dataset 2/train/X_train.txt")
      subject_train_data <- read.table("./data/UCI HAR Dataset 2/train/subject_train.txt")
      y_train_data <- read.table("./data/UCI HAR Dataset 2/train/y_train.txt")

      x_test_data <- read.table("./data/UCI HAR Dataset 2/test/X_test.txt")
      y_test_data <- read.table("./data/UCI HAR Dataset 2/test/y_test.txt")
      subject_test_data <- read.table("./data/UCI HAR Dataset 2/test/subject_test.txt")

      #combine  the training and test class data by type
      combined_subject <- rbind(subject_train_data, subject_test_data)
      combined_excercise <- rbind(y_train_data, y_test_data)
      combined_test_data <- rbind(x_train_data,x_test_data)

      #head(combined_data)
      #new dataframes consisting of only columns picked by mean or std of measurement data
      clean_test_data <-data.frame(sapply(subnames, function(x) combined_test_data[,x]))

      clean_data <- cbind( clean_test_data, combined_subject, combined_excercise )

#attach names to clean_data columns from data.frame-names
      colnames(clean_data) <-  subnames_str #name the columns

#rename activity class number to name
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

      #names(clean_data)

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
      #make a new df called avedata consisting of means of each measurement type grouped by subject and activity class
      library(dplyr)
      #ncol(clean_data)
      avedata <-
            clean_data %>%
            group_by(subject, activity) %>%
            summarise_each(funs(mean))
     
      #Output the files 
      write.csv(avedata, file = "independent_tidy_data_set.csv")
      write.table(avedata, file = "independent_tidy_data_set.txt", row.name=FALSE)
