library(stringr)
#### Function to find and replace a pattern in data frame . Will be used for Step3 later.
replace_all <- function(df, pattern, replacement) 
{
  char <- vapply(df, function(x) is.factor(x) || is.character(x), logical(1))
  df[char] <- lapply(df[char], str_replace_all, pattern, replacement)  
  df
}


run_analysis <- function(dir=NULL)
{
  ######################### Program initiation task to set up the environment ##################################### 
  ## Importing packages
  library(data.table)
  library(reshape2)
  library(proto)
  library(DBI)
  library(RSQLite)
  library(Cairo)
  library(tcltk2)
  library(gsubfn)
  library(sqldf)
  ## Downloading and extracting data from the zip file
  ## Setting up the landing directory and file extraction url
  fileurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip?accessType=DOWNLOAD"
  if(is.null(dir)) dir<-paste(getwd(),"/",sep="")
  landdir<-dir
  #landdir<-"/Users/malaydas/Documents/Data Science/Getting and Cleaning Data/ProgrammingAssignment/"
  setwd(landdir)
  ## Dowloading the file at landing directory
  download.file(fileurl,destfile="./Dataset.zip",method="curl")
  unzip("Dataset.zip")
  ## Setting up working directory and other directory available inside the data dump
  workdir<-paste(landdir,"UCI HAR Dataset/",sep="")
  #workdir<-"/Users/malaydas/Documents/Data Science/Getting and Cleaning Data/ProgrammingAssignment/UCI HAR Dataset/"
  setwd(workdir)
  testdir<-"./test/"
  traindir<-"./train/"
  
  
  ### Step 1 of the assignment - "Merges the training and the test sets to create one data set"
  ######################### Actual file merging and tidy data creation steps #####################################
  ## Reading Files
  ### 1.a features.txt
  features<-read.table("features.txt")
  x<-as.vector(features[,2])
  ## Transposing the feature name to column , so that it can fit the final dataset as header
  ## Also adding two extra column for subject and activity
  header<-t(x)
  h<-append(paste("V",c(1:561),sep=''),c("subject","activity"))
  header<-cbind(header,c("subject"))
  header<-cbind(header,c("activity"))
  colnames(header)<-h
 
  ## 1.b
  ## 1.b.1 -Reading test files - X_test.txt
  setwd(testdir)
  xtest<-read.table("X_test.txt")
  ## Reading subject files - subject_test.txt - to extract subject
  subject<-read.table("subject_test.txt")
  colnames(subject)<-c("subject")
  ## Reading activity files - y_test.txt - to extract subject's activity for test sample
  y<-read.table("y_test.txt")
  colnames(y)<-c("activity")
  ## Adding subject and activity to test data.frame
  xtest<-cbind(xtest,data.frame(subject))
  xtest<-cbind(xtest,data.frame(y))

  
  ## 1.b.2 - Reading train files
  setwd("..")
  setwd(traindir)
  xtrain<-read.table("X_train.txt")
  ## Reading subject files - subject_train.txt - to extract subject
  subject<-read.table("subject_train.txt")
  colnames(subject)<-c("subject")
  ## Reading activity files - y_train.txt - to extract subject's activity for train sample
  y<-read.table("y_train.txt")
  colnames(y)<-c("activity")
  ## Adding subject and activity to train data.frame
  xtrain<-cbind(xtrain,data.frame(subject))
  xtrain<-cbind(xtrain,data.frame(y))
  
  setwd("..")
  setwd(workdir)
  
  ## 1.c
  ## Creating final dataset with test and train dataset created earlier
  finaldf<-rbind(xtest,xtrain)
  ## Adding the header created from features.txt and assigning it as rowname
  colnames(finaldf)<-as.vector(header)

  
  ## 1.d
  ## Reading activity_label.txt
  activitylabel<-read.table("activity_labels.txt")
  colnames(activitylabel)<-c("activity","activity_label")
  mergeddata<-merge(finaldf,activitylabel,by.x="activity",by.y="activity")
  
  
  ### Step 2 of the assignment - "Extracts only the measurements on the mean and standard deviation for each measurement"
  ## Finding variable with mean() and std() at the end of it , I am not picking variable where mean/std middle of the name
  mnstd<-grep("mean\\(\\)|std\\(\\)",colnames(mergeddata),ignore.case=TRUE)
  mnstdfinal<-append(grep("subject|activity|activity_label",colnames(mergeddata),ignore.case=TRUE),mnstd)
  ## Extracting mean & std data
  mnstddata<-mergeddata[,mnstdfinal]
  
  
  ### Step-3 of assignment - 
  ### "Uses descriptive activity names to name the activities in the data set"
  ### Personally i found activity label to be decently named , hence only changing case and removing '_'s
  submission31<-replace_all(mnstddata,"WALKING_UPSTAIRS","Walking Upstairs")
  submission32<-replace_all(submission31,"WALKING_DOWNSTAIRS","Walking Downstairs")
  submission33<-replace_all(submission32,"WALKING","Walking")
  submission34<-replace_all(submission33,"STANDING","Standing")
  submission35<-replace_all(submission34,"LAYING","Laying")
  submission3<-replace_all(submission35,"SITTING","Seating")
  
  
  ### Step-4 of assignment - 
  ### "Appropriately labels the data set with descriptive variable names"
  ###  Personally i felt the rownames are already descriptive enough , since this is part of assignment hence only time for t 
  ###  and Freq for f ; also at the same time removing brackets
  #### Extracting column name to format 
  cn<-colnames(submission3)
  #### Replacing ^t with time , ^f with freq and () with ..
  cntime<-replace_all(cn,"^t","time")
  cnfreq<-replace_all(cntime,"^f","freq")
  cnfinal<-replace_all(cnfreq,"\\(\\)","")
  colnames(submission3)<-cnfinal
  
  ### Step-5 of assignment - 
  ### "From the data set in step 4, creates a second, inndependent tidy data set with the average of each variable for each activity and each subject"
  ###  Using sqldf to calculate avg for all the measures . 
  #### Forming sql to extract the data
  sqlstr<-'select subject,activity,activity_label,
          avg("timeBodyAcc-mean-X"),avg("timeBodyAcc-mean-Y"),avg("timeBodyAcc-mean-Z"),avg("timeBodyAcc-std-X"),avg("timeBodyAcc-std-Y"),
          avg("timeBodyAcc-std-Z"),avg("timeGravityAcc-mean-X"),avg("timeGravityAcc-mean-Y"),avg("timeGravityAcc-mean-Z"),avg("timeGravityAcc-std-X"),
          avg("timeGravityAcc-std-Y"),avg("timeGravityAcc-std-Z"),avg("timeBodyAccJerk-mean-X"),avg("timeBodyAccJerk-mean-Y"),avg("timeBodyAccJerk-mean-Z"),
          avg("timeBodyAccJerk-std-X"),avg("timeBodyAccJerk-std-Y"),avg("timeBodyAccJerk-std-Z"),avg("timeBodyGyro-mean-X"),avg("timeBodyGyro-mean-Y"),
          avg("timeBodyGyro-mean-Z"),avg("timeBodyGyro-std-X"),avg("timeBodyGyro-std-Y"),avg("timeBodyGyro-std-Z"),avg("timeBodyGyroJerk-mean-X"),
          avg("timeBodyGyroJerk-mean-Y"),avg("timeBodyGyroJerk-mean-Z"),avg("timeBodyGyroJerk-std-X"),avg("timeBodyGyroJerk-std-Y"),avg("timeBodyGyroJerk-std-Z"),
          avg("timeBodyAccMag-mean"),avg("timeBodyAccMag-std"),avg("timeGravityAccMag-mean"),avg("timeGravityAccMag-std"),avg("timeBodyAccJerkMag-mean"),
          avg("timeBodyAccJerkMag-std"),avg("timeBodyGyroMag-mean"),avg("timeBodyGyroMag-std"),avg("timeBodyGyroJerkMag-mean"),avg("timeBodyGyroJerkMag-std"),
          avg("freqBodyAcc-mean-X"),avg("freqBodyAcc-mean-Y"),avg("freqBodyAcc-mean-Z"),avg("freqBodyAcc-std-X"),avg("freqBodyAcc-std-Y"),avg("freqBodyAcc-std-Z"),
          avg("freqBodyAccJerk-mean-X"),avg("freqBodyAccJerk-mean-Y"),avg("freqBodyAccJerk-mean-Z"),avg("freqBodyAccJerk-std-X"),avg("freqBodyAccJerk-std-Y"),avg("freqBodyAccJerk-std-Z"),
          avg("freqBodyGyro-mean-X"),avg("freqBodyGyro-mean-Y"),avg("freqBodyGyro-mean-Z"),avg("freqBodyGyro-std-X"),avg("freqBodyGyro-std-Y"),avg("freqBodyGyro-std-Z"),
          avg("freqBodyAccMag-mean"),avg("freqBodyAccMag-std"),avg("freqBodyBodyAccJerkMag-mean"),avg("freqBodyBodyAccJerkMag-std"),
          avg("freqBodyBodyGyroMag-mean"),avg("freqBodyBodyGyroMag-std"),avg("freqBodyBodyGyroJerkMag-mean"),avg("freqBodyBodyGyroJerkMag-std")
          from  submission3 group by activity,activity_label,subject order by subject,activity'
  submission5<-sqldf(sqlstr)
  ## renaming the header with Avg prefixed to every measure .
  oldcolnm<-colnames(submission3)
  newcolnm1<-replace_all(oldcolnm,"^time","AvgTime")
  newcolnm<-replace_all(newcolnm1,"^freq","AvgFreq")
  newcolnm[1]<-"subject"
  newcolnm[2]<-"activity"
  colnames(submission5)<-as.vector(newcolnm)
  
  ## creating file and subsequent writting it to the file. These is the ultimate data to be submitted for evaluation
  if(!file.exists("final.txt")) 
  {
    file.create("final.txt")
  }else{ 
    file.remove("final.txt")
    file.create("final.txt")
  }
  
  # Writting submission5 to file.
  write.table(submission5,file="final.txt",row.names=FALSE)
  setwd(landdir)
 
}

