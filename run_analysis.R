#Adjust directory
getwd()
setwd("/Users/Tu/Desktop/R")

#download file
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile = "./data/dataset.zip")
unzip(zipfile="./data/dataset.zip",exdir="./data")

# directory to data folder
path_f <- file.path("./data" , "UCI HAR Dataset")

#importing data
Act_test<-read.table(file.path(path_f, "test" , "Y_test.txt" ),header = FALSE)
Act_train<-read.table(file.path(path_f, "train" , "Y_train.txt" ),header = FALSE)

Sub_train <- read.table(file.path(path_f, "train", "subject_train.txt"),header = FALSE)
Sub_test  <- read.table(file.path(path_f, "test" , "subject_test.txt"),header = FALSE)

Feat_test  <- read.table(file.path(path_f, "test" , "X_test.txt" ),header = FALSE)
Feat_train <- read.table(file.path(path_f, "train", "X_train.txt"),header = FALSE)

# 1. Merging data set (trainings and tests)

Act_data<-rbind(Act_test,Act_train)
Sub_data<-rbind(Sub_test,Sub_train)
Feat_data<-rbind(Feat_test,Feat_train)

  #name the variables
  names(Sub_data)<-c("subject")
  names(Act_data)<- c("activity")
  Feat_names <- read.table(file.path(path_f, "features.txt"),head=FALSE)
  names(Feat_data)<- Feat_names$V2
  #Merge columns for final data
  dataCombine <- cbind(Sub_data, Act_data)
  Data <- cbind(Feat_data, dataCombine)
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
  subFeat_names<-Feat_names$V2[grep("mean\\(\\)|std\\(\\)", Feat_names$V2)]

#subset the data frame "data"  by selected names of features
  selectedNames<-c(as.character(subFeat_names), "subject", "activity" )
  Data<-subset(Data,select=selectedNames)
#3. Uses descriptive activity names to name the activities in the data set
  #Read descriptive activity names from “activity_labels.txt”
  act_labels <- read.table(file.path(path_f, "activity_labels.txt"),header = FALSE)
  #Merge activity name
  library(dplyr)
  Data1=left_join(Data, act_labels, by = c("activity"="V1"))
  Data<-Data1%>% select(-activity) %>% rename(activity=V2)
#4. Appropriately labels the data set with descriptive variable names
  names(Data)<-gsub("^t", "time", names(Data))
  names(Data)<-gsub("^f", "frequency", names(Data))
  names(Data)<-gsub("Acc", "Accelerometer", names(Data))
  names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
  names(Data)<-gsub("Mag", "Magnitude", names(Data))
  names(Data)<-gsub("BodyBody", "Body", names(Data))
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  Data2<-aggregate(. ~subject + activity, Data, mean)
  Data2<-Data2[order(Data2$subject,Data2$activity),]
  write.table(Data2, file = "tidydata.txt",row.name=FALSE)