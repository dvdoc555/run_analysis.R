## Copy this file into R, and there will be 2 tidy Data Sets in files labled 
## DataSet1 and DataSet2.  Both files will appear in your your working directory



## Download  data needed



fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,"Bugs.zip")
unzip("Bugs.zip")
unlink("Bugs.zip")

## Read in Data

y_Test<-read.table("UCI HAR Dataset/test/y_test.txt")
y_Train<-read.table("UCI HAR Dataset/train/y_train.txt")
y_all<-rbind(y_Test,y_Train)
rm(y_Test);rm(y_Train)
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt")
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt")
subject<-rbind(subject_test,subject_train)
rm(subject_test);rm(subject_train)

AccelorometerMeanX<-rbind(read.table(
    "UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt"),
    read.table("UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt"))
      
AccelorometerMeanY<-rbind(read.table(
  "UCI HAR Dataset/test/Inertial Signals/total_acc_Y_test.txt"),
  read.table("UCI HAR Dataset/train/Inertial Signals/total_acc_Y_train.txt"))

AccelorometerMeanZ<-rbind(read.table(
  "UCI HAR Dataset/test/Inertial Signals/total_acc_Y_test.txt"),
  read.table("UCI HAR Dataset/train/Inertial Signals/total_acc_Y_train.txt"))

GryoMeanX<-rbind(read.table(
  "UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt"),
  read.table("UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt"))
  
GryoMeanY<-rbind(read.table(
  "UCI HAR Dataset/test/Inertial Signals/body_gyro_Y_test.txt"),
  read.table("UCI HAR Dataset/train/Inertial Signals/body_gyro_Y_train.txt"))

GryoMeanZ<-rbind(read.table(
  "UCI HAR Dataset/test/Inertial Signals/body_gyro_Z_test.txt"),
  read.table("UCI HAR Dataset/train/Inertial Signals/body_gyro_Z_train.txt"))



## Make all data numeric

AccelorometerMeanX<-sapply(AccelorometerMeanX,as.numeric)
AccelorometerMeanY<-sapply(AccelorometerMeanY,as.numeric)
AccelorometerMeanZ<-sapply(AccelorometerMeanZ,as.numeric)
GryoMeanX<-sapply(GryoMeanX,as.numeric)
GryoMeanY<-sapply(GryoMeanY,as.numeric)
GryoMeanZ<-sapply(GryoMeanZ,as.numeric)


## Gather the Data

Gather_Data<-data.frame("TimeID"=1:10299, 
    "Subject ID"=subject[,1],
    "Activity"=y_all[,1],
    "AccelormeterMeanX"=apply(AccelorometerMeanX,1,mean),
    "AccelormeterMeanY"=apply(AccelorometerMeanY,1,mean),
    "AccelormeterMeanZ"=apply(AccelorometerMeanZ,1,mean),
    "AccelormeterSDX"=apply(AccelorometerMeanX,1,sd),
    "AccelormeterSDY"=apply(AccelorometerMeanY,1,sd),
    "AccelormeterSDZ"=apply(AccelorometerMeanZ,1,sd),
    "GryoMeanX"=apply(GryoMeanX,1,mean),
    "GryoMeanY"=apply(GryoMeanY,1,mean),
    "GryoMeanZ"=apply(GryoMeanZ,1,mean),
    "GryoSDX"=apply(GryoMeanX,1,sd),
    "GryoSDY"=apply(GryoMeanY,1,sd),
    "GryoSDZ"=apply(GryoMeanZ,1,sd))

    
library(dplyr)
library(tidyr)

## a little clean up

rm(GryoMeanZ);rm(GryoMeanX);rm(GryoMeanY)
rm(AccelorometerMeanX);rm(AccelorometerMeanY);rm(AccelorometerMeanZ)
rm(subject);rm(y_all)

## Make and Write Tables
dir.create("DataSet1")
dir.create("DataSet2")

## File writeFiles.R must be in place written to the environment

writeFiles<-function(file,actNum){
  file1<-paste0("DataSet1/",file,"_Table.csv")
  x<-filter(Gather_Data,Activity==actNum)%>%
    select(-Activity)
  write.csv(x,file1)
  
  x<-group_by(x,Subject.ID)
  
  x<-summarise(x,mean(AccelormeterMeanX),mean(AccelormeterMeanY),mean(AccelormeterMeanZ),
               mean(AccelormeterSDX),mean(AccelormeterSDY),mean(AccelormeterSDZ),
               mean(GryoMeanX),mean(GryoMeanY),mean(GryoMeanZ),
               mean(GryoSDX),mean(GryoSDY),mean(GryoSDZ))
  file1<-paste0("DataSet2/",file,"_Table.csv")
  names(x)[2:13]<-names(Gather_Data)[4:15]
  write.csv(x,file1)
  
}

Act_lab<-read.table("UCI HAR Dataset/activity_labels.txt")
mapply(writeFiles,file=Act_lab$V2,actNum=Act_lab$V1)

rm(Act_lab); rm(Gather_Data)



