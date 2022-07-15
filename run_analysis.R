load_data<- function(){
  #loading packages
  library(tidyr)
  library(dplyr)
  
  ##loading train and test data
  xtrain<-read.table("./UCI HAR Dataset/train/X_train.txt")
  ytrain<-read.table("./UCI HAR Dataset/train/y_train.txt")
  subtrain<-read.table("./UCI HAR Dataset/train/subject_train.txt")
  
  xtest<-read.table("./UCI HAR Dataset/test/X_test.txt")
  ytest<-read.table("./UCI HAR Dataset/test/y_test.txt")
  subtest<-read.table("./UCI HAR Dataset/test/subject_test.txt")
  
  #loading feature and activity names
  features<-read.table("./UCI HAR Dataset/features.txt")
  activity<-read.table("./UCI HAR Dataset/activity_labels.txt")
  
  ##combining train and test data for x,y and subjects
  x<-rbind(xtrain,xtest)
  y<-rbind(ytrain,ytest)
  subj<-rbind(subtrain,subtest)
  
  ##substituting activity names
  activity<- activity[y[[1]],2]
  
  ##naming variables
  colnames(x)<- features[[2]]
  colnames(subj)<- "subject"
  
  ##filtering out mean and std data
  x<-select(x,contains("mean()"),contains("std()"))
  
  ##combining to a single dataset
  proj_data<-cbind(subj,activity,x)
  
  #group the data by subject and activity
  grouped_data<-group_by(proj_data,subject, activity)
  #find average of each variable for each subject and each activity
  summ_data<-summarise(grouped_data,value=colMeans(grouped_data[,3:68]),ind=1:66)
  #assigning variable names
  summ_data$ind<-colnames(proj_data)[3:68][summ_data$ind]
  #spreading data
  tidy_data<-spread(summ_data,ind, value)
  tidy_data<-select(tidy_data,subject,everything())

  tidy_data 
  
}
