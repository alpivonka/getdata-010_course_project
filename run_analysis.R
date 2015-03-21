
library(plyr)
library(dplyr)

run_analysis<-function(){
  x_trainLoc<-"Dataset/train/X_train.txt"
  y_trainLoc<-"Dataset/train/y_train.txt"
  trainSubjectsLoc<-"Dataset/train/subject_train.txt"
  
  featuresLoc<-"Dataset/features.txt"
  activity_labelsLoc<-"Dataset/activity_labels.txt"
  
  
  
  #Join y_train with Activities
  y_train<-join_yTrain2Activitylabels(y_trainLoc,activity_labelsLoc)
  
  #subject_train  and joint to ytrain
  subjectYTrainActivities <- join_yTrainActivity2TrainSubjects(y_train,trainSubjectsLoc)
  #Load the Train and assign the Feature List
  xtrain<-joinXTrain2Features(x_trainLoc,featuresLoc)
  
  results<-cbind(subjectYTrainActivities,xtrain)
  print(head(results))
  stop()
  
  print(sample_n(subjectYTrainActivities,10))
}

joinXTrain2Features<-function(xtrainLoc, featuresLoc){
 
  features<-read.table(featuresLoc,header = F,strip.white = T)
  names(features)<-c("id","headings")
  theHeadings<-as.character(features$headings)
  x_train<-read.table(xtrainLoc,header = F,strip.white = T)
  names(x_train)<-theHeadings
  
  print(head(x_train))
  x_train
}

# Responsible for combining the Train y + actvities
join_yTrain2Activitylabels<-function(ytrainLoc,activtiesLoc){
  
  #load and join the y_train  to labels
  y_train<-read.table(ytrainLoc,header = F,strip.white = T)
  names(y_train)<-c("id")
  activityLable<-read.table(activtiesLoc,header = F,strip.white = T)
  names(activityLable)<-c("id","activity")
  y_Train_activities<-select(join(y_train,activityLable,by="id"),activity)
}

# Responsible for combining the train y + activities + subjects
join_yTrainActivity2TrainSubjects<-function(ytrainActvities,trainSubjectsLoc){
  
  subjects<-read.table(trainSubjectsLoc,header = F,strip.white = T)
  xx<-cbind(subjects,ytrainActvities$activity)
  names(xx)<-c("subject","activity")
  xx
  
}