
#library(plyr)
library(dplyr)

run_analysis<-function(){
  
  #Load the x_train contains -- 
  #x_train<-read.table("Dataset/train/X_train.txt",header = F,strip.white = T)
  #print(head(x_train))
  
  
  y_train<-join_yTrain2Activitylabels()
  
  #subject_train  and joint to xtrain
  subjectYTrainActivities <- join_yTrainActivity2TrainSubjects(y_train)
  print(str(subjectYTrainActivities))
  print(sample_n(subjectYTrainActivities,10))
}


join_yTrain2Activitylabels<-function(){
  
  #load and join the y_train  to labels
  y_train<-read.table("Dataset/train/y_train.txt",header = F,strip.white = T)
  names(y_train)<-c("id")
  activityLable<-read.table("Dataset/activity_labels.txt",header = F,strip.white = T)
  names(activityLable)<-c("id","activity")
 
  
  y_Train_activities<-select(join(y_train,activityLable,by="id"),activity)
}

join_yTrainActivity2TrainSubjects<-function(ytrainActvities){
  
  subjects<-read.table("Dataset/train/subject_train.txt",header = F,strip.white = T)
  #subjects<-names(subjects)<-c("subjectId")
  xx<-cbind(subjects,ytrainActvities$activity)
  names(xx)<-c("subject","activity")
  xx
  
}