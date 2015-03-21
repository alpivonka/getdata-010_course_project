library(plyr)
library(dplyr)

main<-function(){
  x_trainLoc<-"Dataset/train/X_train.txt"
  y_trainLoc<-"Dataset/train/y_train.txt"
  trainSubjectsLoc<-"Dataset/train/subject_train.txt"
  
  x_testLoc<-"Dataset/test/X_test.txt"
  y_testLoc<-"Dataset/test/y_test.txt"
  testSubjectsLoc<-"Dataset/test/subject_test.txt"
  
  featuresLoc<-"Dataset/features.txt"
  activity_labelsLoc<-"Dataset/activity_labels.txt"
  
  
  activities<-join_Y_test2train_labels(y_trainLoc,y_testLoc,activity_labelsLoc)
  subjects<-joinTest2TrainSubjects(testSubjectsLoc,trainSubjectsLoc)
  observations<-join_X_test2Train(x_trainLoc,x_testLoc,featuresLoc)
  allData<-cbind(subjects,activities,observations)
  print(sample_n(allData,2))
  #-mean()-
  #-std()-
}


join_X_test2Train<-function(x_trainLoc,x_testLoc,featuresLoc){
  train<-read.table(x_trainLoc,header = F,strip.white = T)
  #print(nrow(train))
  test<-read.table(x_testLoc,header = F,strip.white = T)
  #print(nrow(test))
  observations<-rbind(test,train)
  print(nrow(observations))
  features<-read.table(featuresLoc,header = F,strip.white = T)
  names(features)<-c("id","headings")
  theHeadings<-as.character(features$headings)
  names(observations)<-theHeadings
  #print(sample_n(observations,2))
  observations
  
}

joinTest2TrainSubjects<-function(testSubjectsLoc,trainSubjectsLoc){
  train<-read.table(trainSubjectsLoc,header = F,strip.white = T)
  #print(nrow(train))
  test<-read.table(testSubjectsLoc,header = F,strip.white = T)
  #print(nrow(test))
  subjects<-rbind(test,train)
  #print(nrow(subjects))
  names(subjects)<-c("subject_id")
  subjects
}

join_Y_test2train_labels<-function(yTrainLoc,yTestLoc,activitiesLoc){
  #Load the two y files
  y_train<-read.table(yTrainLoc,header = F,strip.white = T)
  y_test<-read.table(yTestLoc,header = F,strip.white = T)
  y<-rbind(y_test, y_train)
  names(y)<-c("id")
  
  #print(nrow(y))
  #load activities
  activityLabel<-read.table(activitiesLoc,header = F,strip.white = T)
  names(activityLabel)<-c("id","activity")
  #select/join the y to activties
  y_activities<-select(join(y,activityLabel,by="id"),activity)
  #print(sample_n(y_activities,10))
  #print(nrow(y_activities))
  y_activities
}