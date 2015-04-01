#assumption that all required Libraries have been installed
# plyr
# dplyr
# tidyr
# utils

library(plyr)
library(dplyr)
library(tidyr)
library(utils)


# Main function--
# Entry point all other functions are called from there.
main<-function(){
  x_trainLoc<-"Dataset/train/X_train.txt"
  y_trainLoc<-"Dataset/train/y_train.txt"
  trainSubjectsLoc<-"Dataset/train/subject_train.txt"
  
  x_testLoc<-"Dataset/test/X_test.txt"
  y_testLoc<-"Dataset/test/y_test.txt"
  testSubjectsLoc<-"Dataset/test/subject_test.txt"
  
  featuresLoc<-"Dataset/features.txt"
  activity_labelsLoc<-"Dataset/activity_labels.txt"
  
  # Load and join the Y test and train together
  activities<-join_Y_test2train_labels(y_trainLoc,y_testLoc,activity_labelsLoc)
  #Combine the Y train and test and row bind subjects together
  subjects<-joinTest2TrainSubjects(testSubjectsLoc,trainSubjectsLoc)
  #Combine the x_train and x_test data and apply the headers
  observations<-join_X_test2Train(x_trainLoc,x_testLoc,featuresLoc)
  #Combine the data together
  allData<-cbind(subjects,activities,observations)
  #clean up the column heading names
  allData<-cleanup_columnNames(allData)
  #View(allData)
  recordWideData(allData)
  recordTidyNarrow(allData)
  recordTidyMean(allData)
  print("All done, enjoy the day.")
}

#create output csv file for tidy wide data example
recordWideData<-function(wideData){
  write.csv(wideData,file="tidyWide.csv",)
  print("Done recordinng wide data tidywide.csv")
}

#create output csv file for tidy narrow/tall data example
recordTidyNarrow<-function(narrowData){
  yy<-gather(narrowData,coltype,values,-subject_id,-activity)
  #View(yy)
  ya<-separate(data=yy,col=coltype, into=c("type","stat","axis"), extra="merge")
  #View(ya)
  write.csv(ya,file="tidyNarrow.csv",)
  print("Done recording narrow data tidyNarrow.csv - Not part of requirements")
}

#create output for #5
recordTidyMean<-function(wideData){
  subsetForMean<-subset(wideData,select=tBodyAcc_Mean_X:fBodyGyroJerkMag_Std)
  tidy <- aggregate(subsetForMean, by=list(activity = wideData$activity,subject=wideData$subject), mean,na.rm=TRUE)
  #View(tidy)
  write.csv(tidy,file="tidy.csv",)
  print("Done recording tidy.csv  -- Final out put")
  
}

# Clean up the headers 
cleanup_columnNames<-function(allData){
  names <- names(allData) 
  names <- gsub('mean', 'Mean', names) # Replace mean with Mean
  names <- gsub('std', 'Std', names) # Replace std with Std
  names <- gsub('[()]', '', names) # Remove the parenthesis
  names <- gsub('[-]', '_', names) # Remove the dashes
  names <- gsub('BodyBody', 'Body', names) # Replace `BodyBody' by `Body'
  names(allData)<-names
  allData
}

#Combine the x_train and x_test data and apply the headers
join_X_test2Train<-function(x_trainLoc,x_testLoc,featuresLoc){
  #Load the two X files train/test
  train<-read.table(x_trainLoc,header = F,strip.white = T)
  test<-read.table(x_testLoc,header = F,strip.white = T)
  #row bind the data together
  observations<-rbind(test,train)
  #load the features data
  features<-read.table(featuresLoc,header = F,strip.white = T)
  #assign column names 
  names(features)<-c("id","headings")
  theHeadings<-as.character(features$headings)
  names(observations)<-theHeadings
  # obtain only those columns which are mean or std
  meanStd<-grep("mean\\(\\)|std\\(\\)",theHeadings,value = T)
  #print(yy)
  observations<-observations[,meanStd]
  #return observations
  observations
}

#Combine the Y train and test and row bind subjects together
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

# Load and join the Y test and train together
join_Y_test2train_labels<-function(yTrainLoc,yTestLoc,activitiesLoc){
  #Load the two y files
  y_train<-read.table(yTrainLoc,header = F,strip.white = T)
  y_test<-read.table(yTestLoc,header = F,strip.white = T)
  #row bind the files together
  y<-rbind(y_test, y_train)
  #give the column a name 
  names(y)<-c("id")
  #load activities file
  activityLabel<-read.table(activitiesLoc,header = F,strip.white = T)
  #give the activities data column names
  names(activityLabel)<-c("id","activity")
  #select/join the y to activities
  y_activities<-select(join(y,activityLabel,by="id"),activity)
  #return the activities
  y_activities
}