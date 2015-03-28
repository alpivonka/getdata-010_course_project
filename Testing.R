library(plyr)
library(dplyr)
library(tidyr)

main2<-function(){
  xx<- read.csv("testing.csv")
  #View(xx)
  #subject_id  activity	tBodyAcc_Mean_X	tBodyAcc_Mean_Y	tBodyAcc_Mean_Z
  #yy<-gather(xx,coltype,values,-X,-subject_id,-activity)
  #View(yy)
  #yy<-separate(data=yy,col=coltype, into=c("type","stat","axis"))
  #View(yy)
  gby<-group_by(xx,activity)
  gby %>% summarise_each(funs(mean))
  aa<-subset(xx,select=tBodyAcc_Mean_X:fBodyGyroJerkMag_Std)
  tidy = aggregate(aa, by=list(activity = xx$activity,subject=xx$subject), mean,na.rm=TRUE)
  View(tidy)
  
  
  
  #summarise
  
  #View(aggregate(xx, by=as.list(xx$activity),FUN="mean"))
  
}