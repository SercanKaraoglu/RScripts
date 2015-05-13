setSubmission<- function(prediction,fileName){
  otto_dir<-'/Dropbox/otto'
  submissionDir<-paste0(path.expand("~"),otto_dir,'/',fileName)
  submission <- read.csv(submissionDir)
  submission[,2:10] <- 0
  
  submission[,2:10] <- submission[,2:10] + prediction;
  
  write.csv(submission,file=submissionDir,row.names=FALSE)  
}