library(Metrics)
library(xgboost)
library(data.table)

root_dir<-"~/git/RScripts/WestNile/"
submissionDir<-paste0(root_dir,"submission.csv")

source("~/git/RScripts/WestNile/loadWestNileData.R")

# Set necessary parameter
param <- list("objective" = "binary:logistic",
              "eval_metric" = "auc",
              "eta" = 0.20,
              "max_depth"=3,
              "nthread" = 2)
nrounds<-65

xg.cv = xgb.cv(param=param, data = x1AsMatrix, label = y1, 
               nfold = 5, nrounds=nrounds)

xg = xgboost(param=param, data = x1AsMatrix, label = y1, nrounds=nrounds)
# model<-getDeepModel(cbind(x1AsMatrix,y1))
# h2oPrediction<-as.data.frame(h2o.predict(model,as.h2o(localH2O,x2AsMatrix)))
xgPrediction<-predict(xg,x2AsMatrix);

## check for a reasonable AUC of the model against unseen data (2011)
print(paste("xg auc:",auc(y2,xgPrediction)));
## now fit a new model to all the data, so that our final submission includes information learned from 2011 as well
xg.cv = xgb.cv(param=param, data = xAsMatrix, label = y, 
                nfold = 5, nrounds=nrounds)
xg = xgboost(param=param, data = xAsMatrix, label = y, nrounds=nrounds)
xgPrediction<-predict(xg,testAsMatrix);

pSubmit<-xgPrediction;
## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
summary(pSubmit)

submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,submissionDir,row.names=FALSE,quote=FALSE)
lasagneSubmissionDir<-paste0(root_dir,"lasagne_west_nile.csv")
lasagneSubmission<-read.csv(lasagneSubmissionDir)
submissionFile[,2]<-(pSubmit+lasagneSubmission[,2])/2
write.csv(submissionFile,submissionDir,row.names=FALSE,quote=FALSE)