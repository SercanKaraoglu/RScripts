library(Metrics)
library(xgboost)
library(data.table)

root_dir<-"~/git/RScripts/WestNile/"
dropbox_root_dir<-"~/Dropbox/WestNileViursPrediction/"
submissionDir<-paste0(root_dir,"submission.csv")

source("~/git/RScripts/WestNile/loadWestNileData.R")
source("~/git/RScripts/WestNile/prepData.R")

# Set necessary parameter
param <- list("objective" = "binary:logistic",
              "eval_metric" = "auc",
              "eta" = 0.50,
              "subsample" = 0.5,
              "min_child_weight" = 3,
              "colsample_bytree" = 0.5,
              "max_depth"=30,
              "nthread" = 2)
nrounds<-160

xg.cv = xgb.cv(param=param, data = d_X, label = d_y, 
               nfold = 5, nrounds=nrounds)

xg = xgboost(param=param, data = d_X, label = d_y, nrounds=nrounds)
# model<-getDeepModel(cbind(x1AsMatrix,y1))
# h2oPrediction<-as.data.frame(h2o.predict(model,as.h2o(localH2O,x2AsMatrix)))
xgPrediction2<-predict(xg,d_x_valid);

## check for a reasonable AUC of the model against unseen data (2011)

xgPrediction<-predict(xg,d_test);

pSubmit<-xgPrediction;
## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
summary(pSubmit)

submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,submissionDir,row.names=FALSE,quote=FALSE)
# lasagneSubmissionDir<-paste0(root_dir,"lasagne_west_nile.csv")
# lasagneSubmission<-read.csv(lasagneSubmissionDir)
# submissionFile[,2]<-(pSubmit+lasagneSubmission[,2])/2
# write.csv(submissionFile,submissionDir,row.names=FALSE,quote=FALSE)

print(paste("xg auc:",auc(d_y_valid,xgPrediction2)));