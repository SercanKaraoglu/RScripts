source('~/git/RScripts/loadMyData.R')
source('~/git/RScripts/createFold.R')
source('~/git/RScripts/h2oDeepLearning.R')
source('~/git/RScripts/GetXgBoost.R')

getSubmissionFormat <- function(submissionDir){
  submission <- read.csv(submissionDir)
  submission[,2:10] <- 0
  submission;
}
writeSubmissionByAveraging <- function(preds){
  submissionDir <- paste0(path.expand("~"), "/Dropbox/otto/submission.csv")
  submission<-getSubmissionFormat(submissionDir);
  for(pred in preds){
    submission[,2:10] <- submission[,2:10] + pred;
  }
  submission[,2:10] <- submission[,2:10]/length(preds);
  write.csv(submission,file=submissionDir,row.names=FALSE)
}
writeSubmissionByEnsembling <- function(){
  submissionDir <- paste0(path.expand("~"), "/Dropbox/otto/submission.csv")
  submission <- read.csv(submissionDir)
  preds_fold1<-cbind(pred4,pred5,pred6)
  preds_fold2<-cbind(pred1,pred2,pred3)
  data<-(preds_fold1+preds_fold2)/2;
  View(submission[,2:10])
  View(data)
  submission[,2:10] <-(data+submission[,2:10])/2
  View(submission[,2:10])
  write.csv(submission,file=submissionDir,row.names=FALSE)
}

folds<-createFold(train,2)
fold1<-folds$fold1$train
fold2<-folds$fold2$train

model1<-getDeepModel(fold1)
model2<-getDeepModel(fold2)
####################################################################################
##Extract Features
####################################################################################
model1_deep_feat_1<-extractFeature(1,model1,fold2)
model1_deep_feat_1<-column_bind(model1_deep_feat_1);

model1_deep_feat_2<-extractFeature(2,model1,fold2)
model1_deep_feat_2<-column_bind(model1_deep_feat_2);

model1_deep_feat_3<-extractFeature(3,model1,fold2)
model1_deep_feat_3<-column_bind(model1_deep_feat_3);

model2_deep_feat_1<-extractFeature(1,model2,fold1)
model2_deep_feat_1<-column_bind(model2_deep_feat_1);

model2_deep_feat_2<-extractFeature(2,model2,fold1)
model2_deep_feat_2<-column_bind(model2_deep_feat_2);

model2_deep_feat_3<-extractFeature(3,model2,fold1)
model2_deep_feat_3<-column_bind(model2_deep_feat_3);
####################################################################################
##Classification with Extracted Features
####################################################################################

crossValidateXgBoost(model1_deep_feat_1,FALSE,40,15)
crossValidateXgBoost(model1_deep_feat_2,FALSE,40,15)
crossValidateXgBoost(model1_deep_feat_3,FALSE,40,15)
 
crossValidateXgBoost(model2_deep_feat_1,FALSE,40,15)
crossValidateXgBoost(model2_deep_feat_2,FALSE,40,15)
crossValidateXgBoost(model2_deep_feat_3,FALSE,40,15)

first_xg1<-GetXGModel(model1_deep_feat_1,FALSE,40,15)
first_xg2<-GetXGModel(model1_deep_feat_2,FALSE,40,15)
first_xg3<-GetXGModel(model1_deep_feat_3,FALSE,40,15)

second_xg1<-GetXGModel(model2_deep_feat_1,FALSE,40,15)
second_xg2<-GetXGModel(model2_deep_feat_2,FALSE,40,15)
second_xg3<-GetXGModel(model2_deep_feat_3,FALSE,40,15)

transformated<-transform(forPrediction=TRUE,test);
test<-transformated@x

test_model1_deep_feat_1<-extractFeature(1,model1,test)
test_model1_deep_feat_2<-extractFeature(2,model1,test)
test_model1_deep_feat_3<-extractFeature(3,model1,test)

test_model2_deep_feat_1<-extractFeature(1,model2,test)
test_model2_deep_feat_2<-extractFeature(2,model2,test)
test_model2_deep_feat_3<-extractFeature(3,model2,test)

pred1<-makePrediction(first_xg1,test_model1_deep_feat_1)
pred2<-makePrediction(first_xg2,test_model1_deep_feat_2)
pred3<-makePrediction(first_xg3,test_model1_deep_feat_3)

pred4<-makePrediction(second_xg1,test_model2_deep_feat_1)
pred5<-makePrediction(second_xg2,test_model2_deep_feat_2)
pred6<-makePrediction(second_xg3,test_model2_deep_feat_3)

preds <- mget( paste0( "pred" , 1:6 ) )
do.call( rbind , preds )
writeSubmissionByAveraging(preds)