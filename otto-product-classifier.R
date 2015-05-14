# Output submission
writeSubmissionByAveraging <- function(preds){
  submissionDir <- paste0(path.expand("~"), "/Dropbox/otto/submission.csv",)
  submission <- read.csv(submissionDir)
  submission[,2:10] <- 0
  for(pred in preds){
    submission[,2:10] <- submission[,2:10] + pred;
  }
  submission[,2:10] <- submission[,2:10]/length(preds);
  write.csv(submission,file=submissionDir,row.names=FALSE)  
}

fold1<-cbind(X,Y)
colnames(fold1)<-colnames(train)

fold2<-cbind(Xt,Yt)
colnames(fold2)<-colnames(fold1)

model1<-getDeepModel(fold1)
model2<-getDeepModel(fold2)
####################################################################################
##Extract Features
####################################################################################
model1_deep_feat_1<-extractFeature(1,model1,fold2)
model1_deep_feat_2<-extractFeature(2,model1,fold2)
model1_deep_feat_3<-extractFeature(3,model1,fold2)

model2_deep_feat_1<-extractFeature(1,model2,fold1)
model2_deep_feat_2<-extractFeature(2,model2,fold1)
model2_deep_feat_3<-extractFeature(3,model2,fold1)

####################################################################################
##Classification with Extracted Features
####################################################################################

crossValidateXgBoost(model1_deep_feat_1,FALSE,100,3)
crossValidateXgBoost(model1_deep_feat_2,FALSE,20,3)
crossValidateXgBoost(model1_deep_feat_3,FALSE,20,3)

crossValidateXgBoost(model2_deep_feat_1,FALSE,20,3)
crossValidateXgBoost(model2_deep_feat_2,FALSE,20,3)
crossValidateXgBoost(model2_deep_feat_3,FALSE,20,3)

first_xg1<-GetXGModel(model1_deep_feat_1,FALSE,20,3)
first_xg2<-GetXGModel(model1_deep_feat_2,FALSE,20,3)
first_xg3<-GetXGModel(model1_deep_feat_3,FALSE,20,3)

second_xg1<-GetXGModel(model2_deep_feat_1,FALSE,20,3)
second_xg2<-GetXGModel(model2_deep_feat_2,FALSE,20,3)
second_xg3<-GetXGModel(model2_deep_feat_3,FALSE,20,3)



testDir <- paste0(path.expand("~"),otto_dir,'/test.csv')
test<-readCSV(testDir);

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

writeSubmissionByAveraging(c(pred1,pred2,pred3,pred4,pred5,pred6))

