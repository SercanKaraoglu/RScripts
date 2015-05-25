source('~/git/RScripts/loadIndicatorData.R')
source('~/git/RScripts/transform.R')
source('~/git/RScripts/featureEngineering.R')
source('~/git/RScripts/createFold.R')
source('~/git/RScripts/h2oDeepLearning.R')
source('~/git/RScripts/GetXgBoost.R')

folds<-createFold(train,2)
fold1<-folds$fold1$train
fold2<-folds$fold2$train

model1<-getDeepModel(fold1,isClassification=FALSE)
model2<-getDeepModel(fold2,isClassification=FALSE)
####################################################################################
##Extract Features
####################################################################################
model1_deep_feat_1<-extractFeature(1,model1,fold2)
model1_deep_feat_1<-column_bind(model1_deep_feat_1);

model1_deep_feat_2<-extractFeature(2,model1,fold2)
model1_deep_feat_2<-column_bind(model1_deep_feat_2);

model2_deep_feat_1<-extractFeature(1,model2,fold1)
model2_deep_feat_1<-column_bind(model2_deep_feat_1);

model2_deep_feat_2<-extractFeature(2,model2,fold1)
model2_deep_feat_2<-column_bind(model2_deep_feat_2);

####################################################################################
##Classification with Extracted Features
####################################################################################
param <- list("objective" = "reg:logistic",
              "eval_metric" = "rmse",
              "eta" = 0.25,
              "max_depth"=12,
              "nthread" = 6)
nfold<-15
crossValidateXgBoost(model1_deep_feat_1,FALSE,param,nfold)
crossValidateXgBoost(model1_deep_feat_2,FALSE,param,nfold)

crossValidateXgBoost(model2_deep_feat_1,FALSE,param,nfold)
crossValidateXgBoost(model2_deep_feat_2,FALSE,param,nfold)

first_xg1<-GetXGModel(model1_deep_feat_1,FALSE,param,nfold)
first_xg2<-GetXGModel(model1_deep_feat_2,FALSE,param,nfold)

second_xg1<-GetXGModel(model2_deep_feat_1,FALSE,param,nfold)
second_xg2<-GetXGModel(model2_deep_feat_2,FALSE,param,nfold)

transformated<-transform(forPrediction=TRUE,test);
test<-cbind(transformated@x,transformated@y);
colNames<-paste0("feat_",1:(ncol(test)-1))
colNames<-append(colNames,"target")
colnames(test)<-colNames

test_model1_deep_feat_1<-extractFeature(1,model1,test)
test_model1_deep_feat_1<-column_bind(test_model1_deep_feat_1);

test_model1_deep_feat_2<-extractFeature(2,model1,test)
test_model1_deep_feat_2<-column_bind(test_model1_deep_feat_2);

test_model2_deep_feat_1<-extractFeature(1,model2,test)
test_model2_deep_feat_1<-column_bind(test_model2_deep_feat_1)

test_model2_deep_feat_2<-extractFeature(2,model2,test)
test_model2_deep_feat_2<-column_bind(test_model2_deep_feat_2)

pred1<-predict(first_xg1,test_model1_deep_feat_1)
pred2<-predict(first_xg2,test_model1_deep_feat_2)

pred4<-predict(second_xg1,test_model2_deep_feat_1)
pred5<-predict(second_xg2,test_model2_deep_feat_2)

h2oPreds1<-h2o.predict(model1,as.h2o(localH2O,test))
h2oPreds2<-h2o.predict(model2,as.h2o(localH2O,test))

h2oPreds1<-as.data.frame(h2oPreds1)
h2oPreds2<-as.data.frame(h2oPreds2)


thirdxg1<-GetXGModel(fold1,FALSE,list("objective" = "reg:logistic",
                                      "eval_metric" = "rmse",
                                      "eta" = 0.1,
                                      "max_depth"=12,
                                      "nthread" = 6)
                     ,100)
thirdxg2<-GetXGModel(fold2,FALSE,list("objective" = "reg:logistic",
                                      "eval_metric" = "rmse",
                                      "eta" = 0.1,
                                      "max_depth"=12,
                                      "nthread" = 6)
                     ,100)
pred7<-predict(thirdxg1,test)
pred8<-predict(thirdxg2,test)

require(hydroGOF)

rmse(pred1,test[,ncol(test)])
rmse(pred2,test[,ncol(test)])
rmse(pred4,test[,ncol(test)])
rmse(pred5,test[,ncol(test)])
rmse(pred7,test[,ncol(test)])
rmse(pred8,test[,ncol(test)])

extractedPreds<-(pred1+pred2+pred4+pred5)/6
h2oPreds<-(h2oPreds1+h2oPreds2)/2;
xgPreds<-(pred7+pred8)/2;

myPrint<-function(FUN){
  print(paste("h2oPreds1",FUN(h2oPreds1,test[,ncol(test)])))
  print(paste("h2oPreds2",FUN(h2oPreds2,test[,ncol(test)])))
  print(paste("h2oPredsAvg",FUN(h2oPreds,test[,ncol(test)])))
  
  print(paste("extractedPreds",FUN(extractedPreds,test[,ncol(test)])))
  print(paste("xgPreds",FUN(xgPreds,test[,ncol(test)])))
  print(paste("(h2o+xg)/2 preds",FUN((xgPreds+h2oPreds)/2,test[,ncol(test)]))) 
}
myPrint(rmse)
myPrint(cor)