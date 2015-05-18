require(caret)
getFolds<-function(data,fold){
  return(list("train"=data[fold,],"test"=data[-fold,]))
}
getIndexes <- function(data,k){
  y_data <- data[,ncol(data)]
  folds<-createFolds(y=y_data,k = k,list=TRUE,returnTrain = TRUE)
  fold<-folds$Fold01
}
fold2Indexes<-getIndexes(model1_deep_feat_1,10);
fold2_extracted_feat1<-getFolds(model1_deep_feat_1,fold2Indexes)
fold2_extracted_feat2<-getFolds(model1_deep_feat_2,fold2Indexes)
fold2_extracted_feat3<-getFolds(model1_deep_feat_3,fold2Indexes)

fold1Indexes<-getIndexes(model2_deep_feat_1,10);
fold1_extracted_feat1<-getFolds(model2_deep_feat_1,fold1Indexes)
fold1_extracted_feat2<-getFolds(model2_deep_feat_2,fold1Indexes)
fold1_extracted_feat3<-getFolds(model2_deep_feat_3,fold1Indexes)

fold2_xg1<-GetXGModel(fold2_extracted_feat1$train,FALSE,20,6)
fold2_xg2<-GetXGModel(fold2_extracted_feat2$train,FALSE,20,6)
fold2_xg3<-GetXGModel(fold2_extracted_feat3$train,FALSE,20,6)

fold1_xg1<-GetXGModel(fold1_extracted_feat1$train,FALSE,20,6)
fold1_xg2<-GetXGModel(fold1_extracted_feat2$train,FALSE,20,6)
fold1_xg3<-GetXGModel(fold1_extracted_feat3$train,FALSE,20,6)

fold2_pred1<-makePrediction(fold2_first_xg1,fold2_extracted_feat1$test[,-ncol(fold2_extracted_feat1$test)])
fold2_pred2<-makePrediction(fold2_first_xg2,fold2_extracted_feat2$test[,-ncol(fold2_extracted_feat2$test)])
fold2_pred3<-makePrediction(fold2_first_xg3,fold2_extracted_feat3$test[,-ncol(fold2_extracted_feat3$test)])

fold1_pred4<-makePrediction(fold1_second_xg1,fold1_extracted_feat1$test[,-ncol(fold1_extracted_feat1$test)])
fold1_pred5<-makePrediction(fold1_second_xg2,fold1_extracted_feat2$test[,-ncol(fold1_extracted_feat2$test)])
fold1_pred6<-makePrediction(fold1_second_xg3,fold1_extracted_feat3$test[,-ncol(fold1_extracted_feat3$test)])

preds_fold1<-cbind(fold1_pred4,fold1_pred5,fold1_pred6)
preds_fold2<-cbind(fold2_pred1,fold2_pred2,fold2_pred3)
preds_folds<-rbind(preds_fold1,preds_fold2)

actual_preds<-append(fold1_extracted_feat1$test[,ncol(fold1_extracted_feat1$test)],
                    fold2_extracted_feat1$test[,ncol(fold2_extracted_feat1$test)])

metaTrainSet <- cbind(preds_folds,actual_preds)
crossValidateXgBoost(metaTrainSet,FALSE,20,6)
metaXG<-GetXGModel(metaTrainSet,FALSE,20,6)