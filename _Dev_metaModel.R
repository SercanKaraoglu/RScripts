##Burada xgboost ile bir önceki predictionlar column ve row olarak birleştirilip meta model oluşturulacak
x = model2_deep_feat_1;
pred1<-makePrediction(first_xg1,x[,-ncol(x)])
pred2<-makePrediction(first_xg2,x[,-ncol(x)])
pred3<-makePrediction(first_xg3,x[,-ncol(x)])

pred4<-makePrediction(second_xg1,x[,-ncol(x)])
pred5<-makePrediction(second_xg2,x[,-ncol(x)])
pred6<-makePrediction(second_xg3,x[,-ncol(x)])


half1<-mget( paste0( "pred" , 1:3 ) )
half2<-mget( paste0( "pred" , 4:6 ) )
do.call( cbind , half1 )
do.call( cbind , half2 )
testDataFromPredictions<-mget( paste0("half",1:2))
do.call(rbind,trainDataFromPredictions)


trainDataFromPredictions<-rbind(firstHalf,
                                secondHalf)
crossValidateXgBoost(trainDataFromPredictions,FALSE,20,3)
metaXG<-GetXGModel(trainDataFromPredictions,FALSE,20,3)
