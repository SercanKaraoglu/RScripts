fold1_test<-folds$fold1$test
fold2_test<-folds$fold2$test
fold_test<-rbind(fold1_test,
                 fold2_test)

test_h2oPreds1<-h2o.predict(model1,as.h2o(localH2O,fold_test))
test_h2oPreds2<-h2o.predict(model2,as.h2o(localH2O,fold_test))

test_h2oPreds1<-as.data.frame(test_h2oPreds1)
test_h2oPreds2<-as.data.frame(test_h2oPreds2)

test_pred7<-predict(thirdxg1,fold_test)
test_pred8<-predict(thirdxg2,fold_test)

rmse((test_pred7+test_pred8+test_h2oPreds1+test_h2oPreds2)/4,fold_test[,ncol(fold_test)]);
metaDataSet<-cbind(test_pred7,test_pred8,test_h2oPreds1,test_h2oPreds2,fold_test[,ncol(fold_test)])
test_x = as.matrix(metaDataSet)
test_x = matrix(as.numeric(test_x),nrow(test_x),ncol(test_x))

colnames(test_x)<-append(paste0("feat_",1:(ncol(test_x)-1)),"target")
crossValidateXgBoost(test_x,FALSE,list("objective" = "reg:logistic",
                                            "eval_metric" = "rmse",
                                            "eta" = 0.1,
                                            "max_depth"=12,
                                            "nthread" = 6),
                     40)
metaXG<-GetXGModel(test_x,FALSE,list("objective" = "reg:logistic",
                                                 "eval_metric" = "rmse",
                                                 "eta" = 0.1,
                                                 "max_depth"=12,
                                                 "nthread" = 6),
                               40)
metaDeep<-getDeepModel(test_x,FALSE)

metaDataTestSet<-cbind(pred7,pred8,h2oPreds1,h2oPreds2,test[,ncol(test)])
metatest_x = as.matrix(metaDataTestSet)
metatest_x = matrix(as.numeric(metatest_x),nrow(metatest_x),ncol(metatest_x))

colnames(metatest_x)<-append(paste0("feat_",1:(ncol(metatest_x)-1)),"target")

xg_metapredict<-predict(metaXG,metatest_x)
rmse(xg_metapredict,test[,ncol(test)])

h2o_metapredict<-h2o.predict(metaDeep,as.h2o(localH2O,metatest_x))
h2o_metapredict<-as.data.frame(h2o_metapredict)
rmse(h2o_metapredict,test[,ncol(test)])

rmse((xgPreds+xg_metapredict+h2o_metapredict)/3,test[,ncol(test)])