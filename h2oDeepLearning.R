library(h2o)
localH2O <- h2o.init(ip = "localhost", port=54321, nthreads = 6)
column_bind<-function(data){
  deepfeatures_layer<-cbind(data[,2:ncol(data)],
                            data[,1]);
  deepfeatures_layer;
}
extractFeature<-function(layerNumber,model,train){
  train.hex <- as.h2o(localH2O,train)
  
  deepfeatures_layer = h2o.deepfeatures(train.hex, model, layer = layerNumber)
  deepfeatures_layer<-as.data.frame(deepfeatures_layer);
  deepfeatures_layer<-as.matrix(deepfeatures_layer);
  deepfeatures_layer<-matrix(as.numeric(deepfeatures_layer),nrow(deepfeatures_layer),ncol(deepfeatures_layer))
  deepfeatures_layer;
}
#isClassification false ise regression yapılacak
getDeepModel <- function(train,isClassification){
  train.hex <- as.h2o(localH2O,train)
  
  predictors <- 1:(ncol(train.hex)-1)
  response <- ncol(train.hex)
  
  model <- h2o.deeplearning(x=predictors,
                            y=response,
                            data=train.hex,
                            classification=isClassification,
                            activation="RectifierWithDropout",
                            hidden=c(256,256,1),
                            hidden_dropout_ratio=c(0.4,0.4,0.4),
                            input_dropout_ratio=0.05,
                            epochs=50,
                            l1=1e-5,
                            l2=1e-5,
                            rho=0.9,
                            epsilon=1e-8,
                            train_samples_per_iteration=500,
                            nesterov_accelerated_gradient=TRUE,
                            max_w2=10,
                            seed=1)
  model;
}
getPrediction<-function(model,test){
  for(i in 1:ncol(test)){
    test[,i] <- as.numeric(test[,i])
#   test[,i] <- sqrt(test[,i]+(3/8))
  }
  test.hex <- as.h2o(localH2O,test[,1:ncol(test)])
  prediction<-as.data.frame(h2o.predict(model,test.hex))[,2:10]
}