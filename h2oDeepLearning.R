library(h2o)
localH2O = h2o.init(nthreads = -1, max_mem_size = "5g")

getModel <- function(train){
  train<-train[,-1];
  for(i in 1:93){
    train[,i] <- as.numeric(train[,i])
    train[,i] <- sqrt(train[,i]+(3/8))
  }

  train.hex <- as.h2o(localH2O,train)
  
  predictors <- 1:(ncol(train.hex)-1)
  response <- ncol(train.hex)
  
  model <- h2o.deeplearning(x=predictors,
                            y=response,
                            data=train.hex,
                            classification=T,
                            activation="RectifierWithDropout",
                            hidden=c(1024,512,256),
                            hidden_dropout_ratio=c(0.5,0.5,0.5),
                            input_dropout_ratio=0.05,
                            epochs=50,
                            l1=1e-5,
                            l2=1e-5,
                            rho=0.99,
                            epsilon=1e-8,
                            train_samples_per_iteration=2000,
                            max_w2=10,
                            seed=1)
  model;
}
getPrediction<-function(model,test){
  test<-test[,-1];
  for(i in 1:93){
    test[,i] <- as.numeric(test[,i])
    test[,i] <- sqrt(test[,i]+(3/8))
  }
  test.hex <- as.h2o(localH2O,test[,1:93])
  prediction<-as.data.frame(h2o.predict(model,test.hex))[,2:10]
}