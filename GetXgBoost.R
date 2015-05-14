require(xgboost)
require(methods)
readCSV<-function(dir){
  x = read.csv(dir,header=TRUE,stringsAsFactors = F)
  x = x[,-ncol(x)]
  x = as.matrix(x)
  x = matrix(as.numeric(x),nrow(x),ncol(x))
  x;
}
crossValidateXgBoost<-function(train,deleteFirstRow,nrounds,max_depth){
  if(deleteFirstRow==T){
    train<-train[,-1] 
  }
  y = train[,ncol(train)]
  
  x = train[,-ncol(train)];
  
  # Set necessary parameter
  param <- list("objective" = "multi:softprob",
                "eval_metric" = "mlogloss",
                "eta" = 0.25,
                "max_depth"=max_depth,
                "num_class" = 9,
                "nthread" = 3)
  
  bst.cv = xgb.cv(param=param, data = x, label = y, 
                  nfold = 5, nrounds=nrounds)
}
GetXGModel<- function(train,deleteFirstRow,nrounds,max_depth){
  if(deleteFirstRow==T){
    train<-train[,-1] 
  }
  y = train[,ncol(train)]
  x = train[,-ncol(train)];
  # Set necessary parameter
  param <- list("objective" = "multi:softprob",
                "eval_metric" = "mlogloss",
                "eta" = 0.25,
                "max_depth"=max_depth,
                "num_class" = 9,
                "nthread" = 3)
  bst = xgboost(param=param, data = x, label = y, nrounds=nrounds)
  bst;
}
# Make prediction
makePrediction <- function(xgmodel,data){
  pred = predict(xgmodel,data)
  pred = matrix(pred,9,length(pred)/9)
  pred = t(pred)
  pred;
}