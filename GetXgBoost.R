require(xgboost)
require(methods)
readCSV<-function(dir){
  data = read.csv(dir,header=TRUE,stringsAsFactors = F)
  y = data[,ncol(data)]
  y = gsub('Class_','',y)
  y = as.integer(y)-1 #xgboost take features in [0,numOfClass)
  
  x = as.matrix(data)
  x = matrix(as.numeric(x),nrow(x),ncol(x))
  x[,ncol(x)] = y;
  
  colnames(x)<-colnames(data);
  x;
}
crossValidateXgBoost<-function(train,deleteFirstCol,nrounds,max_depth){
  if(deleteFirstCol==T){
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
GetXGModel<- function(train,deleteFirstCol,nrounds,max_depth){
  if(deleteFirstCol==T){
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