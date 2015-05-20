require(caret)
createFold<-function(train,k){
  transformated<-transform(forPrediction=FALSE,train);
  more_features<-transformated@x;
  y<-transformated@y;
  folds<-createFolds(y=y,k = k,list=TRUE,returnTrain = TRUE)
  values = list()
  i<-1;
  for(fold in folds){
    X<-more_features[fold,]; Xt<-more_features[-fold,]
    Y<-y[fold]; Yt<-y[-fold]
    
    train_of_fold<-cbind(X,Y);
    test_of_fold<-cbind(Xt,Yt);
    
    colNames<-paste0("feat_",1:(ncol(train_of_fold)-1))
    colNames<-append(colNames,"target")
    
    colnames(train_of_fold)<-colNames
    colnames(test_of_fold)<-colNames
    key<-paste0("fold",i);
    print(key)
    values[[key]]$train<-train_of_fold;
    values[[key]]$test<-test_of_fold;
    
    i<-i+1;
    
  }
  return(values)
}