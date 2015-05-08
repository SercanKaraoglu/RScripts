require(xgboost)
require(methods)
<<<<<<< Updated upstream

otto_dir<-'/Dropbox/otto'
trainDir <- paste0(path.expand("~"),otto_dir,'/train.csv')
testDir <- paste0(path.expand("~"),otto_dir,'/test.csv')

=======
dir <- paste0(path.expand("~"), "/Dropbox/otto/")
trainDir <- paste0(dir, "train.csv")
testDir  <- paste0(dir, "test.csv")
>>>>>>> Stashed changes
train = read.csv(trainDir,header=TRUE,stringsAsFactors = F)
test = read.csv(testDir,header=TRUE,stringsAsFactors = F)
trainCopy<-train[,-1]
testCopy<-test[,-1]
<<<<<<< Updated upstream

# Find zero variances
=======
>>>>>>> Stashed changes
# nzv <- nearZeroVar(trainCopy)
# trainCopy <- trainCopy[, -nzv]
# testCopy <- testCopy[, -nzv]

y = trainCopy[,ncol(trainCopy)]
y = gsub('Class_','',y)
y = as.integer(y)-1 #xgboost take features in [0,numOfClass)

x = rbind(trainCopy[,-ncol(trainCopy)],testCopy)
x = as.matrix(x)
x = matrix(as.numeric(x),nrow(x),ncol(x))
trind = 1:length(y)
teind = (nrow(trainCopy)+1):nrow(x)

# Set necessary parameter
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "eta" = 0.25,
              "max_depth"=15,
              "num_class" = 9,
              "nthread" = 3)
runCrossValidation()
# Run Cross Valication
preprocess <- function(x){
  datExpr <- x[trind,]
  #datExpr = log(1+datExpr);
  datExpr <- scale(datExpr, center = TRUE, scale = FALSE)
  variancedatExpr <- apply( datExpr,2,var, na.rm=T)
  datExpr<-x[trind,variancedatExpr >= 0.3]
  datExpr <- log(1+datExpr)
  datExpr;
}
preprocess2<-function(x){
  rows = sample(1:nrow(x), nrow(x)/2, replace = TRUE)
  training<-x[rows,]
  preProcValues <- preProcess(training, method = c("center", "scale"))
  trainTransformed <- predict(preProcValues, x)
  trainTransformed;
}
runCrossValidation <- function(){
  cv.nround = 200
  #datExpr<-preprocess(x);
  datExpr <- xTrain
  bst.cv = xgb.cv(param=param, data = datExpr, label = y, 
                  nfold = 5, nrounds=cv.nround)
  #bst;
}
# Train the model
trainModel<- function(){
  nround = 160
  bst = xgboost(param=param, data = x[trind,], label = y, nrounds=nround)
  bst;
}
# Make prediction
makePrediction <- function(){
  pred = predict(bst,x[teind,])
  pred = matrix(pred,9,length(pred)/9)
  pred = t(pred)
  pred;
}
# Output submission
outputSubmission <- function(){
  pred = format(pred, digits=2,scientific=F) # shrink the size of submission
  pred = data.frame(1:nrow(pred),pred)
  names(pred) = c('id', paste0('Class_',1:9))
  write.csv(pred,file='~/Dropbox/otto/submissionScaled.csv', quote=FALSE,row.names=FALSE) 
}