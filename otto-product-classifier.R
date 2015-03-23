require(xgboost)
data <- read.csv("~/Dropbox/shared_workspace/otto-group-product-classification/src/main/resources/train.csv")
testPercentage = 0.3;

theData<-sparse.model.matrix(target~., data = data[-1])
label <- data[[95]];

testsize<-floor(testPercentage*length(label));
weight <- as.numeric(data[[94]]) * testsize / length(label);

xgmat <- xgb.DMatrix(theData, label = label, weight = weight, missing = -999.0)
