require(caret)
otto_dir<-'/Dropbox/otto'
trainDir <- paste0(path.expand("~"),otto_dir,'/train.csv')
train = read.csv(trainDir,header=TRUE,stringsAsFactors = F)

testDir <- paste0(path.expand("~"),otto_dir,'/test.csv')
test = read.csv(testDir,header=TRUE,stringsAsFactors = F)

y = train[,ncol(train)]
y = gsub('Class_','',y)
y = as.integer(y)-1 #xgboost take features in [0,numOfClass)

x = train[,-ncol(train)]
x = as.matrix(x)
x = matrix(as.numeric(x),nrow(x),ncol(x))
features<-x[,2:94];
more_features<-cbind(x,cbind(sqrt(features),log(1+features, base = exp(1))),y)
folds<-createFolds(y=y,k = 2,list=TRUE,returnTrain = TRUE)
fold<-folds$Fold1
X<-more_features[fold,]; Xt<-more_features[-fold,]
Y<-y[fold]; Yt<-y[-fold]