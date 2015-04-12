otto_dir<-'/Dropbox/otto'
trainDir <- paste0(path.expand("~"),otto_dir,'/train.csv')
testDir <- paste0(path.expand("~"),otto_dir,'/test.csv')

train = read.csv(trainDir,header=TRUE,stringsAsFactors = F)
test = read.csv(testDir,header=TRUE,stringsAsFactors = F)

train<-train[,-1]
test<-test[,-1]


y = train[,ncol(train)]
y = gsub('Class_','',y)
y = as.integer(y)-1 #xgboost take features in [0,numOfClass)

x = rbind(train[,-ncol(train)],test)
x = as.matrix(x)
x = matrix(as.numeric(x),nrow(x),ncol(x))
trind = 1:length(y)
teind = (nrow(train)+1):nrow(x)

xTrain<-x[trind,]
folds<-createFolds(y=y,k = 5,list=TRUE,returnTrain = TRUE)
fold<-folds$Fold1
X<-xTrain[fold,]; Xt<-xTrain[-fold,]
Y<-y[fold]; Yt<-y[-fold]

dnn<-sae.dnn.train(x=log(1+X),y=Y,hidden=c(5,5),activationfun = "tanh",learningrate = 0.0007,momentum = 0.9,learningrate_scale = 0.1,numepochs = 1000,batchsize = 256,visible_dropout = 0.5,hidden_dropout=0.5)

