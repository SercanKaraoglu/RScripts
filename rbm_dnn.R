require(caret)
require(deepnet)
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

shufleIndex<-sample(nrow(x[trind,]))
xTrain<-x[shufleIndex,]
y<-y[shufleIndex]
folds<-createFolds(y=y,k = 5,list=TRUE,returnTrain = TRUE)
fold<-folds$Fold1
X<-xTrain[fold,]; Xt<-xTrain[-fold,]
Y<-y[fold]; Yt<-y[-fold]

rbm1<-newRBM(93, 93, batchSize =  93, ff = FALSE,genWeightFunc = generateWeights)
trainRBM1<-trainRBM(rbm1,trainData=X,maxEpoch=3,numCD=4)
rbm2<-newRBM(93, 93, batchSize = 93, ff = FALSE,genWeightFunc = generateWeights)
trainRBM2<-trainRBM(rbm2,trainData=rbm1@weights,maxEpoch=5,numCD=10)
rbm3<-newRBM(93, 93, batchSize = 93, ff = FALSE,genWeightFunc = generateWeights)
trainRBM3<-trainRBM(rbm3,trainData=rbm2@weights,maxEpoch=5,numCD=10)

darch<-newDArch(layers = c(93,512,512,9),batchSize = 128,ff=FALSE,genWeightFunc = generateWeights)
setLayerWeights(darch,1)<-getWeights(rbm1)
setLayerWeights(darch,2)<-getWeights(rbm2)
setLayerWeights(darch,3)<-getWeights(rbm3)

# Prepare the layers for backpropagation training for
# backpropagation training the layer functions must be
# set to the unit functions which calculates the also
# derivatives of the function result.

layers <- getLayers(darch)
for(i in length(layers):1){
  layers[[i]][[2]] <- sigmoidUnitDerivative
}

setLayers(darch) <- layers
rm(layers)

# Setting and running the Fine-Tune function
setFineTuneFunction(darch) <- rpropagation
darch <- fineTuneDArch(darch,trainData=X,targetData=Y,
                       maxEpoch=200,
                       isBin=F,isClass=T,validData=Xt,validTargets=Yt)

# Running the darch
darch <- darch <- getExecuteFunction(darch)(darch,inputs)
outputs2 <- getExecOutputs(darch)
cat(outputs2[[length(outputs2)]])