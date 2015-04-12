library(h2o)
library(SuperLearner)
localH2o<-h2o.init(ip="localhost",port=54321, startH2O =TRUE,nthreads=2)
otto_dir<-'/Dropbox/otto'
trainDir <- paste0(path.expand("~"),otto_dir,'/train.csv')
testDir <- paste0(path.expand("~"),otto_dir,'/test.csv')

train_hex <- h2o.importFile(localH2o,path=trainDir)
test_hex <- h2o.importFile(localH2o,path=testDir)

s = h2o.runif(train_hex)
summary(s)
train_split = train_hex[s <= 0.8,]
train_split = h2o.assign(train_split, "train_split")
validation_split = train_hex[s > 0.8,]
validation_split = h2o.assign(validation_split, "validation_split")

predictors<-2:94
response<-95

h2o.randomForest.1 <- function(..., ntree = 600, nbins = 20, seed = 1) h2o.randomForest.wrapper(..., ntree = ntree, nbins = nbins, seed = seed)
h2o.deeplearning.1 <- function(..., hidden = c(93,93), activation = "Rectifier", seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.gbm.1 <- function(...,distribution = "multinomial",n.trees = 300,interaction.depth = 6,n.minobsinnode = 10,shrinkage = 0.25,n.bins = 20) h2o.gbm.wrapper(...,distribution = distribution,n.trees = n.trees, interaction.depth = interaction.depth,n.minobsinnode = n.minobsinnode,shrinkage = shrinkage,n.bins = n.bins)
family <- "binomial"
learner<- c("h2o.randomForest.1","h2o.deeplearning.1","h2o.gbm.1")
metalearner <- c("SL.glm")

fit <- h2o.ensemble(x = predictors, 
                    y = response, 
                    data = train_split, 
                    family = family, 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V=4))

otto.predict<-predict(object = fit,newdata = validation_split)
h2o.confusionMatrix(data = otto.predict[,1],reference = validation_split$target)

