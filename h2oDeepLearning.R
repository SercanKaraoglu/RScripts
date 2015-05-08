<<<<<<< Updated upstream
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

=======
## Launch h2o on localhost, using all cores
h2oServer = h2o.init(nthreads=-1)

## Point to directory where the Kaggle data is
dir <- paste0(path.expand("~"), "/Dropbox/otto/")

## For Spark/Hadoop/YARN/Standalone operation on a cluster, follow instructions on http://h2o.ai/download/
## Then connect to any cluster node from R

#h2oServer = h2o.init(ip="mr-0xd1",port=53322)
#dir <- "hdfs://mr-0xd6/users/arno/h2o-kaggle/otto/"


######################################################################
## Step 3 - Import Data and create Train/Validation Splits
######################################################################
trainDir <- paste0(dir, "train.csv")
testDir  <- paste0(dir, "test.csv")
train.hex <- h2o.importFile(h2oServer,path=trainDir,key="train.hex")
test.hex <- h2o.importFile(h2oServer,path=testDir,key="test.hex")
dim(train.hex)
summary(train.hex)

predictors <- 2:(ncol(train.hex)-1) #ignore first column 'id'
response <- ncol(train.hex)

## Split into 80/20 Train/Validation
rnd <- h2o.runif(train.hex)
train_holdout.hex <- h2o.assign(train.hex[rnd<0.8,], "train_holdout.hex")
valid_holdout.hex <- h2o.assign(train.hex[rnd>=0.8,], "valid_holdout.hex")

######################################################################
## Step 5 - GBM Hyper-Parameter Tuning with Random Search
######################################################################
# 
## Settings
ensemble_size <- 2
n_fold = 3
reproducible_mode = F # set to TRUE if you want reproducible results, e.g. for final Kaggle submission if you think you'll win :)  Note: will be slower
seed0 = 1337 # Only really matters for reproducible_mode = T
record_model<-h2o.deeplearning(x=predictors, 
                               y=response, 
                               data=train_holdout.hex,
                               classification = T,
                               activation="RectifierWithDropout",
                               hidden = c(100,100),
                               hidden_dropout_ratios = c(0.0,0.0),
                               input_dropout_ratio = 0,
                               epochs = 100,
                               l1 = c(0,1e-5),
                               l2 = c(0,1e-5)
                               )

test.pred<-h2o.predict(object = record_model,newdata = valid_holdout.hex)
h2o.confusionMatrix(data = test.pred[,1],valid_holdout.hex$target)

######################################################################
## Step 6 - Build Final Model using the Full Training Data
######################################################################
record_model<-h2o.deeplearning(x=predictors, 
                               y=response, 
                               data=train.hex,
                               sparse=T,
                               loss='CrossEntropy',
                               balance_classes = T,
                               force_load_balance = T,
                               nfold=3,
                               activation="RectifierWithDropout",
                               hidden=c(93,128,93),
                               epochs=300,
                               l1=1e-5,
                               l2=1e-5,
                               input_dropout_ratio=0.2,
                               train_samples_per_iteration=-1,
                               classification_stop=-1
)

## Predictions: label + 9 per-class probabilities
test.pred<-h2o.predict(object = record_model,newdata = test.hex)
head(pred)

## Remove label
test.pred <- test.pred[,-1]
head(pred)

## Paste the ids (first col of test set) together with the predictions
submission <- cbind(test.hex[,1], test.pred)
head(submission)

## Save submission to disk
h2o.exportFile(submission, paste0(dir, "submission_gbm.csv"))
>>>>>>> Stashed changes
