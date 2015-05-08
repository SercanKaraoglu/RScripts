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
## Step 4 - Use H2O Flow to inspect the data and build some models on 
## train_holdout.hex/valid_holdout.hex to get a feeling for the problem
######################################################################

## Connect browser to http://localhost:54321 (or http://cluster-node-ip:port)


######################################################################
## Step 5 - GBM Hyper-Parameter Tuning with Random Search
######################################################################
# 
model <- h2o.gbm(x=predictors, 
                 y=response, 
                 data=train_holdout.hex,
                 distribution="multinomial",
                 n.trees=42, 
                 interaction.depth=10, 
                 n.minobsinnode=10, 
                 shrinkage=0.175
                 )

test.pred<-h2o.predict(object = model,newdata = valid_holdout.hex)
h2o.confusionMatrix(data = predict[,1],valid_holdout.hex$target)

######################################################################
## Step 6 - Build Final Model using the Full Training Data
######################################################################

model <- h2o.gbm(x=predictors, 
                 y=response, 
                 data=train.hex,
                 distribution="multinomial",
                 n.trees=42, 
                 interaction.depth=10, 
                 n.minobsinnode=10, 
                 shrinkage=0.175
                 )

test.pred<-h2o.predict(object = model,newdata = valid_holdout.hex)

######################################################################
## Step 7 - Make Final Test Set Predictions for Submission
######################################################################

## Predictions: label + 9 per-class probabilities
test.pred<-h2o.predict(object = model,newdata = test.hex)
head(pred)

## Remove label
test.pred <- test.pred[,-1]
head(pred)

## Paste the ids (first col of test set) together with the predictions
submission <- cbind(test.hex[,1], test.pred)
head(submission)

## Save submission to disk
h2o.exportFile(submission, paste0(dir, "submission_gbm.csv"))