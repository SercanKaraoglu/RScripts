library(h2oEnsemble)
library(SuperLearner)  # For metalearner such as "SL.glm"
library(cvAUC)  # Used to calculate test set AUC (requires version >=1.0.1 of cvAUC)

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

h2o.gradientBoostedMachine.1 <- function(..., n.trees=42,interaction.depth=10,n.minobsinnode=10,shrinkage=0.175) h2o.gbm.wrapper(...,n.trees=n.trees,interaction.depth=interaction.depth,n.minobsinnode=n.minobsinnode,shrinkage=shrinkage)
h2o.randomForest.1 <- function(..., ntree = 1000, nbins = 100, seed = 1) h2o.randomForest.wrapper(..., ntree = ntree, nbins = nbins, seed = seed)
#h2o.deeplearning.1 <- function(..., hidden = c(512,512,512,512), activation = "Rectifier", seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)

#learner <- c("h2o.gradientBoostedMachine.1","h2o.randomForest.1", "h2o.deeplearning.1")
learner <- c("h2o.gradientBoostedMachine.1","h2o.randomForest.1")
family <- "binomial"

# Train the ensemble using 4-fold CV to generate level-one data
# More CV folds will take longer to train, but should increase performance
fit <- h2o.ensemble(x = predictors, 
                    y = response, 
                    data = train_holdout.hex, 
                    family = family, 
                    learner = learner,
                    cvControl = list(V=4,shuffle=TRUE),
                    seed=TRUE,
                    parallel="seq")

# Generate predictions on the test set
pred <- predict(fit, valid_holdout.hex)
labels <- as.data.frame(newdata[,c(y)])[,1]


# Ensemble test AUC 
AUC(predictions=as.data.frame(pred$pred)[,1], labels=labels)