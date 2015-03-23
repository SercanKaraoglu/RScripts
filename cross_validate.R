require(xgboost)
param <- list("objective" = "multi:softprob",
              "num_class" = 61877,
              "bst:eta" = 0.1,
              "bst:max_depth" = 6,
              "eval_metric" = "logloss",
              "eval_metric" = "auc",
              "silent" = 0,
              "nthread" = 16);
watchlist <- list("train" = xgmat)
nround = 120
msp<-xgb.train(param, data=xgmat,nround=120)
xgb.save(msp, 'xgb.model')