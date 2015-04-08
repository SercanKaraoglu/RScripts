library(h2o)
localH2o<-h2o.init(ip="localhost",port=54321, startH2O =TRUE)
train_hex <- h2o.importFile(localH2o,path='/home/sercan/Dropbox/otto/train.csv')
test_hex <- h2o.importFile(localH2o,path='/home/sercan/Dropbox/otto/test.csv')
record_model<-h2o.deeplearning(x=2:94,
                               y=95,
                               data=train_hex,
                               nfold=3,
                               activation="RectifierWithDropout",
                               hidden=c(128,128,256),
                               epochs=8000,
                               l1=1e-5,
                               l2=1e-5,
                               input_dropout_ratio=0.2,
                               train_samples_per_iteration=-1,
                               classification_stop=-1
                               )
