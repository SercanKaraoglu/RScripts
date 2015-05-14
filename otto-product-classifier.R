column_bind<-function(data){
  deepfeatures_layer<-cbind(data[,2:ncol(data)],
                            data[,1]);
  deepfeatures_layer;
}
extractFeature<-function(layerNumber,model,train){
  train<-train[,-1]
  for(i in 1:93){
    train[,i] <- as.numeric(train[,i])
    train[,i] <- sqrt(train[,i]+(3/8))
  }
  
  train.hex <- as.h2o(localH2O,train)
  deepfeatures_layer = h2o.deepfeatures(train.hex, model, layer = layerNumber)
  deepfeatures_layerAsDataFrame<-as.data.frame(deepfeatures_layer);
  deepfeatures_layer<-column_bind(deepfeatures_layerAsDataFrame);
  deepfeatures_layer<-as.matrix(deepfeatures_layer);
  deepfeatures_layer<-matrix(as.numeric(deepfeatures_layer),nrow(deepfeatures_layer),ncol(deepfeatures_layer))
  deepfeatures_layer;
}

fold1<-cbind(X,Y)
colnames(fold1)<-colnames(train)

fold2<-cbind(Xt,Yt)
colnames(fold2)<-colnames(fold1)

model1<-getModel(fold1)
model2<-getModel(fold2)
####################################################################################
##Extract Features
####################################################################################
model1_deep_feat_1<-extractFeature(1,model1,fold2)
model1_deep_feat_2<-extractFeature(2,model1,fold2)
model1_deep_feat_3<-extractFeature(3,model1,fold2)

model2_deep_feat_1<-extractFeature(1,model2,fold1)
model2_deep_feat_2<-extractFeature(2,model2,fold1)
model2_deep_feat_3<-extractFeature(3,model2,fold1)

####################################################################################
##Classification with Extracted Features
####################################################################################

crossValidateXgBoost(model1_deep_feat_1,FALSE,100,50)
crossValidateXgBoost(model1_deep_feat_2,FALSE,20,12)
crossValidateXgBoost(model1_deep_feat_3,FALSE,20,12)

crossValidateXgBoost(model2_deep_feat_1,FALSE,20,12)
crossValidateXgBoost(model2_deep_feat_2,FALSE,20,12)
crossValidateXgBoost(model2_deep_feat_3,FALSE,20,12)

first_xg1<-GetXGModel(model1_deep_feat_1,FALSE,20,12)
first_xg2<-GetXGModel(model1_deep_feat_2,FALSE,20,12)
first_xg3<-GetXGModel(model1_deep_feat_3,FALSE,20,12)

second_xg1<-GetXGModel(model2_deep_feat_1,FALSE,20,12)
second_xg2<-GetXGModel(model2_deep_feat_2,FALSE,20,12)
second_xg3<-GetXGModel(model2_deep_feat_3,FALSE,20,12)
