otto_dir<-'/Dropbox/otto'
trainDir <- paste0(path.expand("~"),otto_dir,'/train.csv')
train = read.csv(trainDir,header=TRUE,stringsAsFactors = F)

testDir <- paste0(path.expand("~"),otto_dir,'/test.csv')
test = read.csv(testDir,header=TRUE,stringsAsFactors = F)

DataContainer <- setClass(
  # Set the name for the class
  "DataContainer",
  
  #slots
  slots = c(
    x = "matrix",
    y = "numeric"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    x = matrix(),
    y = numeric()
  )
)

setGeneric(name="forPrediction",
           def=function(theObject)
           {
             standardGeneric("forPrediction")
           }
)
setMethod(f="forPrediction",
          signature="DataContainer",
          definition=function(theObject)
          {
            if(theObject@y < 0) {
              return(TRUE);
            }
            return(FALSE);
          }
)
transform <- function (forPrediction=FALSE, data) {
  helper_transform <- function () {
    x = as.matrix(x)
    x = matrix(as.numeric(x),nrow(x),ncol(x))
    features<-x[,2:ncol(x)];
    return(scale(cbind(sqrt(features),log(1+features, base = exp(1)))));
  }
  
  if(forPrediction==TRUE){
    return(DataContainer(x=helper_transform(),y=1));
  }else{
    y = data[,ncol(data)]
    y = gsub('Class_','',y)
    y = as.integer(y)-1 #xgboost take features in [0,numOfClass)
    
    x = data[,-ncol(data)]
    
    return(DataContainer(x=helper_transform(),y=y)); 
  }
}