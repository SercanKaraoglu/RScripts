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
require("fastICA")
require("caret")
#procValues<-"pre process values for train data"
transform <- function (forPrediction=FALSE, data) {
  transform_wrapper <- function (x) {
    scaledTraindata <-  predict(procValues, x)
    return(scaledTraindata);
  }
  x = data[,-ncol(data)]
  x = as.matrix(x)
  x = matrix(as.numeric(x),nrow(x),ncol(x))
  
  y = data[,ncol(data)]
  
  if(forPrediction!=TRUE){
    assign("procValues", 
           preProcess(x, method = c("ica"),n.comp=2), 
           envir = .GlobalEnv);
  }
  scaled <- predict(procValues, x)
  return(DataContainer(x=scaled, y=y)); 
  
}