x <- fread(paste0(root_dir,"/train.csv"))
test <- fread(paste0(root_dir,"/test.csv"))


## prep the species column by moving the test-only UNSPECIFIED CULEX to CULEX ERRATICUS, and re-doing the levels
## logistic regression will complain otherwise
vSpecies<-c(as.character(x$Species),as.character(test$Species))
vSpecies[vSpecies=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"
vSpecies[-which(vSpecies == "CULEX PIPIENS" |
                  vSpecies == "CULEX PIPIENS/RESTUANS" |
                  vSpecies == "CULEX RESTUANS")] = "CULEX OTHER"
vSpecies<-factor(vSpecies,levels=unique(vSpecies))
vSpecies<-as.numeric(vSpecies)

## data.table syntax for adding a column; could overwrite the existing column as well
x[,Species:=vSpecies[1:nrow(x)]]
test[,Species:=vSpecies[(nrow(x)+1):length(vSpecies)]]

## also add some fields for components of the date using simple substrings
x[,dMonth:=as.factor(paste(substr(x$Date,6,7)))]
x[,dYear:=as.factor(paste(substr(x$Date,1,4)))]
x$Date = as.Date(x$Date, format="%Y-%m-%d")
xsDate = as.Date(paste0(x$dYear, "0101"), format="%Y%m%d")
x$dWeek = as.numeric(paste(floor((x$Date - xsDate + 1)/7)))

test[,dMonth:=as.factor(paste(substr(test$Date,6,7)))]
test[,dYear:=as.factor(paste(substr(test$Date,1,4)))]
test$Date = as.Date(test$Date, format="%Y-%m-%d")
tsDate = as.Date(paste0(test$dYear, "0101"), format="%Y%m%d")
test$dWeek = as.numeric(paste(floor((test$Date - tsDate + 1)/7)))

# we'll set aside 2011 data as test, and train on the remaining
my.x = data.frame(x[,list(WnvPresent, dWeek, Species, Latitude, Longitude)])
x1<-my.x[x$dYear!=2011,]
x2<-my.x[x$dYear==2011,]
props<-c("dWeek","Species","Latitude","Longitude");
xAsMatrix<-as.matrix(my.x[,props],rownames.force = FALSE)
x1AsMatrix<-as.matrix(x1[,props],rownames.force = FALSE)
x2AsMatrix<-as.matrix(x2[,props],rownames.force = FALSE)

testAsMatrix = data.frame(test[,list(dWeek, Species, Latitude, Longitude)])
testAsMatrix<-as.matrix(testAsMatrix[,props],rownames.force = FALSE)

y1<-as.numeric(x1[,"WnvPresent"]);
y2<-as.numeric(x2[,"WnvPresent"]);
y<-as.numeric(my.x[,"WnvPresent"]);

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
  helper_transform <- function (x) {
    x = as.matrix(x)
    x = matrix(as.numeric(x),nrow(x),ncol(x))
    features<-x[,2:ncol(x)];
    return(scale(cbind(sqrt(features),log(1+features, base = exp(1)))));
  }
  
  if(forPrediction==TRUE){
    return(DataContainer(x=helper_transform(data),y=1));
  }else{
    y = data[,ncol(data)]
    y = gsub('Class_','',y)
    y = as.integer(y)-1 #xgboost take features in [0,numOfClass)
    
    return(DataContainer(x=helper_transform(data[,-ncol(data)]),y=y)); 
  }
}