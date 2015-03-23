addSignal <- function(dF){
  signals<-ifelse(diff(dF[,100])<0,"BUY","SELL");
  result<-c("");
  if(signals[1]=="BUY"){
    result <- append(c("BUY"),signals);
  }else{
    result <- append(c("SELL"),signals);
  }
  result;
}