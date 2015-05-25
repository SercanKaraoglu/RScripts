train = train[,c(23,93,121,122)]
test = test[,c(23,93,121,122)]

colNames<-append("RSIDiff",colnames(train))

train = cbind(append(0,diff(train[,"RSI15"])),train)
test = cbind(append(0,diff(test[,"RSI15"])),test)
colnames(train)<-colNames
colnames(test)<-colNames

colNames<-append("MACDDiff",colNames)
train = cbind(append(0,diff(train[,"MACD"])),train)
test = cbind(append(0,diff(test[,"MACD"])),test)
colnames(train)<-colNames
colnames(test)<-colNames

colNames<-append("RSISecondDiff",colnames(train))

train = cbind(append(0,diff(train[,"RSI15"])),train)
test = cbind(append(0,diff(test[,"RSI15"])),test)
colnames(train)<-colNames
colnames(test)<-colNames

colNames<-append("MACDSecondDiff",colnames(train))

train = cbind(append(0,diff(train[,"MACD"])),train)
test = cbind(append(0,diff(test[,"MACD"])),test)
colnames(train)<-colNames
colnames(test)<-colNames

colNames<-append("RSIDiffWithVolume",colNames)

train<-cbind(train[,"RSIDiff"]*train[,"Volume"],train)
test<-cbind(test[,"RSIDiff"]*test[,"Volume"],test)
colnames(train)<-colNames
colnames(test)<-colNames