tickTime <- fxu030Tik['Saat']
tickPrice <- fxu030['Fiyat']

dateTime <- paste(t(tickDate),t(tickTime))
sample <- xts(tickPrice, as.POSIXct(dateTime, format="%d/%m/%Y %H:%M:%S"))
colnames(sample) <- c("DATE","PRICE")