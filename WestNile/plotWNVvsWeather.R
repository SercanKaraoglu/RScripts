root_dir<-"~/git/RScripts/WestNile/"
trainDir<-paste0(root_dir,"train.csv")
weatherDir<-paste0(root_dir,"weather.csv")
library(readr)
train <- read_csv(trainDir)
weather <- read_csv(weatherDir)



# Write to the log:
cat("\n")
cat(sprintf("Training set has %d rows and %d columns\n", nrow(train), ncol(train)))

# Generate output files with write_csv(), plot() or ggplot()
# Any files you write to the current directory get shown as outputs

library(plyr)
library(ggplot2)

station_1 = weather[weather$Station == 1, c('Date','Tmax','Tmin','PrecipTotal')]
station_1$Date=as.Date(station_1$Date,format="%Y-%m-%d")
station_1$Year = format(station_1$Date,"%Y")
station_1$DOY = as.numeric(strftime(station_1$Date,'%j'))
#station_1$PrecipTotal = as.numeric(levels(station_1$PrecipTotal))[station_1$PrecipTotal]
#station_1$PrecipTotal[is.na(station_1$PrecipTotal)] = 0

WNV = train[,c('Date','WnvPresent')]
WNV$Year = format(WNV$Date,'%Y')
WNV$DOY = as.numeric(strftime(WNV$Date,'%j'))


ggplot() + 
  geom_line(aes(DOY,Tmax), data = station_1) +
  geom_line(aes(DOY,Tmin), data = station_1) +
  #geom_line(aes(DOY,Precip_cumsum), col='blue', data = station_1) +
  geom_vline(aes(xintercept = DOY,alpha = .1,
                 colour='red',size=WnvPresent),data=WNV) + 
  labs(x = 'DOY', y = 'Temperature / Cumulative Precipitation',
       title = 'WNV Occurences vs Weather') + 
  facet_wrap(~Year)