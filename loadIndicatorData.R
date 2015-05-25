require(caret)

root_dir<-'/Development/QuantData'

trainDir <- paste0(path.expand("~"),root_dir,'/no_normalization_freq_2_trainData.csv')
train = read.csv(trainDir,header=TRUE,stringsAsFactors = F)

testDir <- paste0(path.expand("~"),root_dir,'/no_normalization_freq_2_testData.csv')
test = read.csv(testDir,header=TRUE,stringsAsFactors = F)