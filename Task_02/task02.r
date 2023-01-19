Data1 <- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
Data2 <- read.csv('http://jonsmitchell.com/data/cyrus.csv', stringsAsFactors=F)
write.csv(Data1, 'rawdata.csv', quote=F)
length(Data1)
nrow(Data1)
ncol(Data1)
colnames(Data1)
head(Data1)
Data1[1,]
Data1[2,]
Data1[1:3,]
Data1[1:3,4]
Data1[1:5, 1:3]
Feeds <- which(Data1[,9]=='bottle')
berenMilk <- Data1[Feeds,]
head(berenMilk)
Feeds <- which(Data1[,'event']=='bottle')
Feeds <- which(Data1$event=='bottle')
dayID <- apply(Data1,1,function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format= "%Y-%m-%d", origin="2019-04-18")
Data1$age <- dateID - dateID[which(Data1$event == 'birth')]
head(Data1)
beren2 <- Data1
beren3 <- beren2[order(beren2$age),]
write.csv(beren3,'beren_new.csv', quote=F, row.names=FALSE)
