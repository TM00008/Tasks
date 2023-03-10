getwd()
setwd("/Users/macbookair")
data<-read.csv("/Users/macbookair/DataSet1.csv")
setwd("/Users/macbookair/Desktop/Evolution/Tasks/Project")
data<-read.csv("/Users/macbookair/Desktop/Evolution/Tasks/Project/DataSet1.csv", header=T)
plot(data)
par(mar=c(1,1,1,1))
ggplot2::aes(data$Specimen, data$Centrum.Length)
ggplot2::geom_histogram(data)
plot(data$Specimen, data$Centrum.Length, xlab="Specimen", ylab="Centrum Length")
plot(data$Centrum.Length, data$Centrum.Width, xlab="Length", ylab="Width")
par(mfrow=c(1,2),mar=c(6,4,1,1))
barplot(cbind(as.numeric(data$Specimen[,2]), as.numeric(data$Centrum.Length[,3]), beside=TRUE,xlab="Specimen", ylab="")

        