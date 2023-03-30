getwd()
library(phytools)
trees<-list()
births<-vector()
Fractions<- vector()
random<- c()
random2<-c()
treelist<-c()
for(i in 1:100) {
  births[i]<- runif(1)
  Fractions[i]<- runif(1)
  trees[[i]]<- pbtree(b=births[i], d=(Fractions[i]*births[i]), n=100)
  random[[i]]<- births[i]
  random2[[i]]<- (Fractions[i])
  treelist[[i]]<- mean(trees[[i]]$edge.length)
}
pdf("Trees.pdf")
plot(trees[[i]])
dev.off()
trees[[100]]$tip.label
sapply(trees,Ntip)
tips<- log(sapply(trees,Ntip))
random3<-unlist(random)
head(tips)
pdf("logtip&diversificationQ4.pdf")
plot(tips,xlab="log of tips", ylab="net diversification", pch=16)
dev.off()
cor(tips,random3)
# The test shows there's not much of correlation between the logs of the total number of tips and the net diversification 
random4<- unlist(random2)
treelist2<- unlist(treelist)
plot(treelist2, random3, xlab="avg. branch length", ylab="Speciation Rate", pch=4)
pdf("Avg.branchlength&SpeciationRateQ5,6.pdf")
plot(treelist2, random3, xlab="avg. branch length", ylab="Speciation Rate", pch=1)
dev.off()
cor(treelist2,random4)
#The correlation test shows a negative value telling us the two do not have a relationship
Tree<- trees[[which.max(tips)]]
pdf("TreeQ7.pdf")
plot(Tree)
dev.off()
rates<- vector()
traits<-c()
meantraits<-c()
vartraits<-c()
for(i in 1:100) {
  rates[i]<-runif(1)
  traits[[i]]<-fastBM(tree=Tree, sig2=rates[i])
  meantraits[[i]]<-mean(traits[[i]])
  vartraits[[i]]<-var(traits[[i]])
}
meantraits<-unlist(meantraits)
pdf("meanoftraits&rateQ8.pdf")
plot(meantraits, rates)
dev.off()
cor(meantraits,rates)
#The small number given from the correlation test shows there's little relationship between the mean of traits and the rates
vartraits<-unlist(vartraits)
pdf("varianceoftraits&ratesQ9.pdf")
plot(vartraits,rates)
dev.off()
cor(vartraits,rates)
#The higher correlation number here shows that there's more of a relationship between the variance of traits and rates, than that of mean of traits and rates
pdf("08_plot.pdf")
plot(traits[[1]],traits[[2]])
dev.off()
cor(traits[[1]], traits[[2]])
traitMat<- cbind(traits[[1]], traits[[4]])
#The small number shows the 2 traits are not very closely related with one another 


#Extra Credit 
pdf("08_EC.pdf")
phylomorphospace(Tree, traitMat, xlab="Trait 1", ylab="Trait 2")
dev.off()
