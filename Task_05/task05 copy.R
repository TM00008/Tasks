library(adegenet)
library(poppr)
library(dplyr)
library(hierfstat)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(scales)
lobster <- read.csv("https://jonsmitchell.com/data/lobster_genotypes.csv")
lobster_wide <- reshape(lobster, idvar=c("ID", "Site"), timevar="Locus", direction="wide", sep="")
colnames(lobster_wide) <- gsub("Genotype", "", colnames(lobster_wide))
snpgeno <- lobster_wide[, 3:ncol(lobster_wide)]
snps_to_remove <- c("25580", "32362", "41521", "53889", "65376", "8953", "21197", "15531", "22740", "28357", "33066", "51507", "53052", "53263", "21880", "22323", "22365")
snpgeno <- snpgeno[, !colnames(snpgeno) %in% snps_to_remove]
ind <- as.character(lobster_wide$ID)
site <- as.character(lobster_wide$Site)
lobster_gen <- df2genind(snpgeno, ploidy=2, ind.names=ind, pop=site, sep="")
lobster_gen <- missingno(lobster_gen, type="geno", cutoff=0.20)
mlg(lobster_gen)
dups_lobster <- mlg.id(lobster_gen)


lob_dups <- c()
x <- 1
for (i in dups_lobster) {
if (length(dups_lobster[i])>1){
lob_dups[x] <- i
x <- x+1  
}
}  
lob_Nodups <- indNames(lobster_gen)[! indNames(lobster_gen) %in% lob_dups]
lobster_gen <- lobster_gen[lob_Nodups,]
summary(lobster_gen$pop)
basic_lobster <- basic.stats(lobster_gen,diploid=TRUE)
Ho_lobster<-round(apply(basic_lobster$Ho, MARGIN =2, FUN= mean, na.rm=TRUE), digits=3)
He_lobster <- round(apply(basic_lobster$Hs, MARGIN=2, FUN=mean, na.rm=TRUE), digits=3)
par(mar=c(4,4,1,1), las=1, mgp=c(2,0.25,0), tck=-0.005, cex.lab=1.25)
plot(He_lobster, Ho_lobster, xlab="expected H", ylab="observed H", pch=21, bg="gray", xlim=c(0.32,0.405), ylim=c(0.32,0.405))
abline(0,1,lty=3)
abline(lm(Ho_lobster~He_lobster), lty=2, lwd=1.25, col='red')
Fis<- apply(basic_lobster$Fis, MARGIN=2, FUN=mean, na.rm=TRUE)
lobster_gen_sub<- popsub(lobster_gen, sublist=c("Ale", "Ber", "Brd", "Pad", "Sar17", "Vig"))
lobster_fst<- genet.dist(lobster_gen_sub, method="WC84") %>%round(digits=3)
lab_order<- c("Ber", "Brd", "Pad", "Vig", "Sar17", "Ale")
fst.mat<- as.matrix(lobster_fst)
fst.mat1<-fst.mat[lab_order,]
fst.mat2 <- fst.mat1[,lab_order]
ind<-which(upper.tri(fst.mat2), arr.ind=TRUE)
fst.df<-data.frame(Site1=dimnames(fst.mat2)[[2]][ind[,2]],Site2=dimnames(fst.mat2)[[1]][ind[,1]], Fst=fst.mat2[ ind ])
fst.df$Site1 <- factor(fst.df$Site1,levels=unique(fst.df$Site1))
fst.df$Site2<- factor(fst.df$Site2,levels=unique(fst.df$Site2))
fst.df$Fst[fst.df$Fst<0] <- 0
fst.label <- expression(italic("F") [ST])
mid <- max(fst.df$Fst)/2
ggplot(data=fst.df,aes(x=Site1, y=Site2, fill=Fst))+geom_tile(colour="black")+geom_text(aes(label=Fst), color="black", size=3)+scale_fill_gradient2(low="blue", mid="pink", high="red", midpoint=mid, name=fst.label,limits=c(0,max(fst.df$Fst)), breaks=c(0,0.05,0.10,0.15))+scale_x_discrete(expand=c(0,0))+scale_y_discrete(expand=c(0,0), position="right")+theme(axis.text=element_text(colour="black", size=10, face="bold"), axis.title=element_blank(), panel.grid=element_blank(), panel.background=element_blank(), legend.position="right", legend.title=element_text(size=14, face="bold"), legend.text=element_text(size=10))



install.packages("phytools", dep=T)
library(learnPopGen)
install.packages(learnPopGen)
coalescent.plot()


#EXTRA CREDIT#

library(coala)
model<- coal_model(sample_size=3, loci_number=1)
model<- model+feat_mutation(rate-1,model="IFS")
model<-coal_model(sample_size=c(5,2,0), loci_number=1)+feat_migration(rate=0.5, symmetric=TRUE)+feat_pop_merge(0.5,3,2)+feat_pop_merge(0.8,2,1)
model<- coal_model(3,1)+feat_mutation(rate=1)+sumstat_seg_sites()
sumstats<-simulate(model,seed=123)
names(sumstats)
sumstats$seg_sites[[1]]
model<- model+locus_single(500)
sumstats<-simulate(model)
sumstats$seg_sites[[1]]
sumstats$seg_sites[[2]]
model<- model+locus_averaged(2,750)
summstats<-simulate(model)
length(sumstats$seg_sites)
model<-coal_model(5,1)+feat_mutation(rate=par_named("theta"))+sumstat_seg_sites()
sumstats1<- simulate(model,pars=c(theta=2.5))
sumstats2<-simulate(model,pars=c(theta=4.3))
model<-coal_model(5,1)+feat_mutation(rate=par_prior("theta", runif(1,0,10)))+sumstat_seg_sites()
sumstats<-simulate(model)
sumstats$pars
sumstats2<-simulate(model)
sumstats2$pars
model<-coal_model(5,1)+feat_mutation(rate=par_range("theta",0.1,5))+sumstat_seg_sites()
model<-coal_model(4,2)+feat_mutation(rate=par_named("theta"))+feat_recombination(rate=par_expr(theta*2))
pdf('005_extracredit.pdf')
plot(sumstats1,sumstats2,xlab="sumstats1",ylab="sumstas2")
dev.off()
