install.packages("diversitree")
library(diversitree)
transition_0to1<-0.1
transition_1to0<-0.1
speciation_0<-0.2
extinction_0<-0.1
speciation_1<-0.2
extinction_1<-0.1
maxN<-1e3
maxT<-50
Pars<-c(speciation_0,speciation_1,extinction_0,extinction_1,transition_0to1,transition_1to0)
simTree<-tree.bisse(Pars,max.taxa=maxN,max.t=maxT)
str(simTree)
stateTable<-table(simTree$tip.state)
stateTable/sum(stateTable)
set.seed(2)
phy<-tree.bd(c(1,0.3),max.taxa=100)
lik<-make.bd(phy)
argnames(lik)
phy2<-tree.bd(c(0,0.3), max.taxa=100)
lik(c(1,0.03)) # -7.74086
lik(c(1,.03), condition.surv=FALSE) # -10.74823
plot(phy,no.margin=TRUE,show.tip.label=FALSE)
fit<-find.mle(lik,c(.1,.03), method="subplex")
coef(fit)
logLik(fit)
fit$lnLik
purebirth<-constrain(lik,mu~0)
fit.purebirth<-find.mle(purebirth,coef(fit)[1],method="subplex")
anova(fit,purebirth=fit.purebirth)
set.seed(1)
samples<-mcmc(lik,fit$par,nsteps=10000,w=.1, print.every=0)
set.seed(1)
phy.sub<-drop.tip(phy,sample(100,25))
lik.sub<-make.bd(phy.sub,sampling.f=75/100)
fit.sub<-find.mle(lik.sub, c(0.1,0.03), method="subplex")
coef(fit.sub)
lik.sub.purebirth<-constrain(lik.sub, mu~0)
fit.sub.purebirth<-find.mle(lik.sub.purebirth,coef(fit.sub)[1], method="subplex")
anova(fit.sub,purebirth=fit.sub.purebirth)
samples$r<-samples$lambda-samples$mu
pdf("09_plot1.pdf")
col<-c("#eaab00", "#004165","#618e02")
profiles.plot(samples[c("lambda", "mu", "r")], col.line=col, las=1, xlab="Parameter estimate", ylab="Probability density", alpha=.75, legend.pos="topright")
abline(v=c(.1,.03,.07), col=col, lty=2)
dev.off()
# This graph displays the distribution of birth-death rate, which includes mu(extinction rate), lambda (speciation rate), and r(diversification rate). 
pars<- c(0.1,0.2,0.03,0.03,0.01,0.01)
set.seed(4)
phy<- tree.bisse(pars,max.t=30, x0=0)
states<- phy$tip.state
head(states)
lik<-make.bisse(phy,states)
lik(pars) #-159.71
p<- starting.point.bisse(phy)
p
fit<-find.mle(lik,p)
fit$lnLik
round(coef(fit),3)
par(mar=rep(0,4))
pdf("09_plot2.pdf")
col<-c("#004165", "#eaab00")
plot(history.from.sim.discrete(phy,0:1), phy, col=col)
dev.off()
# Yellow is state 1 and Blue is state 0. This combines the birth-death rates of two states. Yellow speciates at twice the rate of Blue. 