#############################################
## first we go through some of the basic analyses of an IPM 
###############################################

#### lambda ####
(lambda <- as.numeric(eigen(IPM)$value[1]))

#### stable size distribution and reproductive value ####
w.eigen <- Re(eigen(IPM)$vectors[,1])
stable.dist <- w.eigen/sum(w.eigen)
v.eigen <- Re(eigen(t(IPM))$vectors[,1])
repro.val <- v.eigen/v.eigen[1]

#### matrix sensitivity & elasticity ####
sensitivity <- sens(IPM)
elasticity <- elas(IPM)

library(fields)
x11()
par(mfrow=c(2,3),mar=c(4,5,2,2))
image.plot(Pmatrix@meshpoints,Pmatrix@meshpoints,t(IPM), xlab="Size (t)",ylab="Size (t+1)", col=topo.colors(100), main="IPM matrix")
contour(Pmatrix@meshpoints,Pmatrix@meshpoints,t(IPM), add = TRUE, drawlabels = TRUE)
plot(Pmatrix@meshpoints,stable.dist,xlab="Size",type="l",main="Stable size distribution")
plot(Pmatrix@meshpoints,repro.val,xlab="Size",type="l",main="Reproductive values")
image.plot(Pmatrix@meshpoints,Pmatrix@meshpoints,t(elasticity),xlab="Size (t)",ylab="Size (t+1)",main="Elasticity")
image.plot(Pmatrix@meshpoints,Pmatrix@meshpoints,t(sensitivity),xlab="Size (t)",ylab="Size (t+1)", main="Sensitivity")


#### parameter sensitivity & elasticity ####
res <- sensParams(go, so, fo, co, nBigMatrix = bin, minSize=minSize,maxSize=maxSize,correction="constant")

x11()
par(mfrow = c(2, 1), bty = "l", pty = "m")
barplot(res$sens, main = expression("Parameter sensitivity of "*lambda),las = 2, cex.names = 0.5)
barplot(res$elas, main = expression("Parameter elasticity of "*lambda),las = 2, cex.names = 0.5)





############################################################
## now we do som more advanced analysis

## (1) we will build a model for the control and one for the treatment
##     and do life table response experiment (LTRE) analysis

## (2) we will build separate models for the annual transitions in the control
##     and calculate stochastic population growth rates and make stochastic projections
##     of population size

###############################################################
load("Data/VB.RData")


#### comparing treatment and control - life table response experiment (LTRE) ####
# running the IPM-function for treatment (2, 'RTC') & control (1, 'TTC')
VB1 <- subset(VB,VB$Treat=='TTC')
VB2 <- subset(VB,VB$Treat=='RTC')

IPMx <- makeVBIPM(VB1,VB.seeds.cap=VB.seeds.cap,VB.estab=VB.estab,bin=bin,minSize=minSize,maxSize=maxSize)
IPM1 <- IPMx$A
Pmatrix1 <- IPMx$Pmatrix
Fmatrix1 <- IPMx$Fmatrix
Cmatrix1 <- IPMx$Cmatrix
so1 <- IPMx$survObj
go1 <- IPMx$growObj

IPMx <- makeVBIPM(VB2,VB.seeds.cap=VB.seeds.cap,VB.estab=VB.estab,bin=bin,minSize=minSize,maxSize=maxSize)
IPM2 <- IPMx$A
Pmatrix2 <- IPMx$Pmatrix
Fmatrix2 <- IPMx$Fmatrix
Cmatrix2 <- IPMx$Cmatrix
so2 <- IPMx$survObj
go2 <- IPMx$growObj

as.numeric(eigen(IPM1)$value[1])
as.numeric(eigen(IPM2)$value[1])

## matrix element LTRE
D.treat <- (IPM2-IPM1)	# Difference between treat and control
B <- (IPM2+IPM1)/2		# Mean of treat and control matrices
C.treat <- D.treat*sens(B)	# Contribution to difference in lambda
image( Pmatrix@meshpoints,Pmatrix@meshpoints,
       t(C.treat),
       main = "LTRE Contribution",
       xlab = "Size at t",
       ylab = "Size at t+1" )


## vital rate LTRE
BaseIPM <- (IPM1+IPM2)/2
SBaseIPM <- sens(BaseIPM)

Difference.fec <- Fmatrix2-Fmatrix1
Contributions.fec = Difference.fec*SBaseIPM
sum(Contributions.fec)

Difference.clone <- Cmatrix2-Cmatrix1
Contributions.clone = Difference.clone*SBaseIPM
sum(Contributions.clone)

Difference.pm <- Pmatrix2-Pmatrix1
Contributions.pm = Difference.pm*SBaseIPM
sum(Contributions.pm)

sum(Contributions.fec)+sum(Contributions.clone)+sum(Contributions.pm)

as.numeric(eigen(IPM2)$value[1])-as.numeric(eigen(IPM1)$value[1]) 


so1.growth.only <- coerceSurvObj(so1, c(100,0))
Pmatrix1.growth.only <- makeIPMPmatrix(nBigMatrix = bin,
                                       minSize = minSize, maxSize = maxSize,
                                       growObj = go1, survObj =  so1.growth.only, correction = "constant")

so2.growth.only <- coerceSurvObj(so2, c(100,0))
Pmatrix2.growth.only <- makeIPMPmatrix(nBigMatrix = bin,
                                       minSize = minSize, maxSize = maxSize,
                                       growObj = go2, survObj =  so2.growth.only, correction = "constant")

Difference.growth <- Pmatrix2.growth.only-Pmatrix1.growth.only
Contributions.growth = Difference.growth*SBaseIPM
sum(Contributions.growth)

# contributions survival     
Contributions.surv <- Contributions.pm-Contributions.growth       # or sum(Contributions.pm)-sum(Contributions.growth)


sum(Contributions.surv)
sum(Contributions.growth)
sum(Contributions.clone)
sum(Contributions.fec)

contr <- data.frame(val=c(sum(Contributions.surv),
                          sum(Contributions.growth),
                          sum(Contributions.clone),
                          sum(Contributions.fec)),
                    vr=c('survival','growth','clonality','fecundity')
)


x11()
barplot(contr$val~contr$vr,xlab='Vital rate',ylab=expression(paste("Contribution to ",Delta,lambda," (treat-control)")))
abline(h=0,lty=2)



###################################################################
#### stochastic population growth ####
###################################################################
## creating a list of IPMs with one IPM per transition for the control (VB1)
IPMlist <- list()

for (i in unique(VB1$trans) ) {
  IPMx <- makeVBIPM(subset(VB1,VB1$trans==i,),VB.seeds.cap=VB.seeds.cap,VB.estab=VB.estab,bin=bin,minSize=minSize,maxSize=maxSize)
  IPMlist[[i]] <- IPMx$A
}

str(IPMlist)

## the lambdas of the single transitions
for (i in unique(VB1$trans) ) {
  print(c(i,as.numeric(eigen(IPMlist[[i]])$value[1])))
}

## the stochastic growth rate
exp(stochGrowthRateSampleList(IPMlist,nRunIn=10000,tMax=50000))


## projecting population size into the future based on stochastic transitions

# initiate population with 10 individuals at the stable pop structure for the first environment
nt0 <-abs(Re(eigen(IPMlist[[1]])$vector[,1])*10)       # 271
# create vectors fos storage
rt <- rtGood <- rtBad <- matrix(NA,20,100)
#run 20 simulations
for (j in 1:20) {
  ntGood <- ntBad <- nt <- nt0
  #loop over 100 years
  for (k in 1:100) {
    #pick a year type at random and iterate the pop
    yrtype <- sample(1:7,size=1)
    nt <- IPMlist[[yrtype]]%*%nt
    rt[j,k] <- sum(nt)
    #store what happens in only good and only bad environments
    ntGood <- IPMlist[[3]]%*%ntGood
    rtGood[j,k] <- sum(ntGood)
    #    rtGood <- rtGood
    ntBad <- IPMlist[[2]]%*%ntBad
    rtBad[j,k] <- sum(ntBad)
    #    rtBad <- rtBad
  }}



par(mfrow=c(1,1),bty="l")
matplot(t(rt),type="l",xlab="time (yrs)", ylab="total population size",
        log="y", ylim=range(c(rtGood,rtBad)), lty=1)
# you can check to see that all the rtGoods and all the rtBads
# are the same at every time-step
# so we are just plotting the first simulation
points(rtGood[1,], type="l",lwd=2,lty=2)
points(rtBad[1,], type="l",lwd=2,lty=2)
title("Stochastic population size in controls")
text(x=55, y=median(rtBad), labels="only bad years")
text(x=55, y=median(rtGood), labels="only good years")
