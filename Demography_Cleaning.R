########################################
#####     Demography data 2018     #####
########################################

#### Loading packages ####

library(tidyverse)
#library(GGally)
library(IPMpack)

contourPlot <- function(M,meshpts,maxSize,upper,lower) {
  q <- sum(meshpts<=maxSize);
  filled.contour(meshpts[1:q],meshpts[1:q],M[1:q,1:q], zlim=c(upper,lower),
                 xlab="size at time t", ylab="size at time t+1", color=heat.colors, nlevels=20, cex.lab=1.5);
  return(0);
}

### some extra functions ###
#Visualizing the IPM kernel - this function is not available in IPMpack, you'll have to run it here
contourPlot <- function(M,meshpts,maxSize,upper,lower) {
  q <- sum(meshpts<=maxSize);
  filled.contour(meshpts[1:q],meshpts[1:q],M[1:q,1:q], zlim=c(upper,lower),
                 xlab="size at time t", ylab="size at time t+1", color=heat.colors, nlevels=20, cex.lab=1.5,
                 plot.axes = { axis(1); axis(2); lines(0:maxSize, 0:maxSize, lty=2)});
  return(0);
}


### function for plotting of sequential means
pointsSeq <- function (dataf, ncuts = 10, vitalRate = "survival", col="red")
{
  
  os <- order(dataf$size)
  osSize <- (dataf$size)[os]
  psz <- tapply(osSize, as.numeric(cut(osSize, ncuts)), mean, na.rm = TRUE)
  
  if (vitalRate == "survival") {
    os.surv <- (dataf$surv)[os]
    ps <- tapply(os.surv, as.numeric(cut(osSize, ncuts)), mean, na.rm = TRUE)
    points(as.numeric(psz), as.numeric(ps), pch = 19, col=col)
  }
  if (vitalRate == "cloning") {
    os.cloning <- (dataf$cloning)[os]
    ps <- tapply(os.cloning, as.numeric(cut(osSize, ncuts)), mean, na.rm = TRUE)
    points(as.numeric(psz), as.numeric(ps), pch = 19, col=col)
  }
  if (vitalRate == "flowering") {
    os.flowering <- (dataf$flowering)[os]
    ps <- tapply(os.flowering, as.numeric(cut(osSize, ncuts)), mean, na.rm = TRUE)
    points(as.numeric(psz), as.numeric(ps), pch = 19, col=col)
  }
  if (vitalRate == "growth") {
    os.sizeNext <- (dataf$sizeNext)[os]
    ps <- tapply(os.sizeNext, as.numeric(cut(osSize, ncuts)), mean, na.rm = TRUE)
    points(as.numeric(psz), as.numeric(ps), pch = 19, col=col)
  }
  if (vitalRate == "clones") {
    os.clonesNext <- (dataf$clonesNext)[os]
    ps <- tapply(os.clonesNext, as.numeric(cut(osSize, ncuts)), mean, na.rm = TRUE)
    points(as.numeric(psz), as.numeric(ps), pch = 19, col=col)
  }
  if (vitalRate == "clonesize") {
    os.sizeNext <- (dataf$sizeNext)[os]
    ps <- tapply(os.sizeNext, as.numeric(cut(osSize, ncuts)), mean, na.rm = TRUE)
    points(as.numeric(psz), as.numeric(ps), pch = 19, col=col)
  }
  if (vitalRate == "flowers") {
    os.flo.no <- (dataf$flowers)[os]
    ps <- tapply(os.flo.no, as.numeric(cut(osSize, ncuts)), mean, na.rm = TRUE)
    points(as.numeric(psz), as.numeric(ps), pch = 19, col=col)
  }
}



#### Loading data #### 

#Ver_alp <- read.csv2("Data/Ver_alp_2018.csv")
Treatments <- read.csv2("Data/Treatment_dictionary.csv", stringsAsFactors = FALSE)
Sib_pro <- read.csv2("Data/Sib_pro_2018-2021.csv")


#### Cleaning data ####

Treatments <- Treatments %>%
  dplyr::select(plotID, OTC, Treatment)

 Seedling_info <- Sib_pro %>% 
  filter(seedl == "yes") %>% 
  mutate(size = 2.625811097 + LSL * 0.005558019 + NL * 0.069472337 + LL * 0.066783627) %>% #Mock numbers from Seedclim data and another species
  mutate(seeds_cap = mean(size),
         seeds_cap_sd = sd(size),
         seedling_establishment_rate = 0.6) %>% # 60% chance of germinating in the lab with seeds from sibbaldia at Lavsidalen
  dplyr::select(seeds_cap, seeds_cap_sd, seedling_establishment_rate) %>% 
  distinct()


Sib_pro <- Sib_pro %>% 
  mutate(plotID = paste0(Site, "_", Block, "_", Plot)) %>%
  mutate(Uni_IDS = paste0(plotID, "_", IDS)) %>% 
  dplyr::select(-starts_with("X."))
#Some Site is called "Ulv ", needs to be changed to "Ulv".

Sib_pro <- Sib_pro %>% 
  left_join(Treatments, by =c("plotID" = "plotID")) %>% 
  mutate(full_Treat = paste0(OTC, Treatment))

# Sib_pro1 <- Sib_pro %>% 
#   group_by(full_Treat) %>%
#   summarize(n())
  #summarize(mean_SH = mean(SH, na.rm = TRUE),
   #         min_SH = min(SH, na.rm = TRUE),
    #        max_SH = max(SH, na.rm = TRUE))


Sib_pro_2018 <- Sib_pro %>% 
  filter(Year == 2018,
         Site == "Lav") %>% 
  dplyr::select(plotID, Uni_IDS, OTC, Treatment, full_Treat, Year, LSL, NL, LL, NFL, NB, NC, NAC, seedl, juvenile)

Sib_pro_2019 <- Sib_pro %>% 
  filter(Year == 2019,
         Site == "Lav") %>% 
  dplyr::select(plotID, Uni_IDS, OTC, Treatment, full_Treat, Year, LSL, NL, LL, NFL, NB, NC, NAC, seedl, juvenile)


Sib_pro_2018_2019 <- Sib_pro_2018 %>% 
  full_join(Sib_pro_2019, by = c("Uni_IDS", "plotID", "OTC", "Treatment", "full_Treat"), suffix = c("_2018", "_2019")) %>% 
  #dplyr::select(-Year.x, -Year.y) %>% 
  #rename("LSL_2018" = "LSL.x", "NL_2018" = "NL.x","LL_2018" = "LL.x", "NFL_2018" = "NFL.x", "NB_2018" = "NB.x", "NC_2018" = "NC.x", "NAC_2018" = "NAC.x", "LSL_2019" = "LSL.y", "NL_2019" = "NL.y", "LL_2019" =  "LL.y", "NFL_2019" = "NFL.y","NB_2019" = "NB.y", "NC_2019" = "NC.y", "NAC_2019" = "NAC.y") %>% 
  mutate(size = 2.625811097 + LSL_2018 * 0.005558019 + NL_2018 * 0.069472337 + LL_2018 * 0.066783627, #Mock numbers from Seedclim data and another species
         sizeNext = 2.625811097 + LSL_2019 * 0.005558019 + NL_2019 * 0.069472337 + LL_2019 * 0.066783627, #Mock numbers from Seedclim data and another species
         fec = (4.38 * NFL_2018) + (4.38 * NB_2018) + (4.38 * NC_2018), #Average seeds per flower at Skjellingahaugen was 4.38
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA)),
         flo.no = NB_2018 + NFL_2018 + NC_2018,
         flo.if = ifelse(flo.no > 0, 1, 0)) %>%
  mutate(offspringNext = ifelse(seedl_2019 == "yes", "sexual",
                                ifelse(juvenile_2019 == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
  
  ## Make clonal information (clo.if, clo.no and transfer the size of the mother to size)
  dplyr::select(Uni_IDS, OTC, Treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedl_2019, juvenile_2019) 
  

#### vital rate regressions ####

params=data.frame(
  surv.int=NA,
  surv.slope=NA,
  surv.2=NA,
  growth.int=NA,
  growth.slope=NA,
  growth.sd=NA,
  growth.2=NA,
  flower.if.int=NA,
  flower.if.slope=NA,
  flower.no.int=NA,
  recruit.size.mean=NA,
  recruit.size.sd=NA,
  establishment.prob=NA,
  seedsperflower=4.38
)

x11() 
survModelComp(Sib_pro_2018_2019,makePlot = T)
#survChosenModel <- surv ~ size + size2 #Chosen based on AIC

# 1. survival regression
surv.reg=glm(surv~size + I(size^2),data=Sib_pro_2018_2019,family=binomial())
summary(surv.reg)
params$surv.int=coefficients(surv.reg)[1]
params$surv.slope=coefficients(surv.reg)[2]
params$surv.2=coefficients(surv.reg)[3]

x11()   
growthModelComp(Sib_pro_2018_2019,makePlot=T)
#growthChosenModel <- sizeNext ~ size + size2 #The AIC value

growth.reg=lm(sizeNext~size + I(size^2),data=Sib_pro_2018_2019)
summary(growth.reg)
params$growth.int=coefficients(growth.reg)[1]
params$growth.slope=coefficients(growth.reg)[2]
params$growth.sd=sd(resid(growth.reg))
params$growth.2=coefficients(growth.reg)[3]

# flowering
plot(x = Sib_pro_2018_2019$size, y = jitter(Sib_pro_2018_2019$flo.if))

AIC(glm(flo.if~1,family='binomial',data=Sib_pro_2018_2019))
AIC(glm(flo.if~size,family='binomial',data=Sib_pro_2018_2019))
AIC(glm(flo.if~size+I(size^2),family='binomial',data=Sib_pro_2018_2019))
AIC(glm(flo.if~size+I(size^2)+I(size^3),family='binomial',data=Sib_pro_2018_2019))

#floweringChosenModel <- flo.if ~ size +I(size^2)+I(size^3)

flowering.if.reg=glm(flo.if~size,family='binomial', data=Sib_pro_2018_2019)
summary(flowering.if.reg)
params$flower.if.int=coefficients(flowering.if.reg)[1]
params$flower.if.slope=coefficients(flowering.if.reg)[2]

# # of flowers

Sib_pro_2018_2019_flo <- Sib_pro_2018_2019 %>% filter(flo.if == 1)

plot(x = Sib_pro_2018_2019_flo$size, y = Sib_pro_2018_2019_flo$flo.no)

AIC(glm(flo.no~1,family='poisson',data = Sib_pro_2018_2019_flo))
AIC(glm(flo.no~size,family='poisson',data = Sib_pro_2018_2019_flo))
AIC(glm(flo.no~size+I(size^2),family='poisson',data = Sib_pro_2018_2019_flo))
AIC(glm(flo.no~size+I(size^2)+I(size^3),family='poisson',data = Sib_pro_2018_2019_flo))

#flowersChosenModel <- flo.no ~ size + I(size^2)

flowering.no.reg=glm(flo.no~1,family='poisson', data=Sib_pro_2018_2019_flo)
summary(flowering.no.reg)
params$flower.no.int=coefficients(flowering.no.reg)[1]
#params$flower.no.slope=coefficients(flowering.no.reg)[2]
#params$flower.no.2=coefficients(flowering.no.reg)[3]

# seeds regression
# note that we are just pooling all individuals into this regression regardless of whether they flowered or not. a later exercise will be to explicitly model flowering probability.
seed.reg=glm(fec~size,data=Sib_pro_2018_2019,family=poisson())
summary(seed.reg)
params$seed.int=coefficients(seed.reg)[1]
params$seed.slope=coefficients(seed.reg)[2]

# size distribution of recruits

Sib_pro_2018_2019_seedlings <- Sib_pro_2018_2019 %>% 
  filter(seedl_2019 == "yes" | juvenile_2019 == "yes",
         is.na(size))

params$recruit.size.mean = mean(Sib_pro_2018_2019_seedlings$sizeNext)
params$recruit.size.sd=sd(Sib_pro_2018_2019_seedlings$sizeNext)

params$establishment.prob=sum(is.na(Sib_pro_2018_2019$size))/sum(Sib_pro_2018_2019$fec,na.rm=TRUE)

# cloning

Sib_pro_2018_2019_clones <- Sib_pro_2018_2019 %>%
  mutate(offspringNext = ifelse(seedl_2019 == "yes", "sexual",
                                ifelse(juvenile_2019 == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA))))
  

 #AIC(glm(cloning ~ 1, family='binomial', data = Sib_pro_2018_2019))
 #AIC(glm(cloning ~ size, family='binomial', data = Sib_pro_2018_2019))
 #AIC(glm(cloning ~ size + I(size^2),family='binomial', data = Sib_pro_2018_2019))
 #AIC(glm(cloning ~ size + I(size^2) + I(size^3), family='binomial',data = Sib_pro_2018_2019))
 
 #cloningChosenModel <- cloning~1





# 6. plot the models over the data - figure 2
par(mfrow=c(2,2),mar=c(4,4,2,1))
xx=seq(0,8,by=.01)
plot(Sib_pro_2018_2019$size,jitter(Sib_pro_2018_2019$surv),main='Survival') # jittered to see easier
lines(xx,predict(surv.reg,data.frame(size=xx),type='response'), col='red',lwd=3)
plot(Sib_pro_2018_2019$size,Sib_pro_2018_2019$sizeNext,main='Growth/Shrinkage/Stasis')	
lines(xx,predict(growth.reg,data.frame(size=xx)),col='red',lwd=3)
plot(Sib_pro_2018_2019$size,jitter(Sib_pro_2018_2019$flo.if),main='Flowering')	
lines(xx,predict(flowering.if.reg,data.frame(size=xx), type = "response"),col='red',lwd=3)

plot(Sib_pro_2018_2019_flo$size,Sib_pro_2018_2019_flo$flo.no,main='Flower number') # jittered to see easier
lines(xx,predict(flowering.no.reg,data.frame(size=xx),type='response'), col='red',lwd=3)
hist(Sib_pro_2018_2019_seedlings$sizeNext,main='Size of Recruits',freq=FALSE)
lines(xx,dnorm(xx,params$recruit.size.mean,params$recruit.size.sd), col='red',lwd=3)

## vital rate functions

# 1. probability of surviving
s.x=function(x,params) {
  u=exp(params$surv.int+params$surv.slope*x+params$surv.2*x^2)
  return(u/(1+u))
}

# 2. growth function
g.yx=function(xp,x,params) { 			
  dnorm(xp,mean=params$growth.int + params$growth.slope*x + params$growth.2*x^2,sd=params$growth.sd)
}

# 3. reproduction function      
f.yx=function(xp,x,params) { 		
  exp(params$flower.if.int + params$flower.if.slope*x)/(1+exp(params$flower.if.int + params$flower.if.slope*x))*
        exp(params$flower.no.int)*
    params$seedsperflower*
  params$establishment.prob*
    dnorm(xp,mean=params$recruit.size.mean,sd=params$recruit.size.sd)
    
}

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# D. make a kernel
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# 1. boundary points b, mesh points y and step size h
# integration limits - these limits span the range of sizes observed in the data set, and then some.
min.size=.9*min(c(Sib_pro_2018_2019$size,Sib_pro_2018_2019$sizeNext),na.rm=T)
max.size=1.1*max(c(Sib_pro_2018_2019$size,Sib_pro_2018_2019$sizeNext),na.rm=T)
# number of cells in the discretized kernel
n=100 
# #boundary points (the edges of the cells defining the kernel)
b=min.size+c(0:n)*(max.size-min.size)/n 
# mesh points (midpoints of the cells)
y=0.5*(b[1:n]+b[2:(n+1)])
# width of the cells
h=y[2]-y[1] 

# 2. make component kernels
# the function outer() evaluates the kernal at all pairwise combinations of the two vectors y and y. for the numerical integration, we're estimating the area of a rectangle under the curve. the hieghts of the rectangles are given by the outer function and the width of the rectangles is h. 
G=h*outer(y,y,g.yx,params=params) # growth kernel
S=s.x(y,params=params) # survival 
P=G # placeholder; we're about to redefine P on the next line
for(i in 1:n) P[,i]=G[,i]*S[i]  # growth/survival kernel
F=h*outer(y,y,f.yx,params=params) # reproduction kernel
K=P+F #full kernel

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# E. basic analyses
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# 1. get lamda,v,w  
(lam=Re(eigen(K)$values[1]))
w.eigen=Re(eigen(K)$vectors[,1])
stable.dist=w.eigen/sum(w.eigen) 
v.eigen=Re(eigen(t(K))$vectors[,1])
repro.val=v.eigen/v.eigen[1] 

# 2. compute elasticity and sensitivity matrices
v.dot.w=sum(stable.dist*repro.val)*h
sens=outer(repro.val,stable.dist)/v.dot.w
elas=matrix(as.vector(sens)*as.vector(K)/lam,nrow=n)

# 3. plot results
par(mfrow=c(2,3)) 
image(y,y,t(K), xlab="Size (t)",ylab="Size (t+1)",col=topo.colors(100), main="Kernel IPM")
abline(0,1,lty=2)
image(y,y,t(P), xlab="Size (t)",ylab="Size (t+1)",col=topo.colors(100), main="Kernel P")
abline(0,1,lty=2)
image(y,y,t(F), xlab="Size (t)",ylab="Size (t+1)",col=topo.colors(100), main="Kernel F")
abline(0,1,lty=2)



contour(y,y,t(K), add = TRUE, drawlabels = TRUE)
plot(y,stable.dist,xlab="Size",type="l",main="Stable size distribution")
plot(y,repro.val,xlab="Size",type="l",main="Reproductive values") 
image(y,y,t(elas),xlab="Size (t)",ylab="Size (t+1)",main="Elasticity")
image(y,y,t(sens),xlab="Size (t)",ylab="Size (t+1)", main="Sensitivity")


#### Testing warming effect on population dynamics ####

Control <- Sib_pro_2018_2019 %>% 
  filter(OTC == "C")

#### vital rate objects ####
so_c <- makeSurvObj(Cold, Formula = survChosenModel)
go_c <- makeGrowthObj(Cold, Formula = growthChosenModel)

fo_c <- makeFecObj(Cold, Formula=c(floweringChosenModel, flowersChosenModel),
                 Family=c("binomial", "poisson"),
                 Transform=c("none", "none"),
                 meanOffspringSize = Seedling_info$seeds_cap,
                 sdOffspringSize = Seedling_info$seeds_cap_sd,
                 fecConstants = data.frame(seedsPerCap=4.38, seedlingEstablishmentRate=0.1))

#### matrices ####
bin = 100
minSize_c = min(Cold$size,na.rm=T)*0.9
maxSize_c = max(Cold$size,na.rm=T)*1.1
Pmatrix_c <- makeIPMPmatrix(nBigMatrix = bin, minSize = minSize_c, maxSize = maxSize_c, growObj = go_c, survObj = so_c, correction = "constant")

x11()
diagnosticsPmatrix(Pmatrix, growObj=go_c, survObj=so_c, dff=Cold, correction="constant")

contourPlot(t(Pmatrix_c), Pmatrix_c@meshpoints,maxSize_c, 0.03, 0)

#Cmatrix <- makeIPMCmatrix(clonalObj=co,nBigMatrix = bin,minSize=minSize,maxSize=maxSize,correction="constant")
Fmatrix_c <- makeIPMFmatrix(fecObj=fo,nBigMatrix = bin,minSize=minSize_c, maxSize=maxSize_c, correction="constant")

#contourPlot(t(Cmatrix), Cmatrix@meshpoints,maxSize, 0.03, 0)
contourPlot(t(Fmatrix_c), Fmatrix_c@meshpoints, maxSize_c, 0.003, 0)

IPM_c <- Pmatrix_c + Fmatrix_c #+ Cmatrix

contourPlot(t(IPM_c), Pmatrix_c@meshpoints, maxSize_c, 0.05, 0)
as.numeric(eigen(IPM_c)$value[1])


#### vital rate objects ####
so_w <- makeSurvObj(Warm, Formula = survChosenModel)
go_w <- makeGrowthObj(Warm, Formula = growthChosenModel)

fo_w <- makeFecObj(Warm, Formula=c(floweringChosenModel, flowersChosenModel),
                   Family=c("binomial", "poisson"),
                   Transform=c("none", "none"),
                   meanOffspringSize = Seedling_info$seeds_cap,
                   sdOffspringSize = Seedling_info$seeds_cap_sd,
                   fecConstants = data.frame(seedsPerCap=4.38, seedlingEstablishmentRate=0.1))

#### matrices ####
bin = 100
minSize_w = min(Warm$size,na.rm=T)*0.9
maxSize_w = max(Warm$size,na.rm=T)*1.1
Pmatrix_w <- makeIPMPmatrix(nBigMatrix = bin, minSize = minSize_w, maxSize = maxSize_w, growObj = go_w, survObj = so_w, correction = "constant")

x11()
diagnosticsPmatrix(Pmatrix, growObj=go_w, survObj=so_w, dff=Warm, correction="constant")

contourPlot(t(Pmatrix_w), Pmatrix_w@meshpoints,maxSize_w, 0.03, 0)

#Cmatrix <- makeIPMCmatrix(clonalObj=co,nBigMatrix = bin,minSize=minSize,maxSize=maxSize,correction="constant")
Fmatrix_w <- makeIPMFmatrix(fecObj=fo,nBigMatrix = bin,minSize=minSize_w, maxSize=maxSize_w, correction="constant")

#contourPlot(t(Cmatrix), Cmatrix@meshpoints,maxSize, 0.03, 0)
contourPlot(t(Fmatrix_w), Fmatrix_w@meshpoints, maxSize_w, 0.003, 0)

IPM_w <- Pmatrix_w + Fmatrix_w #+ Cmatrix

contourPlot(t(IPM_w), Pmatrix_w@meshpoints, maxSize_w, 0.05, 0)
as.numeric(eigen(IPM_w)$value[1])

#############################################
## Analysis of IPM 
###############################################

#### lambda ####
lambda_w <- as.numeric(eigen(IPM_w)$value[1])

#### stable size distribution and reproductive value ####
w.eigen_w <- Re(eigen(IPM_w)$vectors[,1])
stable.dist_w <- w.eigen_w/sum(w.eigen_w)
v.eigen_w <- Re(eigen(t(IPM_w))$vectors[,1])
repro.val_w <- v.eigen_w/v.eigen_w[1]

#### matrix sensitivity & elasticity ####
sensitivity <- sens(IPM_w)
elasticity <- elas(IPM_w)

x11()
par(mfrow=c(2,3),mar=c(4,5,2,2))
image.plot(Pmatrix_w@meshpoints,Pmatrix_w@meshpoints,t(IPM_w), xlab="Size (t)",ylab="Size (t+1)", col=topo.colors(100), main="IPM matrix")
contour(Pmatrix_w@meshpoints_w@meshpoints,t(IPM_w), add = TRUE, drawlabels = TRUE)
plot(Pmatrix_w@meshpoints,stable.dist,xlab="Size",type="l",main="Stable size distribution")
plot(Pmatrix_w@meshpoints,repro.val,xlab="Size",type="l",main="Reproductive values")
image.plot(Pmatrix_w@meshpoints,Pmatrix_w@meshpoints,t(elasticity),xlab="Size (t)",ylab="Size (t+1)",main="Elasticity")
image.plot(Pmatrix_w@meshpoints,Pmatrix_w@meshpoints,t(sensitivity),xlab="Size (t)",ylab="Size (t+1)", main="Sensitivity")

#### parameter sensitivity & elasticity ####
res <- sensParams(go, so, fo, nBigMatrix = bin, minSize=minSize,maxSize=maxSize,correction="constant") #removed clonality, just add co if you want to add that

x11()
par(mfrow = c(2, 1), bty = "l", pty = "m")
barplot(res$sens, main = expression("Parameter sensitivity of "*lambda),las = 2, cex.names = 0.5)
barplot(res$elas, main = expression("Parameter elasticity of "*lambda),las = 2, cex.names = 0.5)


#### vital rate objects ####
so <- makeSurvObj(Sib_pro_2018_2019, Formula = survChosenModel)
go <- makeGrowthObj(Sib_pro_2018_2019, Formula = growthChosenModel)

fo <- makeFecObj(Sib_pro_2018_2019, Formula=c(floweringChosenModel, flowersChosenModel),
                 Family=c("binomial", "poisson"),
                 Transform=c("none", "none"),
                 meanOffspringSize = Seedling_info$seeds_cap,
                 sdOffspringSize = Seedling_info$seeds_cap_sd,
                 fecConstants = data.frame(seedsPerCap=4.38, seedlingEstablishmentRate=0.1))

#### matrices ####
bin = 100
minSize = min(Sib_pro_2018_2019$size,na.rm=T)*0.9
maxSize = max(Sib_pro_2018_2019$size,na.rm=T)*1.1
Pmatrix <- makeIPMPmatrix(nBigMatrix = bin, minSize = minSize, maxSize = maxSize, growObj = go, survObj = so, correction = "constant")

x11()
diagnosticsPmatrix(Pmatrix, growObj=go, survObj=so, dff=Sib_pro_2018_2019, correction="constant")

contourPlot(t(Pmatrix), Pmatrix@meshpoints,maxSize, 0.03, 0)

#Cmatrix <- makeIPMCmatrix(clonalObj=co,nBigMatrix = bin,minSize=minSize,maxSize=maxSize,correction="constant")
Fmatrix <- makeIPMFmatrix(fecObj=fo,nBigMatrix = bin,minSize=minSize,maxSize=maxSize,correction="constant")

#contourPlot(t(Cmatrix), Cmatrix@meshpoints,maxSize, 0.03, 0)
contourPlot(t(Fmatrix), Fmatrix@meshpoints,maxSize, 0.003, 0)

IPM <- Pmatrix + Fmatrix #+ Cmatrix

contourPlot(t(IPM), Pmatrix@meshpoints,maxSize, 0.05, 0)
as.numeric(eigen(IPM)$value[1])
