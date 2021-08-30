########################################
#####     Demography data 2018     #####
########################################

#### Loading packages ####

library(tidyverse)
library(GGally)
library(IPMpack)

contourPlot <- function(M,meshpts,maxSize,upper,lower) {
  q <- sum(meshpts<=maxSize);
  filled.contour(meshpts[1:q],meshpts[1:q],M[1:q,1:q], zlim=c(upper,lower),
                 xlab="size at time t", ylab="size at time t+1", color=heat.colors, nlevels=20, cex.lab=1.5);
  return(0);
}

#### Loading data #### 

#Ver_alp <- read.csv2("Data/Ver_alp_2018.csv")
Treatments <- read.csv2("Data/Treatment_dictionary.csv", stringsAsFactors = FALSE)
Sib_pro <- read.csv2("Data/Sib_pro_2018-2021.csv")


#### Cleaning data ####

Treatments <- Treatments %>%
  dplyr::select(plotID, Site, Block, Plot, OTC, Treatment)

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

Sib_pro <- Sib_pro %>% 
left_join(Treatments, by =c("plotID" = "plotID", "Site" = "Site", "Block" = "Block", "Plot" = "Plot")) %>% 
  mutate(full_Treat = paste0(OTC, Treatment))

# Sib_pro1 <- Sib_pro %>% 
#   group_by(full_Treat) %>%
#   summarize(n())
  #summarize(mean_SH = mean(SH, na.rm = TRUE),
   #         min_SH = min(SH, na.rm = TRUE),
    #        max_SH = max(SH, na.rm = TRUE))


Sib_pro_2018 <- Sib_pro %>% 
  filter(Year == 2018,
         Site == "Lav",
         Block %in% c(1,2,3)) %>% 
  dplyr::select(plotID, Uni_IDS, OTC, Treatment, full_Treat, Year, LSL, NL, LL, NFL, NB, NC, NAC)

Sib_pro_2019 <- Sib_pro %>% 
  filter(Year == 2019,
         Site == "Lav",
         Block %in% c(1,2,3)) %>% 
  dplyr::select(plotID, Uni_IDS, Year, LSL, NL, LL, NFL, NB, NC, NAC)


Sib_pro_2018_2019 <- Sib_pro_2018 %>% 
  full_join(Sib_pro_2019, by = c("Uni_IDS", "plotID"), suffix = c("_2018", "_2019")) %>% 
  #dplyr::select(-Year.x, -Year.y) %>% 
  #rename("LSL_2018" = "LSL.x", "NL_2018" = "NL.x","LL_2018" = "LL.x", "NFL_2018" = "NFL.x", "NB_2018" = "NB.x", "NC_2018" = "NC.x", "NAC_2018" = "NAC.x", "LSL_2019" = "LSL.y", "NL_2019" = "NL.y", "LL_2019" =  "LL.y", "NFL_2019" = "NFL.y","NB_2019" = "NB.y", "NC_2019" = "NC.y", "NAC_2019" = "NAC.y") %>% 
  mutate(size = 2.625811097 + LSL_2018 * 0.005558019 + NL_2018 * 0.069472337 + LL_2018 * 0.066783627, #Mock numbers from Seedclim data and another species
         sizeNext = 2.625811097 + LSL_2019 * 0.005558019 + NL_2019 * 0.069472337 + LL_2019 * 0.066783627, #Mock numbers from Seedclim data and another species
         fec = (4.38 * NFL_2018) + (4.38 * NB_2018) + (4.38 * NC_2018), #Average seeds per flower at Skjellingahaugen was 4.38
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA)),
         flo.no = NB_2018 + NFL_2018 + NC_2018,
         flo.if = ifelse(flo.no > 0, 1, 0),
         offspringNext = NA)
  

#### vital rate regressions ####
x11()
survModelComp(Sib_pro_2018_2019,makePlot = T)
survChosenModel <- surv ~ size + size2 #Chosen based on AIC
   
growthModelComp(Sib_pro_2018_2019,makePlot=T)
growthChosenModel <- sizeNext ~ size + size2 #The AIC value

# flowering
AIC(glm(flo.if~1,family='binomial',data=Sib_pro_2018_2019))
AIC(glm(flo.if~size,family='binomial',data=Sib_pro_2018_2019))
AIC(glm(flo.if~size+I(size^2),family='binomial',data=Sib_pro_2018_2019))
AIC(glm(flo.if~size+I(size^2)+I(size^3),family='binomial',data=Sib_pro_2018_2019))

floweringChosenModel <- flo.if ~ size

# # of flowers
AIC(glm(flo.no~1,family='poisson',data = Sib_pro_2018_2019))
AIC(glm(flo.no~size,family='poisson',data = Sib_pro_2018_2019))
AIC(glm(flo.no~size+I(size^2),family='poisson',data = Sib_pro_2018_2019))
AIC(glm(flo.no~size+I(size^2)+I(size^3),family='poisson',data = Sib_pro_2018_2019))

flowersChosenModel <- flo.no ~ size + size2 

# cloning
# AIC(glm(cloning ~ 1, family='binomial', data = Sib_pro_2018_2019))
# AIC(glm(cloning ~ size, family='binomial', data = Sib_pro_2018_2019))
# AIC(glm(cloning ~ size + I(size^2),family='binomial', data = Sib_pro_2018_2019))
# AIC(glm(cloning ~ size + I(size^2) + I(size^3), family='binomial',data = Sib_pro_2018_2019))
# 
# cloningChosenModel <- cloning~1


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
