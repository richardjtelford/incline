# Script for data sheet preparation

library(xtable)
library(tidyverse)

sp <- c("sibpro","veralp")
#sp2 <- c("novel", "extant")

species <- c("Sibbaldia procumbens","Veronica alpina")
#species2 <- c("Novel", "Extant")

variables <- list(c("IDS","NL20", "LL20", "Info", "MS","IDG","X","Y","LSL","NL","LL","NFS","NFL","NB","NC","NAC","Comment"),
                  c("IDS","SH20", "NL20", "Info", "MS","IDG","X","Y","SH","NL","LL","WL","NFL","NB","NC","NAC","Comment"))

#variables2 <- list(c("IDS", "IDS_Sp18", "NL18","MS","IDG","X","Y","SH","NL","LL","WL", "NFS", "N/D F","N/D B","N/D C","N/D AC", "IL", "Comment"),
#                  c("IDS","IDS_Sp18", "NL18","MS","IDG","X","Y","SH","NL","LL","WL", "NFS", "NF","NB","NC","NAC", "IL", "Comment")) 

# variables from t0 to t1
info2020 <- list(1:9, 1:9)    						 
#info20182 <-  list(1:7, 1:7)

notation <- list(	c("IDS  Shoot ID", "Info   Info from 2020 sampling", "MS   Mother shoot","IDG  Genet ID","X     x coordinate","Y     y coordinate","LSL   Leaf stalk length of longest leav (mm)","NL    Number of leaves","LL    Length of the biggest leaf (mm)","NFS Nmber of flower stems", "NFL  Number of flowers","NB    Number of buds","NC    Number of capsules","NAC  Number of aborted capsules"),
                  c("IDS  Shoot ID", "Info   Info from 2020 sampling", "MS   Mother shoot","IDG  Genet ID","X     x coordinate","Y     y coordinate","SH   Shoot height (mm)","NL    Number of leaves","LL    Length of the biggest leaf (mm)","WL    Width of the biggest leaf (mm)","NFL  Number of flowers","NB    Number of buds","NC    Number of capsules","NAC  Number of aborted capsules"))

#notation2 <- list( c("IDS  Shoot ID","MS   Mother shoot","IDG  Genet ID","X     x coordinate","Y     y coordinate","SH   Shoot height (mm)","NL    Number of leaves","LL    Length of the biggest leaf (mm)","WL    Width of the biggest leaf (mm)", "NFS   Number of flower stems", "NF  Number/Diameter (mm) of flowers","NB    Number/Diameter of buds","NC    Number/Diameter (mm) of capsules","NAC  Number/Diameter(mm) of aborted capsules", "IL    Length of influoresence for Carex (mm)"),
#c("IDS  Shoot ID","MS   Mother shoot","IDG  Genet ID","X     x coordinate","Y     y coordinate","SH   Shoot height (mm)","NL    Number of leaves","LL    Length of the biggest leaf (mm)","WL    Width of the biggest leaf (mm)", "NFS   Number of flower stems", "NF  Number of flowers","NB    Number of buds","NC    Number of capsules","NAC  Number of aborted capsules", "IL    Length of influoresence for Carex (mm)"))


setwd("O:/PhD/R_stats/incline/Data")

for(i in 1:length(sp)){
	#print(sp[i])
  sp[i] <- "veralp"
	# Load data
	x18 <- read.delim(paste("veralp","18.txt",sep=""),dec=",", sep="\t")
	x19 <- read.delim(paste("veralp","19.txt",sep=""),dec=",", sep = "\t")		
	x20 <- read.delim(paste("veralp","20.txt",sep=""),dec=",", sep = "\t")
	
	x18 <- x18 %>% 
	  mutate(Site = as.character(Site)) %>% 
	  mutate(Site = ifelse(Site == "Ulv ", "Ulv", Site)) %>%
	  select(-starts_with("X.")) %>% 
	  mutate(Plot_IDS = paste(x18$Site, x18$Block, x18$Plot, x18$IDS, sep = "_")) %>% 
	  mutate(PlotID = paste(x18$Site, x18$Block, x18$Plot, sep = "_")) %>% 
	  filter(!PlotID == "_NA_NA")
	
		x19 <- x19 %>% 
	  mutate(Site = as.character(Site)) %>% 
	  mutate(Site = ifelse(Site == "Ulv ", "Ulv", Site)) %>%
	  select(-starts_with("X.")) %>% 
	  mutate(Plot_IDS = paste(x19$Site, x19$Block, x19$Plot, x19$IDS, sep = "_")) %>% 
	  mutate(PlotID = paste(x19$Site, x19$Block, x19$Plot, sep = "_")) %>% 
	  filter(!PlotID == "_NA_NA")
	
	x20 <- x20 %>% 
	  mutate(Site = as.character(Site)) %>% 
	  mutate(Site = ifelse(Site == "Ulv ", "Ulv", Site)) %>% 
	  select(-starts_with("X."), -comment_transcription) %>% 
	  mutate(Plot_IDS = paste(x20$Site, x20$Block, x20$Plot, x20$IDS, sep = "_")) %>% 
	  mutate(PlotID = paste(x20$Site, x20$Block, x20$Plot, sep = "_")) %>% 
	  filter(!PlotID == "_NA_NA")
	
	
	# Merge data for 2019 and 2020
    x <- merge(x19,x20, by = c("Site", "Block", "Plot", "Plot_IDS"), all=TRUE, suffix = c("19","20"))
    x <- merge(x, x18, by = c("Site", "Block", "Plot", "Plot_IDS"), all=TRUE, suffix = c("","18"))
 
	x <- x %>% 
	  rename(PlotID18 = PlotID) %>% 
	  mutate(PlotID = PlotID18) %>% 
	  mutate(PlotID = ifelse(is.na(PlotID18), PlotID19, PlotID)) %>% 
	  mutate(PlotID = ifelse(is.na(PlotID), PlotID20, PlotID)) #In case there are any NAs in PlotID name (meaning that there are some plots that had individuals in them in 2020 but not in 2019)
	

	# Make variables for the last recorded coordinates, right IDS and seedling, juvenile and dead info
	# index for dead/missing individuals
	#dead1 <- is.na(x$X20) | is.na(x$NL20) | x$NL20=0	These are the arguments for being dead
	
x <- x %>% 
  rename(X18 = X, 
         Y18 = Y, 
         IDS18 = IDS, 
         IDG18 = IDG, 
         NL18 = NL, 
         MS18 = MS,
         NAC18 = NAC,
         NB18 = NB,
         NC18 = NC,
         NFL18 = NFL,
         LL18 = LL) %>% 
  mutate(X = X20) %>%
  mutate(X = ifelse(is.na(X20), X19, X)) %>% 
  mutate(X = ifelse(is.na(X), X18, X)) %>% 
  mutate(Y = Y20) %>% 
  mutate(Y = ifelse(is.na(Y20), Y19, Y)) %>%
  mutate(Y = ifelse(is.na(Y), Y18, Y)) %>% 
  mutate(IDG20 = ifelse(IDG20 == "", NA, IDG20)) %>% 
  mutate(IDG = IDG20) %>%
  mutate(IDG = ifelse(is.na(IDG20), IDG19, IDG)) %>% 
  mutate(IDG = ifelse(is.na(IDG), IDG18, IDG)) %>%
  mutate(MS = MS20) %>% 
  mutate(MS = ifelse(is.na(MS20), MS19, MS)) %>% 
  mutate(MS = ifelse(is.na(MS), MS18, MS)) %>% 
  mutate(IDS = IDS20) %>% 
  mutate(IDS = ifelse(is.na(IDS20), IDS19, IDS)) %>% 
  mutate(IDS = ifelse(is.na(IDS), IDS18, IDS)) %>% 
  mutate(Info = ifelse(seedl20 == "yes", "S",
	                         ifelse(juvenile20 == "yes", "J",
	                                ifelse(is.na(X20), "+",
	                                       ifelse(is.na(NL20), "+",
	                                              ifelse(NL20 == 0, "+", NA))))))
	
	
	# Adjust variable names for different species
	if(sp[i]=="veralp") x <- x %>% 
  rename(SH18 = SH,
         WL18 = WL)

 if(sp[i]=="sibpro") x <- x %>% 
  rename(LSL18 = LSL,
         NFS18 = NFS)


	# Make maps for each plot
	pid <- sort(unique(x$PlotID))
	#pid <- c("Lav_2_5", "Skj_3_4", "Ulv_7_3")
	for(j in 1:length(pid))
	{
		#print(pid[j])
		y <- x %>% 
		  filter(PlotID == pid[j]) %>% 
		  filter(!is.na(Plot_IDS)) 
				
		# Make numeric IDS (replace "b" with .5) and sort
#		y$IDSnum <- as.numeric(gsub("b",".5",y$IDS))      # only if non.numeric IDS names
		y <- y[order(y$IDS),]
		
		# index for dead/missing individuals
		dead <- is.na(y$X20) | is.na(y$NL20) | y$NL20==0
		
		# set number of lines per sheet, if more than nlines individuals, print new sheets with maps of all ind
		nlines <- 40
		start <- 1; stop <- min(nlines,nrow(y))
		sheet <- 0
		while(stop >= start)
		{
			z <- y[start:stop,]
			start <- start+nlines; stop <- min(stop+nlines,nrow(y))
			sheet <- sheet+1

			# create layout for the sheet
			pdf(file=paste(species[i],pid[j],sheet,".pdf"),width=8.3,height=11.7)	
			L <- layout(matrix(c(1,2,3,3),2,2,byrow=T),heights=c(10.5, 19.0),width=c(6,15))
			
			# annotation	
			par(mar=c(0,1,0,0))
			plot(0,0,xlim=c(0,100),ylim=c(0,200),type="n",bty="n",axes=F,xlab="",ylab="")
			text(0,195,species[i],cex=1.5,pos=4)	
			text(0,180,"Date           _________________________",pos=4, cex=0.8)
			text(0,165,"Registrator _________________________",pos=4, cex=0.8)
			text(0,155,"Notation",pos=4, cex=0.8)
			for(k in 1:length(notation[[i]])) text(0,155-k*10,notation[[i]][k],pos=4, cex=0.8)

			# map
			par(mar=c(4,4,4,2))
			plot(y$X[!dead],y$Y[!dead],xlim=c(-2,37),ylim=c(-2,27),xlab="X (cm)",ylab="Y (cm)",
			     main=pid[j], mtext("o present 2020, + missing/dead 2020",cex=0.8,line=0.5))
			if(any(!dead)) text(y$X[!dead]+0.5,y$Y[!dead]+0.5,y$IDS[!dead],cex=0.8)
			if(any(dead))
			{
				if(any(!is.na(y$X)))
				{
					points(y$X[dead],y$Y[dead],pch=3,cex=0.7)
					text(y$X[dead]+0.5,y$Y[dead]+0.5,as.character(y$IDS[dead]),cex=0.8)
				}
				else
				{
					points(y$X[dead],y$Y[dead],pch=3,cex=0.7)
					text(y$X[dead]+0.5,y$Y[dead]+0.5,as.character(y$IDS[dead]),cex=0.8)
				}

			}
			abline(h=c(5,10,15,20),lty=3,col="gray")
			abline(v=c(5,10,15,20,25,30),lty=3,col="gray")
			abline(h=c(0,25),lty=2)
			abline(v=c(0,35),lty=2)
			axis(side = 1, at = c(0,5,10,15,20,25,30,35))

			# data sheet
			par(mar=c(2,1,1,1))
			plot(0,0,xlim=c(5,150),ylim=c(2,nlines+1),type="n",bty="n",axes=F,xlab="",ylab="")
			colx <- nchar(variables[[i]])+c(7,7,7,5,6,6,10,10,9,9,9,9,14,14,14,14,10)
			colx[length(colx)] <- colx[length(colx)]*2.5
			colx <- cumsum(colx)
			colx <- colx*150/max(colx)
			colx <- c(0,colx[-length(colx)])
			rowy <- (nlines:1)
			abline(v=colx)
			abline(h=rowy + 1)
			text(colx-1, max(rowy) + 1.5,variables[[i]],pos=4, cex=0.7)
			for(k in info2020[[i]])
			{
				maxrow <- ifelse(nrow(z)>nlines,nlines,nrow(z))
				text(colx[k]-1,rowy[1:maxrow]+0.5,z[1:maxrow,variables[[i]][k]],pos=4, cex=0.7)
			}
			dev.off()
		}	
	}
}


## Making plots without information

species <- c("Sibbaldia procumbens","Veronica alpina")
#species2 <- c("Novel", "Extant")

variables_oneyear <- list(c("IDS","MS","IDG","X","Y","LSL","NL","LL","NFS","NFL","NB","NC","NAC","Comment"),
                  c("IDS","MS","IDG","X","Y","SH","NL","LL","WL","NFL","NB","NC","NAC","Comment"))

#variables2 <- list(c("IDS", "IDS_Sp18", "NL18","MS","IDG","X","Y","SH","NL","LL","WL", "NFS", "N/D F","N/D B","N/D C","N/D AC", "IL", "Comment"),
#                  c("IDS","IDS_Sp18", "NL18","MS","IDG","X","Y","SH","NL","LL","WL", "NFS", "NF","NB","NC","NAC", "IL", "Comment")) 

# variables from t0 to t1
info2020 <- list(1:9, 1:9)    						 
#info20182 <-  list(1:7, 1:7)

notation <- list(	c("IDS  Shoot ID","MS   Mother shoot","IDG  Genet ID","X     x coordinate","Y     y coordinate","LSL   Leaf stalk length of longest leaf (mm)","NL    Number of leaves","LL    Length of the biggest leaf (mm)","NFL  Number of flowers","NB    Number of buds","NC    Number of capsules","NAC  Number of aborted capsules"),
                  c("IDS  Shoot ID","MS   Mother shoot","IDG  Genet ID","X     x coordinate","Y     y coordinate","SH   Shoot height (mm)","NL    Number of leaves","LL    Length of the biggest leaf (mm)","WL    Width of the biggest leaf (mm)","NFL  Number of flowers","NB    Number of buds","NC    Number of capsules","NAC  Number of aborted capsules"))



  # set number of lines per sheet
    nlines <- 40
    setwd("O:/PhD/R_stats/incline/Data")
    
    for(i in 1:length(species)){
    # create layout for the sheet
    pdf(file=paste(species[i],".pdf"),width=8.3,height=11.7)	
    L <- layout(matrix(c(1,2,3,3),2,2,byrow=T),heights=c(10.5, 19.0),width=c(6,15))
    
    # annotation	
    par(mar=c(0,1,0,0))
    plot(0,0,xlim=c(0,100),ylim=c(0,200),type="n",bty="n",axes=F,xlab="",ylab="")
    text(0,195,species[i],cex=1.5,pos=4)	
    text(0,180,"Date           _________________________",pos=4, cex=0.8)
    text(0,165,"Registrator _________________________",pos=4, cex=0.8)
    text(0,155,"Notation",pos=4, cex=0.8)
    for(k in 1:length(notation[[i]])) text(0,155-k*10,notation[[i]][k],pos=4, cex=0.8)
    
    # map
    par(mar=c(4,4,4,2))
    plot(0,0, xlim=c(-2,37),ylim=c(-2,27),xlab="X (cm)",ylab="Y (cm)", main="Plot: _________________")
    abline(h=c(5,10,15,20),lty=3,col="gray")
    abline(v=c(5,10,15,20,25,30),lty=3,col="gray")
    abline(h=c(0,25),lty=2)
    abline(v=c(0,35),lty=2)
    axis(side = 1, at = c(0,5,10,15,20,25,30,35))
    
    # data sheet
    par(mar=c(2,1,1,1))
    plot(0,0,xlim=c(5,150),ylim=c(2,nlines+1),type="n",bty="n",axes=F,xlab="",ylab="")
    colx <- nchar(variables_oneyear[[i]])+c(8,8,8,10,10,8,8,8,8,14,14,14,14,4)
    colx[length(colx)] <- colx[length(colx)]*2.5
    colx <- cumsum(colx)
    colx <- colx*150/max(colx)
    colx <- c(0,colx[-length(colx)])
    rowy <- (nlines:1)
    abline(v=colx)
    abline(h=rowy + 1)
    text(colx, max(rowy) + 1.5,variables_oneyear[[i]],pos=4, cex=0.7)
    dev.off()
    }
    