# Script for data sheet preparation

library(xtable)

sp <- c("sibpro","veralp")
sp2 <- c("novel", "extant")

species <- c("Sibbaldia procumbens","Veronica alpina")
species2 <- c("Novel", "Extant")

variables <- list(c("IDS","IDG18","NL18","MS","IDG","X","Y","LSL","NL","LL","NFS","NFL","NB","NC","NAC","Comment"),
                  c("IDS","IDG18","SH18","MS","IDG","X","Y","SH","NL","LL","WL","NFL","NB","NC","NAC","Comment"))

variables2 <- list(c("IDS", "IDS_Sp18", "NL18","MS","IDG","X","Y","SH","NL","LL","WL", "NFS", "N/D F","N/D B","N/D C","N/D AC", "IL", "Comment"),
                  c("IDS","IDS_Sp18", "NL18","MS","IDG","X","Y","SH","NL","LL","WL", "NFS", "NF","NB","NC","NAC", "IL", "Comment")) 

# variables from t0 to t1
info2018 <- list(1:7, 1:7)    						 
info20182 <-  list(1:7, 1:7)

notation <- list(	c("IDS  Shoot ID","MS   Mother shoot","IDG  Genet ID","X     x coordinate","Y     y coordinate","LSL   Length of longest leaf stalk (mm)","NL    Number of leaves","LL    Length of the biggest leaf (mm)","NFL  Number of flowers","NB    Number of buds","NC    Number of capsules","NAC  Number of aborted capsules"),
                  c("IDS  Shoot ID","MS   Mother shoot","IDG  Genet ID","X     x coordinate","Y     y coordinate","SH   Shoot height (mm)","NL    Number of leaves","LL    Length of the biggest leaf (mm)","WL    Width of the biggest leaf (mm)","NFL  Number of flowers","NB    Number of buds","NC    Number of capsules","NAC  Number of aborted capsules"))

notation2 <- list( c("IDS  Shoot ID","MS   Mother shoot","IDG  Genet ID","X     x coordinate","Y     y coordinate","SH   Shoot height (mm)","NL    Number of leaves","LL    Length of the biggest leaf (mm)","WL    Width of the biggest leaf (mm)", "NFS   Number of flower stems", "NF  Number/Diameter (mm) of flowers","NB    Number/Diameter of buds","NC    Number/Diameter (mm) of capsules","NAC  Number/Diameter(mm) of aborted capsules", "IL    Length of influoresence for Carex (mm)"),
c("IDS  Shoot ID","MS   Mother shoot","IDG  Genet ID","X     x coordinate","Y     y coordinate","SH   Shoot height (mm)","NL    Number of leaves","LL    Length of the biggest leaf (mm)","WL    Width of the biggest leaf (mm)", "NFS   Number of flower stems", "NF  Number of flowers","NB    Number of buds","NC    Number of capsules","NAC  Number of aborted capsules", "IL    Length of influoresence for Carex (mm)"))


setwd("//hjemme.uib.no/rgy042/PhD/R_stats/incline/Data")

for(i in 1:length(sp)){
	#print(sp[i])

	# Load data
	x18 <- read.delim(paste(sp[i],"18.txt",sep=""),dec=",", sep=";", stringsAsFactors = FALSE)
	#x18 <- as.numeric(x18$IDS)
#	x19 <- read.delim(paste(sp[i],"19.txt",sep=""),dec=",")		# does not exist yet in 2019
	
	# Merge data for 2018 and 2019						# will first be necessary in 2020
#	x <- merge(x18,x19,by.x=c("Plot.ID","IDS"),by.y=c("No","IDS"),all=TRUE,suffixes=c("18","19"))

	# alternative for 2019	
	x <- x18 
	x$PlotID <- paste(x$Site,x$Block,x$Plot,sep='_')
	
	for (h in c(1:7,9:ncol(x))){colnames(x)[h] <- paste(colnames(x)[h],'18',sep='')}
	colnames(x)

##	# Remove superfluous 2010 data: each IDS is replicated five times in the Veronica alpina data files
##	x <- x[!duplicated(paste(x$Plot.ID,x$IDS)),]

	# Make variables for the last recorded coordinates
	x$X <- x$X18; x$X[!is.na(x$X18)] <- x$X18[!is.na(x$X18)]		############# Changed ...
	x$Y <- x$Y18; x$Y[!is.na(x$Y18)] <- x$Y18[!is.na(x$Y18)]		############# Changed ...
	
	# Adjust variable names
##	if(sp[i]=="vbif") x$NF18 <- x$NF
##	if(sp[i]=="vpal") x$PF18 <- x$PF

	# Make maps for each plot
	pid <- unique(x$PlotID)
	for(j in 1:length(pid))
	{
		#print(pid[j])
		y <- x[x$PlotID==pid[j],]
				
		# Make numeric IDS (replace "b" with .5) and sort
#		y$IDSnum <- as.numeric(gsub("b",".5",y$IDS))      # only if non.numeric IDS names
		y <- y[order(y$IDS),]
		
		# index for dead/missing individuals
		dead <- is.na(y$X18) | is.na(y$NL18) | y$NL18==0		

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
			pdf(file=paste(species[i],pid[j],sheet,".pdf"),width=8.3,height=11.7)	########## Changed Site to Site18 and Treat to Treat18
			L <- layout(matrix(c(1,2,3,3),2,2,byrow=T),heights=c(10.5, 19.2),width=c(6,15))
			
			# annotation	
			par(mar=c(0,1,0,0))
			plot(0,0,xlim=c(0,100),ylim=c(0,200),type="n",bty="n",axes=F,xlab="",ylab="")
			text(0,195,species[i],cex=1.5,pos=4)	
			text(0,180,"Date           _________________________",pos=4, cex=0.8)
			text(0,165,"Registrator _________________________",pos=4, cex=0.8)
			text(0,155,"Notation",pos=4, cex=0.8)
			for(k in 1:length(notation[[i]])) text(0,155-k*10,notation[[i]][k],pos=4, cex=0.8)

			# map
			par(mar=c(4,4,4,1))
			plot(y$X18[!dead],y$Y18[!dead],xlim=c(-2,37),ylim=c(-2,27),xlab="X (cm)",ylab="Y (cm)",
			     main=pid[j] #paste("Site", y$Site18[1], ", block", y$Block18[1], ", plot ", pid[j], ", treatment", y$Treat18[1])
				)	########## Changed Site to Site18 and Treat to Treat18
			#mtext("o present 2018, + missing/dead 2018",cex=0.8,line=0.5)
			if(any(!dead)) text(y$X18[!dead]+0.5,y$Y18[!dead]+0.5,y$IDS[!dead],cex=0.8)
			if(any(dead))
			{
				if(any(!is.na(y$X10)))											########## Plot "dead" with coordinates from two years ago (2010), if they exist... )
				{
					points(y$X10[dead],y$Y10[dead],pch=3,cex=0.7)
					text(y$X10[dead]+0.5,y$Y10[dead]+0.5,as.character(y$IDS[dead]),cex=0.8)
				}
				else														########## ... otherwise use coordinates from last year (2018
				{
					points(y$X18[dead],y$Y18[dead],pch=3,cex=0.7)
					text(y$X18[dead]+0.5,y$Y18[dead]+0.5,as.character(y$IDS[dead]),cex=0.8)
				}

			}
			#points(y$X18,y$Y18)
#			text(y$X18+0.5,y$Y18+0.5,y$IDS,cex=0.8)		
			abline(h=c(5,10,15,20),lty=3,col="gray")
			abline(v=c(5,10,15,20,25,30),lty=3,col="gray")
			abline(h=c(0,25),lty=2)
			abline(v=c(0,35),lty=2)
			axis(side = 1, at = c(0,5,10,15,20,25,30,35))

			# data sheet
			par(mar=c(1,1,1,1))
			plot(0,0,xlim=c(5,150),ylim=c(2,nlines),type="n",bty="n",axes=F,xlab="",ylab="")
			colx <- nchar(variables[[i]])+c(8,8,8,8,8,10,10,8,8,8,8,14,14,14,14,4)
			colx[length(colx)] <- colx[length(colx)]*2.5
			colx <- cumsum(colx)
			colx <- colx*150/max(colx)
			colx <- c(0,colx[-length(colx)])
			rowy <- (nlines:1)
			abline(v=colx)
			abline(h=rowy)
			text(colx,max(rowy)+1,variables[[i]],pos=4, cex=0.7)
			for(k in info2018[[i]])
			{
				maxrow <- ifelse(nrow(z)>nlines,nlines,nrow(z))
				text(colx[k],rowy[1:maxrow]-0.5,z[1:maxrow,variables[[i]][k]],pos=4, cex=0.7)
			}
			dev.off()
		}	
	}
}


### For novel and extant transplants ###

for(i in 1:length(sp2)){
  #print(sp[i])
  
  # Load data
  x18 <- read.delim(paste(sp2[i],"18.txt",sep=""),dec=",", stringsAsFactors = FALSE)
  #x18 <- as.numeric(x18$IDS)
  #	x19 <- read.delim(paste(sp[i],"19.txt",sep=""),dec=",")		# does not exist yet in 2019
  
  # Merge data for 2018 and 2019						# will first be necessary in 2020
  #	x <- merge(x18,x19,by.x=c("Plot.ID","IDS"),by.y=c("No","IDS"),all=TRUE,suffixes=c("18","19"))
  
  # alternative for 2019	
  x <- x18 
  x$PlotID <- paste(x$Site,x$Block,x$Plot,sep='_')
  x$IDS_Sp <- paste(x$Species, x$IDS, sep='')
  
  for (h in c(1:8,10:ncol(x))){colnames(x)[h] <- paste(colnames(x)[h],'18',sep='')}
  colnames(x)
  
  ##	# Remove superfluous 2010 data: each IDS is replicated five times in the Veronica alpina data files
  ##	x <- x[!duplicated(paste(x$Plot.ID,x$IDS)),]
  
  # Make variables for the last recorded coordinates
  x$X <- x$X18; x$X[!is.na(x$X18)] <- x$X18[!is.na(x$X18)]		############# Changed ...
  x$Y <- x$Y18; x$Y[!is.na(x$Y18)] <- x$Y18[!is.na(x$Y18)]		############# Changed ...
  
  # Adjust variable names
  ##	if(sp[i]=="vbif") x$NF18 <- x$NF
  ##	if(sp[i]=="vpal") x$PF18 <- x$PF
  
  # Make maps for each plot
  pid2 <- unique(x$PlotID)
  species3 <- unique(x$Species18)
  for(j in 1:length(pid2))
  {
    #for r in 1:length(species))
  #{
    #print(pid[j])
    y <- x[x$PlotID==pid2[j],]
    
    # Make numeric IDS (replace "b" with .5) and sort
    #		y$IDSnum <- as.numeric(gsub("b",".5",y$IDS))      # only if non.numeric IDS names
    y <- y[order(y$IDS_Sp18),]
    
    # index for dead/missing individuals
    dead <- is.na(y$X18) | is.na(y$NL18) | y$NL18==0		
    
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
      pdf(file=paste(species2[i],pid2[j],sheet,".pdf"),width=8.3,height=11.7)	########## Changed Site to Site18 and Treat to Treat18
      L <- layout(matrix(c(1,2,3,3),2,2,byrow=T),heights=c(10.5, 19.2),width=c(6,15))
      
      # annotation	
      par(mar=c(0,1,0,0))
      plot(0,0,xlim=c(0,100),ylim=c(0,200),type="n",bty="n",axes=F,xlab="",ylab="")
      text(0,195,species2[i],cex=1.5,pos=4)	
      text(0,180,"Date           _________________________",pos=4, cex=0.8)
      text(0,165,"Registrator _________________________",pos=4, cex=0.8)
      text(0,155,"Notation",pos=4, cex=0.8)
      for(k in 1:length(notation2[[i]])) text(0,155-k*10,notation2[[i]][k],pos=4, cex=0.8)
      
      # map
      par(mar=c(4,4,4,1))
      plot(y$X18[!dead],y$Y18[!dead],xlim=c(-2,37),ylim=c(-2,27),xlab="X (cm)",ylab="Y (cm)",
           main=pid2[j] #paste("Site", y$Site18[1], ", block", y$Block18[1], ", plot ", pid[j], ", treatment", y$Treat18[1])
      )	########## Changed Site to Site18 and Treat to Treat18
      #mtext("o present 2018, + missing/dead 2018",cex=0.8,line=0.5)
      if(any(!dead)) text(y$X18[!dead]+0.5,y$Y18[!dead]+0.5,y$IDS[!dead],cex=0.8)
      
      #points(y$X18,y$Y18)
      #			text(y$X18+0.5,y$Y18+0.5,y$IDS,cex=0.8)		
      abline(h=c(5,10,15,20),lty=3,col="gray")
      abline(v=c(5,10,15,20,25,30),lty=3,col="gray")
      abline(h=c(0,25),lty=2)
      abline(v=c(0,35),lty=2)
      axis(side = 1, at = c(0,5,10,15,20,25,30,35))
      
      # data sheet
      par(mar=c(1,1,1,1))
      plot(0,0,xlim=c(5,150),ylim=c(2,nlines),type="n",bty="n",axes=F,xlab="",ylab="")
      colx <- nchar(variables2[[i]])+c(8,8,8,8,8,10,10,8,8,8,8,8,14,14,14,14,8,4)
      colx[length(colx)] <- colx[length(colx)]*2.5
      colx <- cumsum(colx)
      colx <- colx*150/max(colx)
      colx <- c(0,colx[-length(colx)])
      rowy <- (nlines:1)
      abline(v=colx)
      abline(h=rowy)
      text(colx,max(rowy)+1,variables2[[i]],pos=4, cex=0.7)
      for(k in info20182[[i]])
      {
        maxrow <- ifelse(nrow(z)>nlines,nlines,nrow(z))
        text(colx[k],rowy[1:maxrow]-0.5,z[1:maxrow,variables2[[i]][k]],pos=4, cex=0.7)
      }
      dev.off()
    }	
  }
}

