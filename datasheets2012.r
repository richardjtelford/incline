# Script for data sheet preparation

library(xtable)

sp <- c("valp","vbif","vpal")
species <- c("Veronica alpina","Viola biflora","Viola palustris")
variables <- list(c("IDS","MS11","IDG11","X","Y","SH11","NFL11","SH","NL","LL","WL","NFL","NB","NC","NAC","Comment"),
			c("IDS","X","Y","NL11","NF11","FH","NL","LL","WL","NF","NB","NC","NAC","Comment"),
			c("IDS","MS","IDG","X","Y","NL11","PF11","FH","NL","LL","WL","LSL","PF","PB","PC","PAC","Comment")) ########## Changed SH10, NFL10, etc to SH11, NFL11, ...
info2011 <- list(1:7,1:5,1:7)    							############# Changed info2010 to info 2011 
notation <- list(	c("IDS  Shoot ID","MS   Mother shoot","IDG  Genet ID","X     x coordinate","Y     y coordinate","SH   Shoot height (mm)","NL    Number of leaves","LL    Length of the longest leaf (mm)","WL   Width of the broadest leaf (mm)","NFL  Number of flowers","NB    Number of buds","NC    Number of capsules","NAC  Number of aborted capsules"),
			c("IDS  Shoot ID","X     x coordinate","Y     y coordinate","FH   Flower height (mm)","NL    Number of leaves","LL    Length of the longest leaf (mm)","NF    Number of flowers","NB    Number of buds","NC    Number of capsules","NAC  Number of aborted capsules"),
			c("IDS  Shoot ID","MS   Mother shoot","IDG  Genet ID","X     x coordinate","Y     y coordinate","FH   Flower height (mm)","NL    Number of leaves","LL    Length of the longest leaf (mm)","WL   Width of the broadest leaf (mm)","LSL  Length of the longest leaf stalk (mm)","PF    Presence and type of flower","PB    Presence of bud","PC    Presence of capsule","PAC  Presence of aborted capsule"))


setwd("D:/Users/siriol/SeedClim/Skjemaer demografi")				############# Moved this out of the loop (need only do it once

for(i in 1:length(sp))
{
	#print(sp[i])

	# Load data
	x10 <- read.delim(paste(sp[i],"10.txt",sep=""),dec=".")		############# Changed dec="," to dec="."
	x11 <- read.delim(paste(sp[i],"11.txt",sep=""),dec=".")		############# Changed dec="," to dec="."
	
	# Merge data for 2010 and 2011						############# Changed headline to 2010 and 2011
	x <- merge(x10,x11,by.x=c("Plot.ID","IDS"),by.y=c("No","IDS"),all=TRUE,suffixes=c("10","11"))

	# Remove superfluous 2010 data: each IDS is replicated five times in the Veronica alpina data files
	x <- x[!duplicated(paste(x$Plot.ID,x$IDS)),]

	# Make varialbes for the last recorded coordinates
	x$X <- x$X11; x$X[!is.na(x$X11)] <- x$X11[!is.na(x$X11)]		############# Changed 'x$X <- x$X10' to 'x$X <- x$X11'
	x$Y <- x$Y11; x$Y[!is.na(x$Y11)] <- x$Y11[!is.na(x$Y11)]		############# Changed 'x$Y <- x$Y10' to 'x$Y <- x$Y11'
	
	# Adjust variable names
	if(sp[i]=="vbif") x$NF11 <- x$NF
	if(sp[i]=="vpal") x$PF11 <- x$PF

	# Make maps for each plot
	pid <- unique(x$Plot.ID)
	for(j in 1:length(pid))
	{
		#print(pid[j])
		y <- x[x$Plot.ID==pid[j],]
				
		# Make numeric IDS (replace "b" with .5) and sort
		y$IDSnum <- as.numeric(gsub("b",".5",y$IDS))
		y <- y[order(y$IDSnum),]
		
		# index for dead/missing individuals
		dead <- is.na(y$X11) | is.na(y$NL11) | y$NL11==0		############# Changed the criterion for "dead" to include missing coordinates OR missing NL OR NL = 0

		# set number of lines per sheet, if more than nlines individuals, print new sheets with maps of all ind
		nlines <- 40
		start <- 1; stop <- min(nlines,nrow(y))
		sheet <- 0
		while(stop >= start)
		{
			z <- y[start:stop,]
			start <- start+nlines; stop <- min(stop+nlines,nrow(y))
			sheet <- sheet+1

			pdf(file=paste(species[i],y$Site11[1],y$Block11[1],pid[j],y$Treat11[1],sheet,".pdf"),width=8.27,height=11)	########## Changed Site to Site11 and Treat to Treat11
			L <- layout(matrix(c(1,2,3,3),2,2,byrow=T),heights=c(1,2))
					
			par(mar=c(0,1,0,0))
			plot(0,0,xlim=c(0,100),ylim=c(0,200),type="n",bty="n",axes=F,xlab="",ylab="")
			text(0,195,species[i],cex=1.5,pos=4)	
			text(0,180,"Date           _________________________",pos=4)
			text(0,165,"Registrator _________________________",pos=4)
			text(0,150,"Notation",pos=4)
			for(k in 1:length(notation[[i]])) text(0,150-k*10,notation[[i]][k],pos=4)

			par(mar=c(4,4,4,1))
			plot(y$X11[!dead],y$Y11[!dead],xlim=c(-2,27),ylim=c(-2,27),xlab="X (cm)",ylab="Y (cm)",
				main=paste("Site", y$Site11[1], ", block", y$Block11[1], ", plot ", pid[j], ", treatment", y$Treat11[1]))	########## Changed Site to Site11 and Treat to Treat11
			mtext("o present 2011, + missing/dead 2011",cex=0.8,line=0.5)
			if(any(!dead)) text(y$X11[!dead]+0.5,y$Y11[!dead]+0.5,y$IDS[!dead],cex=0.8)
			if(any(dead))
			{
				if(any(!is.na(y$X10)))											########## Plot "dead" with coordinates from two years ago (2010), if they exist... )
				{
					points(y$X10[dead],y$Y10[dead],pch=3,cex=0.7)
					text(y$X10[dead]+0.5,y$Y10[dead]+0.5,as.character(y$IDS[dead]),cex=0.8)
				}
				else														########## ... otherwise use coordinates from last year (2011
				{
					points(y$X11[dead],y$Y11[dead],pch=3,cex=0.7)
					text(y$X11[dead]+0.5,y$Y11[dead]+0.5,as.character(y$IDS[dead]),cex=0.8)
				}

			}
			#points(y$X11,y$Y11)
			text(y$X11+0.5,y$Y11+0.5,y$IDS,cex=0.8)		
			abline(h=c(6.25,12.5,18.75),lty=3,col="gray")
			abline(v=c(6.25,12.5,18.75),lty=3,col="gray")
			abline(h=c(0,25),lty=2)
			abline(v=c(0,25),lty=2)

			par(mar=c(1,1,1,1))
			plot(0,0,xlim=c(5,150),ylim=c(10,nlines*10),type="n",bty="n",axes=F,xlab="",ylab="")
			colx <- nchar(variables[[i]])+10
			colx[length(colx)] <- colx[length(colx)]*2.5
			colx <- cumsum(colx)
			colx <- colx*150/max(colx)
			colx <- c(0,colx[-length(colx)])
			rowy <- (nlines:1)*10
			abline(v=colx)
			abline(h=rowy)
			text(colx,max(rowy)+5,variables[[i]],pos=4)
			for(k in info2011[[i]])
			{
				maxrow <- ifelse(nrow(z)>nlines,nlines,nrow(z))
				text(colx[k],rowy[1:maxrow]-5,z[1:maxrow,variables[[i]][k]],pos=4)
			}
			dev.off()
		}	
	}
}

