pdf(file="Empty_page.pdf",width=8.3,height=11.7)	########## Changed Site to Site18 and Treat to Treat18
L <- layout(matrix(c(1,2,3,3),2,2,byrow=T),heights=c(10.5, 19.2),width=c(6,15))

# annotation	
par(mar=c(0,1,0,0))
plot(0,0,xlim=c(0,100),ylim=c(0,200),type="n",bty="n",axes=F,xlab="",ylab="")
text(0,195,"species[i]",cex=1.5,pos=4)	
text(0,180,"Date           _________________________",pos=4, cex=0.8)
text(0,165,"Registrator _________________________",pos=4, cex=0.8)
text(0,155,"Notation",pos=4, cex=0.8)
for(k in 1:length(notation[[i]])) text(0,155-k*10,notation[[i]][k],pos=4, cex=0.8)

# map
par(mar=c(4,4,4,1))
plot(y$X18,y$Y18,xlim=c(-2,37),ylim=c(-2,27),xlab="X (cm)",ylab="Y (cm)")
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
     dev.off()

