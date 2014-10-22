
# plot of PrPC expression vs incubation time
mousename = c("Prnp+/-","Prnp+/+","Tga19/+","Tga20/+","Tga20/Tga20")
explevel = c(.5,1,3.5,6.5,10) # PrPC expression level
terminal = c(415,166,100,68,62) # time to terminal illness
symptoms = c(290,131,87,64,60) # time to symptoms
par(mar=c(5,5,3,1))
plot(NA,NA,xlim=c(.5,13),ylim=c(0,500),axes=FALSE,
    xlab='PrPC expression level (fold wild-type)',ylab='days post-infection',
    main='Effect of PrPC expression level\non disease progression',
    )
tk = '#8E2323'
sk = '#FF9912'
points(explevel,terminal,type='b',pch=19,lwd=8,col=tk)
points(explevel,symptoms,type='b',pch=19,lwd=8,col=sk)
points(c(.5,.2),c(415,600),type='l',lwd=8,col=tk,lty=3)
points(c(.5,.2),c(290,550),type='l',lwd=8,col=sk,lty=3)
text(explevel,terminal,labels=mousename,pos=c(4,4,3,2,2),cex=.8)
text(rep(max(explevel),2),c(min(terminal)+7,min(symptoms)-7),pos=4,labels=c('terminal illness','symptom onset'),col=c(tk,sk))
axis(side=1,at=explevel,labels=c('.5','1','3.5','6.5','10'),lwd=0,lwd.ticks=1,cex.axis=.8)
axis(side=2,at=(0:4)*100,labels=(0:4)*100,lwd=0,lwd.ticks=1,las=1,cex.axis=.8)