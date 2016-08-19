setwd('~/d/sci/src/prp-misc')
options(stringsAsFactors=FALSE)

fdgcol='#54AB00'
dwicol='#3B0072'
lncol='#333333'

png('figures/imaging_case_reports.png',width=800,height=400,res=85)
par(mar=c(3,10,3,1))
plot(NA,NA,xlim=c(-67,18),ylim=c(.5,8.5),xaxs='i',yaxs='i',axes=FALSE,ann=FALSE)
rect(xleft=-67,xright=0,ybottom=0,ytop=9,col='#FFFFFF',border=NA)
rect(xleft=0,xright=24,ybottom=0,ytop=9,col='#CCCCCC',border=NA)
abline(v=0, lty=3, lwd=6, col='#000000')
# Zanusso 2016
points(x=c(-26,-14,2),y=rep(1,3),pch=c(1,19,19),lwd=4,col=dwicol)
# Verde 2016
points(x=c(-12,0,3),y=rep(2,3),pch=c(19,19,19),lwd=4,col=dwicol)
# Terasawa 2012
points(x=c(-3,0,8),y=rep(3,3),pch=c(19,19,19),lwd=4,col=dwicol)
# Satoh 2011
points(x=c(-2,-1,1,2),y=rep(4,4),pch=c(19,19,19,19),lwd=4,col=dwicol)
# Cortelli 2006 subject 5
points(x=c(-21,12),y=rep(8,2),pch=c(1,19),lwd=4,col=fdgcol)
# Cortelli 2006 subject 6
points(x=c(-65,-32,-13,7,10),y=rep(7,5),pch=c(1,1,19,19,19),lwd=4,col=fdgcol)
# Cortelli 2006 subject 7
points(x=c(-63),y=rep(6,1),pch=c(1),lwd=4,col=fdgcol)
# Cortelli 2006 subject 8
points(x=c(-56,7),y=rep(5,2),pch=c(1,19),lwd=4,col=fdgcol)
abline(h=seq(.5,8.5,1),lwd=.5,col=lncol)
abline(h=c(.5,8.5),lwd=1,col=lncol)
axis(side=1, at=(-5:1)*12, labels=NA, lwd.ticks=1, lwd=0, cex.axis=.7)
mtext(side=1, line=1, at=(-5:1)*12, text=c('5y pre-onset',
                                   '4y pre-onset',
                                   '3y pre-onset',
                                   '2y pre-onset',
                                   '1y pre-onset',
                                   'first symptom',
                                   '1y post-onset'),
      font=c(1,1,1,1,1,2,1), cex=.8)
axis(side=2, at=8:1, lwd.ticks=0, lwd=0, las=2, cex.axis=.9, font.axis=3,
                     labels=c('Cortelli 2006 subject 5',
                              'Cortelli 2006 subject 6',
                              'Cortelli 2006 subject 7',
                              'Cortelli 2006 subject 8',
                              'Satoh 2011',
                              'Terasawa 2012',
                              'Verde 2016',
                              'Zanusso 2016'))
mtext(side=3, line=1, text='case reports of brain imaging before and after prion disease onset', font=2, cex=1.2)
legend('bottomleft',title='key',legend=c('normal FDG-PET','abnormal FDG-PET','normal dwMRI','abnormal dwMRI'),
       col=c(fdgcol,fdgcol,dwicol,dwicol),text.col=c(fdgcol,fdgcol,dwicol,dwicol),text.font=2,cex=.9,pch=c(1,19,1,19),
       pt.lwd=4,lwd=0,title.col='#000000',box.lwd=2)
dev.off()

