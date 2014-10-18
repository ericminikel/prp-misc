
pdf('no-substitute.pdf',width=8,height=6)
qtys = c(17217,45629,405874)
labels = c("our project [1]",
           "average successful\ncancer crowdfunding\nproject [2]",
           "average R01 grant [3]")
citations = c("[1] https://experiment.com/projects/can-anle138b-delay-the-onset-of-genetic-prion-disease",
    "[2] Reviewed in Dragojlovic & Lind 2014 'Crowdfunding drug development: the state of play in oncology and rare diseases' PMID: 24973645",
    "[3] In 2013. Source: http://nexus.od.nih.gov/all/2014/01/10/fy2013-by-the-numbers/")

k = '#CC1100'
par(mar=c(7,1,3,1))
plot(NA,NA,xlim=c(.5,3.5),ylim=c(0,500000),axes=FALSE,xlab='',ylab='',
    main='Crowdfunding is no substitute for conventional funding sources')
points(1:3,qtys,type='h',lend=1,lwd=100,col=k)
text(1:3,qtys,labels=c("$17K","$45K","$406K"),pos=3)
abline(h=0)
axis(side=1,at=1:3,labels=labels,lwd=0,lwd.ticks=0,cex.axis=.8,padj=0)
mtext(side=1,line=3:5,text=citations,cex=.5,adj=0)
dev.off()