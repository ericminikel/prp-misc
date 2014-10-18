require(survival)
tcolor='#D18532' # "treated"
pcolor='#0930B0' # "placebo"

setwd('~/d/sci/032anle138b/data')

# updated with new data as of 2014-10-18

# survival curves
pdf('prelim.fig1.a2.gss.survival.curves.pdf',width=8,height=6)
# A2 = clinical signs
n_anle138b = 13
n_placebo = 10
surv_dpi = c(131,112,124,131,131,104,118,141,128,128,113,121,119,106,132,116,109,138,103,103,137,120,106)
grp = c(rep("anle138b",n_anle138b),rep("placebo",n_placebo))
event = rep(1,n_anle138b+n_placebo) # all these mice died
mean(surv_dpi[grp=="anle138b"])
mean(surv_dpi[grp=="placebo"])
survdf = data.frame(surv_dpi,event,grp)
plot(survfit(Surv(surv_dpi,event==1)~grp,data=survdf),col=c(tcolor,pcolor),lwd=3,
     yaxt='n',xlab='Age (days)',ylab='Percent asymptomatic',xaxs='i',yaxs='i',xlim=c(0,250),ylim=c(0,1.05),axes=FALSE,
     main = 'Time to clinical onset in anle138b-treated vs. placebo-treated GSS mice',
     sub=paste('Preliminary data as of Oct 2014. n = ',n_anle138b,' anle138b, ',n_placebo,' placebo',sep=''),cex.sub=.7)
axis(side=2,at=(0:4)/4,labels=paste((0:4)*25,"%",sep=""),cex.axis=.8,las=1)
axis(side=1,at=(0:5)*50,labels=(0:5)*50,cex.axis=.9)
legend('bottomleft',c("anle138b","placebo"),col=c(tcolor,pcolor),lwd=3)
dev.off()

pdf('prelim.fig1.a5.gss.survival.curves.pdf',width=8,height=6)
# A5 = terminal illness
n_anle138b = 13
n_placebo = 6
surv_dpi = c(173,131,145,160,177,126,135,176,158,158,153,178,205,156,170,159,169,151,151)
grp = c(rep("anle138b",n_anle138b),rep("placebo",n_placebo))
event = rep(1,n_anle138b+n_placebo) # all these mice died
mean(surv_dpi[grp=="anle138b"])
mean(surv_dpi[grp=="placebo"])
survdf = data.frame(surv_dpi,event,grp)
plot(survfit(Surv(surv_dpi,event==1)~grp,data=survdf),col=c(tcolor,pcolor),lwd=3,
     yaxt='n',xlab='Age (days)',ylab='Percent surviving',xaxs='i',yaxs='i',xlim=c(0,250),ylim=c(0,1.05),axes=FALSE,
     main = 'Time to terminal illness of anle138b-treated vs. placebo-treated GSS mice',
     sub=paste('Preliminary data as of Oct 2014. n = ',n_anle138b,' anle138b, ',n_placebo,' placebo',sep=''),cex.sub=.7)
axis(side=2,at=(0:4)/4,labels=paste((0:4)*25,"%",sep=""),cex.axis=.8,las=1)
axis(side=1,at=(0:5)*50,labels=(0:5)*50,cex.axis=.9)
legend('bottomleft',c("anle138b","placebo"),col=c(tcolor,pcolor),lwd=3)
dev.off()

pdf('prelim.fig1.death.rml.survival.curves.pdf',width=8,height=6)
n_anle138b = 10
n_placebo = 10
surv_dpi = c(211,211,239,239,239,136,136,136,136,136,141,138,141,114,114,114,114,114,138,144)
# N.B. the 2nd placebo animal (above) was in the same cohort as the 1st and 3rd, and is labeled "Died" - guessed ~138 dpi
grp = c(rep("anle138b",n_anle138b),rep("placebo",n_placebo))
event = c(rep(0,10),1,1,1,rep(0,5),1,1) # all 10 anle138b still living, 5 of 10 placebo have died
mean(surv_dpi[grp=="anle138b"])
mean(surv_dpi[grp=="placebo"])
survdf = data.frame(surv_dpi,event,grp)
plot(survfit(Surv(surv_dpi,event==1)~grp,data=survdf),col=c(tcolor,pcolor),lwd=3,
     yaxt='n',xlab='Days post-infection',ylab='Percent surviving',xaxs='i',yaxs='i',xlim=c(0,250),ylim=c(0,1.05),axes=FALSE,
     main = 'Time to terminal illness in anle138b-treated vs. placebo-treated\nRML prion-infected mice',
     sub=paste('Preliminary data as of Oct 2014. n = ',n_anle138b,' anle138b, ',n_placebo,' placebo',sep=''),cex.sub=.7)
axis(side=2,at=(0:4)/4,labels=paste((0:4)*25,"%",sep=""),cex.axis=.8,las=1)
axis(side=1,at=(0:5)*50,labels=(0:5)*50,cex.axis=.9)
legend('bottomleft',c("anle138b","placebo"),col=c(tcolor,pcolor),lwd=3)
dev.off()

# relative plaque area
pdf('prelim.fig2.plaque.area.pdf',width=8,height=4)
plaques_cerebellum = c(431,260,477,440,412,324,592,461,498,674,855,517,943,1642,1533,783)
n_anle138b = 12
n_placebo = 4
grp = c(rep("anle138b",n_anle138b),rep("placebo",n_placebo))
plaques_cb_relative = plaques_cerebellum / mean(plaques_cerebellum[grp=="placebo"])
m = lm(plaques_cb_relative ~ grp)
summary(m)
confint(m)
barplot(c(mean(plaques_cb_relative[grp=="placebo"]),mean(plaques_cb_relative[grp=="anle138b"])),
        col=c(pcolor,tcolor),border=NA,yaxt='n',names.arg=rev(unique(grp)),ylim=c(0,1.7),
        main="Relative PrP plaque area in cerebellum of GSS mice",
        sub=paste('Preliminary data as of May 2014. n = ',n_anle138b,' anle138b, ',n_placebo,' placebo',sep=''),cex.sub=.7)
axis(side=2,at=(0:6)*.25,labels=paste((0:6)*25,"%",sep=""),cex.axis=.8,lwd=0,lwd.ticks=1,las=1)
abline(h=0)
arrows(x0=.7,y0=sum(confint(m)[,1]),y1=sum(confint(m)[,2]),code=3,angle=90,length=.1,lwd=2)
arrows(x0=1.9,y0=confint(m)[1,1],y1=confint(m)[1,2],code=3,angle=90,length=.1,lwd=2)
arrows(x0=.7,x1=1.9,y0=1.5,length=0,lwd=1)
text(1.2,1.5,pos=3,"*** p = .0001")
dev.off()