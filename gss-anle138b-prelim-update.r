require(survival)
tcolor='#D18532' # "treated"
pcolor='#0930B0' # "placebo"

setwd('~/d/sci/032anle138b/data')

# survival curves
png('prelim.fig1.survival.curves.png',width=600,height=400)
surv_dpi = c(173,142,149,162,177,128,135,178,158,158,156,179,138,160,157,171)
grp = c(rep("anle138b",12),rep("placebo",4))
event = rep(1,16) # all these mice died
mean(surv_dpi[grp=="anle138b"])
mean(surv_dpi[grp=="placebo"])
survdf = data.frame(surv_dpi,event,grp)
plot(survfit(Surv(surv_dpi,event==1)~grp,data=survdf),col=c(tcolor,pcolor),lwd=3,
     yaxt='n',xlab='Age (days)',ylab='Percent surviving',
     main = 'Survival of anle138b-treated vs. placebo-treated GSS mice',
     sub='Preliminary data as of May 2014. n = 12 anle138b, 4 placebo',cex.sub=.7)
axis(side=2,at=(0:4)/4,labels=paste((0:4)*25,"%",sep=""),cex.axis=.8)
legend('bottomleft',c("anle138b","placebo"),col=c(tcolor,pcolor),lwd=3)
dev.off()

# relative plaque area
png('prelim.fig2.plaque.area.png',width=600,height=400)
plaques_cerebellum = c(431,260,477,440,412,324,592,461,498,674,855,517,943,1642,1533,783)
plaques_cb_relative = plaques_cerebellum / mean(plaques_cerebellum[grp=="placebo"])
m = lm(plaques_cb_relative ~ grp)
summary(m)
confint(m)
barplot(c(mean(plaques_cb_relative[grp=="placebo"]),mean(plaques_cb_relative[grp=="anle138b"])),
        col=c(pcolor,tcolor),border=NA,yaxt='n',names.arg=rev(unique(grp)),ylim=c(0,1.7),
        main="Relative PrP plaque area in cerebellum of GSS mice",
        sub='Preliminary data as of May 2014. n = 12 anle138b, 4 placebo',cex.sub=.7)
axis(side=2,at=(0:6)*.25,labels=paste((0:6)*25,"%",sep=""),cex.axis=.8)
arrows(x0=.7,y0=sum(confint(m)[,1]),y1=sum(confint(m)[,2]),code=3,angle=90,length=.1,lwd=2)
arrows(x0=1.9,y0=confint(m)[1,1],y1=confint(m)[1,2],code=3,angle=90,length=.1,lwd=2)
arrows(x0=.7,x1=1.9,y0=1.5,length=0,lwd=1)
text(1.2,1.5,pos=3,"*** p = .0001")
dev.off()