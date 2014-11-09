
plasmid =   20000
cosmid  =   45000
PAC     =  150000
BAC     =  300000
YAC     = 1000000

constructs = cbind(plasmid,cosmid,PAC,BAC,YAC)[1,]

par(mar=c(0,5,3,1))
barplot(rev(constructs),names.arg=NA,horiz=TRUE,col='#FFB00F',border=NA,axes=FALSE,
        xlim=c(0,1100000))
mtext(side=3,line=1,text='Typical sizes of vectors used for transgenesis',cex=1.5)
axis(side=2,at=(1:5)*1.2-.5,labels=rev(names(constructs)),las=1,lwd.ticks=0,lwd=0)
text(x=rev(constructs),y=(1:5)*1.2-.5,labels=c("1Mb","300kb","150kb","45kb","20kb"),pos=4)

nonts = 1
intron = 3
utr = 10
coding = 20

# chr2:131909928-131938436
mo_prnp_tss = 131909928
exon_lengths = c(76,98,2017)  
exon_starts = c(0,2267,26492)
cds_start = 26501
cds_end = 27266

hgstart = -5500 # Fischer 1996
hgend = exon_starts[3]+exon_lengths[3]+3000 # Fischer 1996
iapend = exon_starts[3]-5404 # Lee 1998
iapstart = iapend-6593 # Lee 1998
ilnjstart = -6000
ilnjend = exon_starts[3]+exon_lengths[3]+15500 # Westaway 1991
costetstart = -24000
costetend = exon_starts[3]+exon_lengths[3]+6000
pacstart = -40000 # just a guess
pacend = 70000 # just a guess

# colors by species
mouse = '#8B8B83'
hamster = '#AA5303'
human = '#3B4990'

# points for lines
breaks = sort(c(exon_starts,exon_starts+exon_lengths))
lwds = c(utr,intron,utr,intron,utr,coding,utr)
exon1start = 0
exon1end = 76
exon2start = 2267
exon2end = 2365
exon3start = 26492
cdsstart = 26501
cdsend = 27266
exon3end = 27509
breaks = c(exon1start,exon1end,exon2start,exon2end,exon3start,cdsstart,cdsend,exon3end)


png('~/d/j/cureffi/media/2014/11/transgene-comparison.png',width=750,height=300)
par(mar=c(2,8,5,3))
plot(NA,NA,xlim=c(mo_prnp_tss-10000,mo_prnp_tss+45000),ylim=c(0.2,5.5),axes=FALSE,xlab='',ylab='')
axis(side=2,at=1:5,labels=c("HuPrP PAC","Half-genomic","I/LnJ cosmid","cos-Tet","mm10 reference"),lwd=0,lwd.ticks=0,las=1)
axis(side=1,at=c(mo_prnp_tss,mo_prnp_tss+exon3end),labels=c(mo_prnp_tss,mo_prnp_tss+exon3end),lwd=0,lwd.ticks=1)
breaks = c(exon1start,exon1end,exon2start,exon2end,iapstart,iapend,exon3start,cdsstart,cdsend,exon3end)
lwds=c(utr,intron,utr,intron,intron,intron,utr,coding,utr)
for (i in 1:(length(breaks)-1)) {
  points(mo_prnp_tss+breaks[i:(i+1)],rep(5,2),type='l',lwd=lwds[i],lend=1)
}

# cosTet
breaks = c(costetstart,exon1start,exon1end,exon2start,exon2end,iapstart,iapend,exon3start,cdsstart,cdsend,exon3end,costetend)
lwds=c(nonts,utr,intron,utr,intron,intron,intron,utr,coding,utr,nonts)
ltys=c(1,1,3,1,1,3,1,1,1,1,1)
cols=rep(hamster,11)
for (i in 1:(length(breaks)-1)) {
  points(mo_prnp_tss+breaks[i:(i+1)],rep(4,2),type='l',lwd=lwds[i],lend=1,lty=ltys[i],col=cols[i])
}

# I/LnJ cosmid
breaks = c(ilnjstart,exon1start,exon1end,exon2start,exon2end,iapstart,iapend,exon3start,cdsstart,cdsend,exon3end,ilnjend)
lwds=c(nonts,utr,intron,utr,intron,intron,intron,utr,coding,utr,nonts)
ltys=c(1,1,1,1,1,3,1,1,1,1,1)
cols=rep(mouse,11)
for (i in 1:(length(breaks)-1)) {
  points(mo_prnp_tss+breaks[i:(i+1)],rep(3,2),type='l',lwd=lwds[i],lend=1,lty=ltys[i],col=cols[i])
}

# Half-genomic
breaks = c(hgstart,exon1start,exon1end,exon2start,exon2end,iapstart,iapend,exon3start,cdsstart,cdsend,exon3end,hgend)
lwds=c(nonts,utr,intron,utr,intron,intron,intron,utr,coding,utr,nonts)
ltys=c(1,1,1,1,3,3,3,1,1,1,1)
cols=rep(mouse,11)
for (i in 1:(length(breaks)-1)) {
  points(mo_prnp_tss+breaks[i:(i+1)],rep(2,2),type='l',lwd=lwds[i],lend=1,lty=ltys[i],col=cols[i])
}

# HuPrP PAC
breaks = c(pacstart,exon1start,exon1end,exon2start,exon2end,iapstart,iapend,exon3start,cdsstart,cdsend,exon3end,pacend)
lwds=c(nonts,utr,intron,utr,intron,intron,intron,utr,coding,utr,nonts)
ltys=c(1,1,3,1,1,3,1,1,1,1,1)
cols=rep(human,11)
for (i in 1:(length(breaks)-1)) {
  points(mo_prnp_tss+breaks[i:(i+1)],rep(1,2),type='l',lwd=lwds[i],lend=1,lty=ltys[i],col=cols[i])
}
arrows(x0=mo_prnp_tss-10000,x1=mo_prnp_tss-0,y0=.5,y1=.5,angle=90,code=3,lwd=3,length=.05,lend=1)
text(x=mo_prnp_tss-5000,y=.5,pos=1,label='10kb')

mtext(side=3,line=2,text='Genomic regions included in PrP transgenes, relative to mm10 reference genome',cex=1.2)

text(x=mo_prnp_tss+15000,y=5,pos=3,label='Prnp^a reference sequence',col='black',cex=.8)
text(x=mo_prnp_tss+exon1end,y=4,pos=3,label='Hamsters lack intron 1',col=hamster,cex=.8)
text(x=mo_prnp_tss+iapend,y=4,pos=3,label='Hamsters lack IAP insertion',col=hamster,cex=.8)
text(x=mo_prnp_tss+iapend,y=3,pos=3,label='Prnp^b mice lack IAP insertion',col=mouse,cex=.8)
text(x=mo_prnp_tss+iapstart,y=2,pos=3,label='Intron 2 deleted',col=mouse,cex=.8)
text(x=mo_prnp_tss+exon1end,y=1,pos=3,label='Humans lack intron 1',col=human,cex=.8)
text(x=mo_prnp_tss+iapend,y=1,pos=3,label='Humans lack IAP insertion',col=human,cex=.8)

legend(x=mo_prnp_tss+35000,y=5.5,legend=c('mouse','hamster','human'),col=c(mouse,hamster,human),lwd=3,bty='n')
dev.off()