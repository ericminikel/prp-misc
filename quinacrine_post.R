setwd('~/d/sci/src/prp-misc')
sar = read.table('klingenstein-2006-data.tsv',header=TRUE,comment.char='#',sep='\t')
sar$prion_ec50 = sar$prion_ec50_nm * 1e-9
sar$malaria_ec50 = sar$malaria_ec50_nm * 1e-9
series_colors = data.frame(series=c('Q','S','A','BC'),
                           colors=c('#FF2015','#FF9912','#00A912','#7924C3'),
                           pch=c(15,16,17,18),
                           names=c('polyquinolines','sulfonamides','amides','amines'))
sar$color = series_colors$colors[match(sar$series,series_colors$series)]
sar$pch = series_colors$pch[match(sar$series,series_colors$series)]
png('~/d/j/cureffi/media/2015/06/klingenstein-2006a-sar-comparison.png',width=800,height=500,res=120)
par(mar=c(5,6,4,2))
plot(NA, NA, xlim=c(-8,-4.9), ylim=c(-8,-4.9), xlab='', ylab='', axes=FALSE, xaxs='i', yaxs='i')
abline(h=-8,lwd=2)
abline(v=-8,lwd=2)
abline(h=-5,lwd=1,lty=3)
abline(v=-6,lwd=1,lty=3)
points(log10(sar$malaria_ec50), log10(sar$prion_ec50), pch=sar$pch, col=sar$color, cex=1.5)
axis(side=1, at=-8:-6, labels=c('10 nM','100 nM','\u22651uM'), lwd=0, lwd.ticks=1)
axis(side=2, at=-8:-5, labels=c('10 nM','100 nM','1 uM','\u226510uM'), lwd=0, lwd.ticks=1, las=2)
mtext(side=1, line=2.5, text='Anti-malarial EC50', font=2, cex=.9)
mtext(side=2, line=4, text='Anti-prion EC50', font=2, cex=.9)
mtext(side=3, line=1, text='Data from Klingenstein 2006 (PMID: 16913719)', cex=.7)
mtext(side=3, line=2, text='SAR of quinolines against RML prions vs. malaria', font=2)
legend('bottomright',series_colors$names,col=series_colors$colors,pch=series_colors$pch,bty='n',cex=.9,pt.cex=1.5)
dev.off()

cor.test(sar$malaria_ec50, sar$prion_ec50, method='spearman')
cor.test(sar$malaria_ec50[sar$series=='Q'], sar$prion_ec50[sar$series=='Q'], method='spearman')
cor.test(sar$malaria_ec50[sar$series=='S'], sar$prion_ec50[sar$series=='S'], method='spearman')
cor.test(sar$malaria_ec50[sar$series=='A'], sar$prion_ec50[sar$series=='A'], method='spearman')
cor.test(sar$malaria_ec50[sar$series=='BC'], sar$prion_ec50[sar$series=='BC'], method='spearman')
cor.test(sar$malaria_ec50[sar$series!='Q'], sar$prion_ec50[sar$series!='Q'], method='spearman')
