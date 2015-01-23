options(stringsAsFactors=FALSE)
setwd("~/d/sci/src/prp-misc")
markers = read.table("ftp://ftp.informatics.jax.org/pub/reports/MRK_List1.rpt",header=TRUE,sep="\t",comment.char='',quote='')
linkage = read.table("qtl-mapping.txt",header=TRUE) # whitespace-separated
chrlen = read.table("grcm38-chrom-lengths.txt",header=TRUE)

# properly number and sort the chromosomes
chrlen$chrno = 0
chrlen$chrno[chrlen$chr=='X'] = 23
chrlen$chrno[chrlen$chr!='X'] = as.integer(chrlen$chr[chrlen$chr!='X'])
# calculate the "absolute position" where each chromosome starts
chrlen = chrlen[with(chrlen, order(chrno)),]
chrlen$startpos = cumsum(as.numeric(chrlen$length)) - chrlen$length + 1
chrlen$endpos = chrlen$startpos + chrlen$length

# find genomic coordinates of markers
linkage$chrom = markers$Chr[match(linkage$marker,markers$Marker.Symbol)]
linkage$pos = markers$genome.coordinate.start[match(linkage$marker,markers$Marker.Symbol)]

# find x-coordinate for marker positions
linkage$abspos = chrlen$startpos[match(linkage$chrom,chrlen$chr)] + linkage$pos

chrbreaks = c(chrlen$startpos,sum(as.numeric(chrlen$length)))
# find places to label chromosomes - midpoint between each line
chrmids = (chrbreaks[1:(length(chrbreaks)-1)] + chrbreaks[2:length(chrbreaks)]) / 2


plot(NA, NA, type='l', lwd=4, axes=FALSE, xaxs='i', yaxs='i',
    xlim=range(chrbreaks),ylim=c(0,10.5),xlab='',ylab='',
    main='Reported prion incubation time QTLs in mice')
abline(v=c(1,chrbreaks),lwd=.5,col='#AAAAAA')
axis(side=1,at=c(1,chrbreaks),labels=NA)
mtext(side=1,at=chrmids,text=chrlen$chr,cex=.6)
axis(side=2,at=1:10,labels=1:10,lwd=0,lwd.ticks=.5,cex.axis=.8,las=1)
mtext(side=1,line=1,text='chromosomal position')
mtext(side=2,line=2,text='LOD score')

prnp_locus = chrlen$startpos[2] + 131909928
points(x=c(prnp_locus,prnp_locus),y=c(0,9),type='l',lwd=1.5,col='#000000')
text(x=prnp_locus,y=9,pos=3,label='Prnp')

extra_points = data.frame(abspos=chrbreaks,lod=rep(0.0,length(chrbreaks)))

col = c("#FF3030","#FF9912","#FFE303","#859C27","#8B7D7B")
i = 1
for (study in unique(linkage$study)) {
    to_plot = linkage[linkage$study==study,c("abspos","lod")]
    to_plot = rbind(to_plot,extra_points)
    to_plot = to_plot[with(to_plot,order(abspos)),]
    points(to_plot$abspos, to_plot$lod, type='l', lwd=5, col=col[i])
    i = i + 1
}

abline(h=2.8,lwd=.5) # suggestive linkage, mouse intercross F2, Lander & Kruglyak 1995
text(x=max(chrbreaks),y=2.8+.2,label='suggestive linkage',pos=2,cex=.8)
abline(h=4.3,lwd=.5) # significant linkage, mouse intercross F2, Lander & Kruglyak 1995
text(x=max(chrbreaks),y=4.3+.2,label='significant linkage',pos=2,cex=.8)

legend("topright",unique(linkage$study),col=col,lwd=5,bg='white',cex=.6)

