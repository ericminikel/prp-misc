options(stringsAsFactors=FALSE)
ucsc_data = read.table(header=TRUE,textConnection("
name   chrom   strand  txStart txEnd   cdsStart    cdsEnd  exonCount   exonStarts  exonEnds    proteinID   alignID
uc002wkt.1  chr20   +   4666796 4682233 4679866 4680628 3   4666796,4679856,4680215,    4667154,4680125,4682233,    P04156  uc002wkt.1
uc002wku.3  chr20   +   4666796 4682234 4679866 4680628 2   4666796,4679861,    4667154,4682234,    P04156  uc002wku.3
uc002wkv.3  chr20   +   4666796 4682234 4679866 4680628 2   4666796,4679861,    4667158,4682234,    P04156  uc002wkv.3
uc002wkw.4  chr20   +   4666796 4682234 4679955 4680177 2   4666796,4679856,    4667158,4682234,    F7VJQ1  uc002wkw.4
uc002wkx.3  chr20   +   4666796 4682234 4679866 4680628 2   4666796,4679856,    4667154,4682234,    P04156  uc002wkx.3
uc002wky.3  chr20   +   4667156 4682234 4679866 4680628 2   4667156,4679856,    4667382,4682234,    P04156  uc002wky.3
uc010gbe.1  chr20   +   4679856 4681289 4679866 4680791 2   4679856,4680748,    4680516,4681289,    B2NI04  uc010gbe.1
uc021wae.1  chr20   +   4679866 4680628 4679866 4680628 1   4679866,    4680628,    P04156  uc021wae.1"))
transcript = ucsc_data[2,] # a reasonable transcript to plot
exon_starts = as.integer(strsplit(transcript$exonStarts,',')[[1]])
exon_ends = as.integer(strsplit(transcript$exonEnds,',')[[1]])
png('~/d/j/cureffi/media/2015/10/prnp_diagram.png',width=800,height=300,pointsize=20)
plot(NA,NA,xlim=c(transcript$txStart,transcript$txEnd),ylim=c(0,3),axes=FALSE,xlab='',ylab='')
segments(x0=transcript$txStart,x1=transcript$txEnd,y0=1,y1=1,lwd=1)
arrows(x0=seq(transcript$txStart,transcript$txEnd-1000,1000),x1=seq(transcript$txStart+1000,transcript$txEnd,1000),y0=1,y1=1,lwd=1,angle=30,length=.05)
segments(x0=exon_starts,x1=exon_ends,y0=1,y1=1,lwd=10,lend=1)
segments(x0=transcript$cdsStart,x1=transcript$cdsEnd,y0=1,y1=1,lwd=20,lend=1)
mtext(side=3,text='PRNP',font=3)
landmarks = c(transcript$txStart, exon_ends[1], exon_starts[2], transcript$txEnd)
axis(side=1, at=landmarks, labels=formatC(landmarks, format='f', digits=0, big.mark=','), las=2, cex.axis=.6, lwd=0, lwd.ticks=1)
intron_length = exon_starts[2] - exon_ends[1]
cds_length= transcript$cdsEnd - transcript$cdsStart
text(x=exon_ends[1]+intron_length/2, y=1.2, pos=3, labels=paste(formatC(intron_length,format='f',digits=0,big.mark=','),'bp intron',sep=''), cex=.7)
text(x=transcript$cdsStart+cds_length/2, y=1.2, pos=3, labels=paste(formatC(cds_length,format='f',digits=0,big.mark=','),'bp\ncoding',sep=''), cex=.7, font=2)
dev.off()

