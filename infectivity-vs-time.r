
# Fig 2, Kimberlin & Walker 1986 http://www.ncbi.nlm.nih.gov/pubmed/3080549
kw1986_dpi = c(0,8,15,30,42,58,70,84,98)
kw1986_titer = c(3.9,2.2,3.5,3.9,5.5,6.5,7.3,8.1,8.8)
kw1986_onset = 88
kw1986_color = '#FF7216'

# Table 1, Baringer 1983 http://www.ncbi.nlm.nih.gov/pubmed/6411868
b1983_dpi = c(0,48,71)
b1983_titer = c(7.0,mean(c(8.9,9.5,9.2,9.5,9.1,9.3,8.6)),mean(c(9.4,9.8,9.8,10.0,9.4,9.8,9.7)))
b1983_onset = 60
b1983_color = '#385E0F'

# Table 3, Bueler 1993 http://www.ncbi.nlm.nih.gov/pubmed/8100741
b1993_dpi = c(0,4,14,56,84,140,168)
b1993_titer = c(8.5,1.5,1.5,5.4,6.8,8.6,8.1)
b1993_onset = 140
b1993_color = '#39B7CD'

# Text, Prusiner 1982 http://www.ncbi.nlm.nih.gov/pubmed/6801762
p1982_dpi = c(0,1,50,70)
p1982_titer = c(7,2,9,9)
p1982_onset = 60
p1982_color = '#CCCCCC'

plot(NA,NA,xlim=c(0,1.25),ylim=c(0,10),axes=FALSE,xlab='',ylab='',xaxs='i',yaxs='i')
points(kw1986_dpi/kw1986_onset,kw1986_titer,type='b',pch=20,lwd=4,col=kw1986_color)
points(b1983_dpi /b1983_onset,b1983_titer  ,type='b',pch=20,lwd=4,col=b1983_color)
points(b1993_dpi /b1993_onset,b1993_titer  ,type='b',pch=20,lwd=4,col=b1993_color)
points(p1982_dpi /p1982_onset,p1982_titer  ,type='b',pch=20,lwd=4,col=p1982_color,lty=3)
axis(side=1,at=c(0,1,1.2),labels=c('inoculation','clinical onset',''),lwd.ticks=c(1,1,0))
axis(side=2,at=0:10,labels=0:10,las=1)
mtext(side=1,text='time in disease course (normalized)',line=3)
mtext(side=2,text='log10(ID50)',line=3)
mtext(side=3,text='Does infectivity plateau at clinical onset of prion disease?')
legend('bottomright',bty='n',
    c('Kimberlin & Walker 1986 Fig 2','Baringer 1983 Table 1','Bueler 1993 Table 3','Prusiner 1982 Text'),
    col=c(kw1986_color,b1983_color,b1993_color,p1982_color),lwd=4,lty=c(1,1,1,3))

