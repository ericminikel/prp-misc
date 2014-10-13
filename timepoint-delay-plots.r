
data = read.table(sep='|',skip=1,header=TRUE,textConnection("
class|treatment|dose|started|control_survival|treated_survival|citation
large molecule|pentosan polysulfate|460 ug/kg/day|7|47|122|Doh-Ura 2004
large molecule|pentosan polysulfate|460 ug/kg/day|21|47|97|Doh-Ura 2004
large molecule|pentosan polysulfate|460 ug/kg/day|35|47|80|Doh-Ura 2004
large molecule|pentosan polysulfate|460 ug/kg/day|42|47|47|Doh-Ura 2004
small molecule|anle138b|10 mg/day|0|180|355|Wagner 2013
small molecule|anle138b|5 mg/day|80|168|242|Wagner 2013
small molecule|anle138b|5 mg/day|120|172|224|Wagner 2013
small molecule|IND24|210 mg/kg/day|1|118|204|Berry 2013
small molecule|IND24|210 mg/kg/day|60|118|211|Berry 2013
small molecule|cpd-b|0.2% wt/wt in feed|0|63|174.5|Kawasaki 2007
small molecule|cpd-b|0.2% wt/wt in feed|35|63|117.2|Kawasaki 2007
small molecule|cpd-b|0.2% wt/wt in feed|49|63|88.7|Kawasaki 2007
genetic manipulation|Cre-MloxP||56|84|399|Mallucci 2003
genetic manipulation|Tet-off||98|150|430|Safar 2005
gene therapy|LV-siRNA||56|88|105|White 2008
gene therapy|AAV2-siRNA||50|137|120|Ahn 2014
gene therapy|ASO 771||1|136|193|Nazor-Friber 2012
"))

data$timepoint = data$started/data$control_survival
data$delay = data$treated_survival/data$control_survival - 1

data$color=''
data$color[data$treatment=='pentosan polysulfate'] = '#DB70DB'
data$color[data$treatment=='anle138b'] = '#39B7CD'
data$color[data$treatment=='IND24'] = '#59D7ED'
data$color[data$treatment=='cpd-b'] = '#79F7FD'
data$color[data$treatment=='Tet-off'] = '#FFD700'
data$color[data$treatment=='Cre-MloxP'] = '#FFF720'
data$color[data$treatment=='LV-siRNA'] = '#7B3F00'
data$color[data$treatment=='AAV2-siRNA'] = '#9B5F20'
data$color[data$treatment=='ASO 771'] = '#AB7F40'

data$pch = 20

data$delay[data$treatment=='Cre-MloxP'] = 2.4
data$pch[data$treatment=='Cre-MloxP'] = 17


i =2
# add in the treatment classes one by one
for (i in 1:4) {
    classes_to_plot = unique(data$class)[1:i]
    par(mar=c(4,5,3,2))
    plot(NA,NA,xlim=c(0,1),ylim=c(-.2,2.5),axes=FALSE,xaxs='i',yaxs='i',
        xlab='Timepoint in disease course\n(treatment started / control survival)',
        ylab='Delay of terminal illness\n(treated survival / control survival - 1)',
        main='Comparing therapeutic efficacy in prion mouse models')
    abline(h=0,col='#777777')
    abline(v=0,col='#777777')
    axis(side=1,at=(0:10)/10,labels=(0:10)/10,lwd=0,lwd.ticks=1)
    axis(side=2,at=(0:5),labels=paste((0:5)*100,'%',sep=''),lwd=0,lwd.ticks=1,las=1)
    for (tmt in unique(data$treatment[data$class %in% classes_to_plot])) {
        subset = data$treatment==tmt
        points(data$timepoint[subset],data$delay[subset],type='b',lwd=5,col=data$color[subset],pch=data$pch[subset])
        text(x=data$timepoint[subset][1]+.01,y=data$delay[subset][1],label=tmt,pos=4,col=unique(data$color[subset]))
    }
}


