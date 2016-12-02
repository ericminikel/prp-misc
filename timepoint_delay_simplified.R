options(stringsAsFactors=FALSE)
setwd('~/d/sci/src/prp-misc')
percent = function(proportion,digits=2) {
  return ( gsub(' ','',paste(formatC(proportion*100, digits=digits, format='fg'),"%",sep="") ) )
}

data = read.table(textConnection("
compound	mouse	treatment_start_dpi	control_endpoint_dpi	treatment_endpoint_dpi
IND24	wt	-14	118	452
IND24	wt	1	118	204
IND24	wt	34	118	202
IND24	wt	46	118	197
IND24	wt	60	118	211
IND24	wt	61	118	221
IND24	wt	74	118	192
IND24	wt	75	118	162
IND24	wt	77	118	208
IND24	wt	90	118	118
#IND24	tg	1	51	112
#IND24	tg	15	51	122
#IND24	tg	26	51	156
#IND24	tg	40	51	64
anle138b	wt	0	180	355
anle138b	wt	80	168	242
anle138b	wt	120	172	224
cpd-b	tg	0	63	174
cpd-b	tg	35	63	117
cpd-b	tg	49	63	89
PPS	tg	7	47	122
PPS	tg	21	47	97
PPS	tg	35	47	80
PPS	tg	42	47	47
"),sep='\t',header=TRUE,comment.char='#')

data$timepoint = data$treatment_start_dpi / data$control_endpoint_dpi
data$delay = data$treatment_endpoint_dpi / data$control_endpoint_dpi - 1

k_ind24 = '#FF2015'
k_anle138b = '#0001CD'
k_cpdb = '#FF9912'
k_pps = '#7D26CD'

data$color = ''
data$color[data$compound=='IND24'] = k_ind24
data$color[data$compound=='anle138b'] = k_anle138b
data$color[data$compound=='cpd-b'] = k_cpdb
data$color[data$compound=='PPS'] = k_pps

pdf('figures/timepoint_delay_simplified.pdf',width=8, height=4.5)
par(mar=c(4,5,4,4)) 
plot(NA, NA, xlab='', ylab='', xlim=c(-.15,1), ylim=c(0,3.5), yaxs='i', axes=FALSE)
abline(h=0, lwd=2, col='#000000')
abline(h=1:3, lwd=.5, col='#888888')
abline(v=0, lwd=1, col='#000000')
axis(side=1, at=(-1:10)/10, labels=percent((-1:10)/10), lwd=0, lwd.ticks=1, cex.axis=1)
axis(side=2, at=(0:6)/2, labels=paste((0:6)/2+1,'x',sep=''), lwd=0, lwd.ticks=.5, cex.axis=.9, las=2)
for (compound in c('anle138b','cpd-b','PPS','IND24')) {
  points(data$timepoint[data$compound==compound], data$delay[data$compound==compound], type='b', lwd=3, pch=20, col=data$color[data$compound==compound], cex=2)
}
mtext(side=1, text='Timepoint when continuous treatment initiated', font=2, line=2.5, cex=.9)
mtext(side=2, text='Survival time relative to controls', font=2, line=3.5, cex=.9)
title('Time of intervention versus survival outcome for antiprion compounds', cex=.9)
dev.off()