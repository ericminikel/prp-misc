setwd('~/d/sci/src/prp-misc')
require(sqldf)
qs = read.table('queues.txt')

colnames(qs) = c('DAYNO','DOW','MONTH','DAY','TIME','TZ','YEAR','QUEUE_NAME','PRIO','STATUS','MAX','JL_U','JL_P','JL_H','NJOBS','PEND','RUN','SUSP')
qs = qs[qs$QUEUE_NAME != 'QUEUE_NAME',]
qs$NJOBS = as.integer(qs$NJOBS)

q_by_day = sqldf("
select   DAYNO, QUEUE_NAME, AVG(NJOBS) MEAN_NJOBS
from     qs
group by 1, 2
;")

total_by_day = sqldf("
select   DAYNO, SUM(MEAN_NJOBS) TOTAL_JOBS
from     q_by_day
-- where    QUEUE_NAME like '%week%'
-- and      DAYNO > 275
group by 1
;")


plot(NA,NA,xlim=c(275,295),ylim=c(0,150000),axes=FALSE,
     ylab='total jobs in the Broad LSF queues',xlab='',
     main='demand for Broad compute time vs. proximity to ASHG')
rect(xleft=291,xright=295,ybottom=0,ytop=200000,col='#EEEEEE',border=NA)
points(total_by_day$DAYNO,total_by_day$TOTAL_JOBS,type='l',lwd=20,col='#FF3030')
axis(side=1,at=c(276,281,286,291),labels=c("t - 15 days","t - 10 days","t - 5 days","first day of ASHG"),lwd=0,lwd.ticks=1)
axis(side=2,at=(0:3)*50000,labels=paste((0:3)*50,"K",sep=''),las=1,lwd=0,lwd.ticks=1)
text(x=293,y=10000,pos=3,label='ASHG\nOct18-22, 2014',col='#666666')
text(x=290,y=139000,pos=2,label='Peak: 139K jobs at t - 1 day ---',col='#FF3030')
