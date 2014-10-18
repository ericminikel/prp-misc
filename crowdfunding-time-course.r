data = read.table(header=FALSE,file=textConnection("
1   100.00
1   20.00
1   30.00
1   40.00
1   30.00
1   50.00
1   20.00
1   20.00
1   250.00
1   5.00
1   50.00
1   75.00
1   100.00
2   100.00
2   8.00
2   100.00
2   100.00
2   30.00
2   50.00
2   50.00
2   100.00
2   100.00
2   150.00
2   170.00
2   50.00
2   150.00
3   80.00
3   100.00
6   20.00
6   20.00
6   100.00
6   100.00
7   200.00
7   500.00
8   100.00
8   20.00
8   300.00
9   200.00
9   100.00
10  100.00
10  250.00
12  1000.00
12  40.00
13  25.00
15  120.00
15  30.00
16  200.00
17  500.00
17  25.00
17  50.00
20  20.00
21  6.00
22  50.00
22  20.00
23  50.00
26  50.00
27  100.00
28  40.00
29  150.00
29  50.00
29  25.00
30  50.00
30  50.00
30  50.00
31  100.00
32  37.00
32  10.00
32  150.00
32  30.00
33  20.00
33  25.00
33  100.00
33  100.00
33  150.00
33  25.00
33  100.00
33  30.00
34  50.00
34  40.00
34  50.00
34  20.00
34  15.00
35  1000.00
35  75.00
35  386.00
35  100.00
36  300.00
37  25.00
38  20.00
39  100.00
42  2633.00
43  40.00
44  3947.00
44  100.00
44  100.00
45  100.00
45  50.00
45  200.00
45  30.00
45  20.00
45  100.00
"))

colnames(data) = c('day','funds')
missing = which(!(1:45 %in% data$day))
add_in = cbind(missing,0)
colnames(add_in) = c('day','funds')
data = rbind(data,add_in)
data = data[with(data,order(day)),]

data$cumfunds = cumsum(data$funds)

goals = c(8000,12400,16800)


pdf('fundraising-time-course.pdf',width=8,height=6)
par(mar=c(6,6,3,6))
plot(NA,NA,xlim=c(0,45),ylim=c(0,17217),xaxs='i',yaxs='i',axes=FALSE,
    xlab='day',ylab='',main='45 days of fundraising for anle138b')
abline(h=c(0,goals),col='#777777')
points(data$day,data$cumfunds,type='l',lwd=5,col='#FF1493')
axis(side=1,at=(0:9)*5,labels=(0:9)*5,lwd=0,lwd.ticks=1)
axis(side=2,at=goals,labels=c('Goal 1','Goal 2','Goal 3'),las=1,lwd=0,lwd.ticks=0,cex.axis=.8)
axis(side=4,at=goals,labels=paste('$',prettyNum(goals,big.mark=',',scientific=FALSE),sep=''),
    lwd=0,lwd.ticks=0,las=1,cex.axis=.8,line=1)
dev.off()

