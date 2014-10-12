pdf('sporadic-genetic-acquired.pdf',width=8,height=6)
par(font=2,mar=c(1,1,1,1))
pie(c(15,84,1),col=c('orange','#4F63E2','red'),labels=c('genetic','sporadic','acquired'),cex=2)
dev.off()

