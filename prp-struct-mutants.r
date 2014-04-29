setwd('~/d/sci/003prigen/data/prp-struct-mutants')

# evaluate an expression stored as a string
# this just shortens the syntax which is otherwise eval(parse(text="mystring"))
# see http://stackoverflow.com/questions/1743698/r-eval-expression
evals = function(mystring) {
  return (eval(parse(text=mystring)))
}

# PRNP GenBank entry http://www.ncbi.nlm.nih.gov/nuccore/NG_009087.1
huprp = 'MANLGCWMLVLFVATWSDLGLCKKRPKPGGWNTGGSRYPGQGSPGGNRYPPQGGGGWGQPHGGGWGQPHGGGWGQPHGGGWGQPHGGGWGQGGGTHSQWNKPSKPKTNMKHMAGAAAAGAVVGGLGGYMLGSAMSRPIIHFGSDYEDRYYRENMHRYPNQVYYRPMDEYSNQNNFVHDCVNITIKQHTVTTTTKGENFTETDVKMMERVVEQMCITQYERESQAYYQRGSSMVLFSSPPVILLISFLIFLIVG'

# post-translational modifications
sp = 1:22
final = 23:230
gpi = 231:253

# Riek 1996 http://www.ncbi.nlm.nih.gov/pubmed/8700211
# Regions of secondary structure - translated from mouse numbering
b1 = 129:132
h1 = 145:155
b2 = 162:165
h2 = 180:194
h3 = 201:218

# Octapeptide repeats
opr = 60:91

# colors for secondary structure
bcolor = '#FF6103' # cadmium orange
hcolor = '#236B8E' # steelblue
ocolor = '#B87333' # copper
cleaved = '#C0C0C0' # silver
rest = '#EEC900' # gold2
dscolor = '#5E2605' #van dyke brown - for disulfide bonds
delcolor = '#000000' # black for deleted elements

# other colors
vcolor = '#C0C0C0' # color for vertical lines

k = rep(rest,253)
k[sp] = cleaved
k[gpi] = cleaved
k[opr] = ocolor
k[b1] = bcolor
k[b2] = bcolor
k[h1] = hcolor
k[h2] = hcolor
k[h3] = hcolor

aapch = '|' # square for present amino acids
delpch = 20

pch = rep(15,253)
pch[]

strsplit(huprp,split='')

plot(1:253,rep(1,253),pch=15,col=k,xlim=c(1,253))

prps = data.frame(name=character(), # name of mutant
                  height=integer(), # height at which to plot
                  deleted=character(), # positions deleted
                  produces=character(), # produces transmissible prions spontaneously?
                  accepts=character(), # accepts inoculated prions in vivo?
                  otherphen=character(), # any other spontaneous phenotype?
                  cite=character()) # citation
prps["wt",] = c("'Wild-type'",0,"254","no","yes","no","") # 254 is an ugly hack to get wild-type to plot properly
prps["Y145X",] = c("'Y145X'",0,"145:253","yes","","","Kitamoto 1993")
prps["Y163X",] = c("'Y163X'",0,"163:253","yes","","","Mead 2013")
prps["D178fs25X",] = c("'D178fs25X'",0,"178:253","yes","","","Matsuzono 2013")
prps["Y226X-Q227X",] = c("'Y226X, Q227X'",0,"226:253","yes","","","Jansen 2009")
prps["2-OPRD",] = c("'2-OPRD'",0,"80:91","","","","?")
prps["OPRI",] = c("'OPRI (various)'",0,"254","yes","","yes?","various")
prps["G29X",] = c("'Goat 32Stop (G29X)'",0,"29:253","no","","","Benestad 2012")
prps["PrPdNterm",] = c("expression(paste(Delta,'Nterm / ',Delta,'A /',Delta,'32-80'))",0,"32:81","no","yes","no","Fischer 1996")
prps["PrPdNco",] = c("expression(paste(Delta,'Nco / ',Delta,'B /',Delta,'69-84'))",0,"70:85","no","yes","no","Fischer 1996")
prps["PrPdelC",] = c("expression(paste(Delta,'C /',Delta,'32-93'))",0,"32:94","no","yes*","no","Shmerling 1998\nFlechsig 2000")
prps["PrPdelD",] = c("expression(paste(Delta,'D /',Delta,'32-106'))",0,"32:107","no","no","no","Shmerling 1998\nWeissmann 2003")
prps["PrPdelE",] = c("expression(paste(Delta,'E /',Delta,'32-121'))",0,"32:122","no","","yes","Shmerling 1998")
prps["PrPdelF",] = c("expression(paste(Delta,'F /',Delta,'32-134'))",0,"32:135","no","","yes","Radovanovic 2005")
prps["PrPdelCD",] = c("expression(paste(Delta,'CD /',Delta,'94-134'))",0,"95:135","no","","yes","Baumann 2007")
prps["PrPdelpHC",] = c("expression(paste(Delta,'pHC / ',Delta,'114-121'))",0,"115:122","no","","yes","Baumann 2007")
prps["PrPdCR",] = c("expression(paste(Delta,'CR'))",0,"106:126","no","","yes","Li 2007")
prps["C1",] = c("'C1'",0,"23:110","no","no","no","Westergard 2011")
prps["PrPd23-134",] = c("expression(paste(Delta,'23-134'))",0,"23:135","no","no","no","Westergard 2011")
prps["PrPd23-88",] = c("expression(paste(Delta,'23-88'))",0,"23:89","no","yes*","no","Supattapone 1999")
prps["PrPd23-88d95-107",] = c("expression(paste(Delta,'23-88',Delta,'95-107'))",0,"c(23:89,96:108)","no","no*","no","Muramoto 1997")
prps["PrPd23-88d108-121",] = c("expression(paste(Delta,'23-88',Delta,'108-121'))",0,"c(23:89,109:122)","no","no*","no","Muramoto 1997")
prps["PrPd23-88d122-140",] = c("expression(paste(Delta,'23-88',Delta,'122-140'))",0,"c(23:89,123:141)","no","no*","no","Muramoto 1997")
prps["PrPd23-88d141-221",] = c("expression(paste(Delta,'23-88',Delta,'141-221'))",0,"c(23:89,142:222)","","","","?")
prps["PrP106",] = c("expression(paste('PrP106'))",0,"c(23:89,142:177)","no","yes","no","Supattapone 1999")
prps["PrPd23-88d177-200",] = c("expression(paste(Delta,'23-88',Delta,'177-200'))",0,"c(23:89,178:201)","no","no*","yes","Muramoto 1997")
prps["PrPd23-88d201-217",] = c("expression(paste(Delta,'23-88',Delta,'201-217'))",0,"c(23:89,202:218)","no","no*","yes","Muramoto 1997")
prps["PrPd23-88C178A",] = c("expression(paste(Delta,'23-88','C178A'))",0,"c(23:89)","no","no*","no","Muramoto 1997")
prps["PrP144X",] = c("'144#'",0,"146:253","","","","Muramoto 1997")
prps["PrPd23-88-144X",] = c("expression(paste(Delta,'23-88','144#'))",0,"c(23:89,146:253)","","","","Muramoto 1997")
prps["PrPd144-227",] = c("expression(paste(Delta,'144-227'))",0,"146:230","","","","Muramoto 1997")
prps["cyPrP",] = c("'cyPrP'",0,"c(1:22,230:253)","no","no","yes","Ma 2002a")
prps["delGPI",] = c("expression(paste(Delta,'GPI'))",0,"231:253","yes","yes","no","Stohr 2011")


prps$height = seq(20/dim(prps)[1],20,20/dim(prps)[1])
increment = 20/dim(prps)[1]

png('prp-deletion-mutants.jpg',width=600,height=1200,res=300,pointsize=3)
par(mar=c(0,0,0,0))
plot(NA,NA,xaxt='n',yaxt='n',xlim=c(-100,320),ylim=c(0,20),xlab='',ylab='')
axis(side=1,at=c(1,23,50,100,150,200,230,253),
     labels=c(1,23,50,100,150,200,230,253),cex=.5,las=2)
text(c(mean(sp),mean(opr),mean(b1),mean(h1),mean(b2),
     mean(h2),mean(h3),mean(gpi)),rep(20,8),
     col=c(cleaved,ocolor,bcolor,hcolor,bcolor,
           hcolor,hcolor,cleaved),
     labels=c("SP","OR",expression(paste(beta,1)),
              expression(paste(alpha,1)),
              expression(paste(beta,2)),
              expression(paste(alpha,2)),
              expression(paste(alpha,3)),
              "GPI"))
text(c(260,280,300)+rep(15,3),c(20,20,20),labels=c('Produces','Accepts','Other'),srt=90,cex=.8,pos=3)
for (row in 1:dim(prps)[1]) {
    deleted = evals(prps$deleted[row])
    present = (1:253)[-deleted]
    ht = 20 - prps$height[row]
    text(-100,ht,labels=evals(prps$name[row]),pos=4) # name
    
    if (length(deleted) > 1) {
      points(deleted,rep(ht,length(deleted)),pch=delpch,col=delcolor)      
    }
    points(present,rep(ht,length(present)),pch=aapch,col=k[present],cex=2.2)
    text(260,ht,labels=prps$produces[row],pos=4) # produces prions
    text(280,ht,labels=prps$accepts[row],pos=4) # accepts prions
    text(300,ht,labels=prps$otherphen[row],pos=4) # other phenotype 
    
    # handle a few special cases
    if (rownames(prps)[row] == "PrPd23-88C178A") {
      points(c(179,179,214,214),c(ht,ht-increment/3,ht-increment/3,ht),type='l',col=dscolor,lwd=2)
      points((179+214)/2,ht-increment/3,pch=4,col=dscolor,cex=2)
    } else if (rownames(prps)[row] == "OPRI") {
      insertedrange = 91:(91+9*8)
      points(insertedrange,rep(ht+increment/3,length(insertedrange)),pch=15,col=ocolor,cex=1)
      points(91,ht+increment/3,pch=17,col='black')
    } else if (rownames(prps)[row] == "D178fs25X") {
      points(178:(178+25-1),rep(ht,25),pch=aapch,col=delcolor,cex=2.2)
    }
}
dev.off()


# now loop to create each one separately
for (row in 1:dim(prps)[1]) {
  pngname = paste(rownames(prps)[row],".png",sep="")
  png(pngname,width=400,height=30,res=300,pointsize=3)
  par(mar=c(0,0,0,0))
  plot(NA,NA,xaxt='n',yaxt='n',xlim=c(1,253),ylim=c(.20,.80),xlab='',ylab='',xaxs='i',bty='n')
  deleted = evals(prps$deleted[row])
  present = (1:253)[-deleted]
  ht = .50
  if (length(deleted) > 1) {
    points(deleted,rep(ht,length(deleted)),pch=delpch,col=delcolor)      
  }
  points(present,rep(ht,length(present)),pch='|',col=k[present],cex=5)
  
  # handle a few special cases
  if (rownames(prps)[row] == "PrPd23-88C178A") {
    points(c(179,179,214,214),c(ht,ht-increment/3,ht-increment/3,ht),type='l',col=dscolor,lwd=2)
    points((179+214)/2,ht-increment/3,pch=4,col=dscolor,cex=2)
  } else if (rownames(prps)[row] == "OPRI") {
    insertedrange = 91:(91+9*8)
    points(insertedrange,rep(ht+increment/2.5,length(insertedrange)),pch=15,col=ocolor,cex=1)
    points(91,ht+increment/2.5,pch=17,col='black')
  } else if (rownames(prps)[row] == "D178fs25X") {
    points(178:(178+25-1),rep(ht,25),pch=aapch,col=delcolor,cex=5)
  }
  
  dev.off()
}
# 
# row = 1
# par(mar=c(0,0,0,0))
# plot(NA,NA,xaxt='n',yaxt='n',xlim=c(-100,260),ylim=c(.25,.75),xlab='',ylab='')
# axis(side=1,at=c(1,23,50,100,150,200,230,253),
#      labels=c(1,23,50,100,150,200,230,253),cex=.5,las=2)
# deleted = evals(prps$deleted[row])
# present = (1:253)[-deleted]
# ht = .5
# text(-100,ht,labels=evals(prps$name[row]),pos=4,cex=1) # name
# points(deleted,rep(ht,length(deleted)),pch=delpch,col=delcolor,cex=3)
# points(present,rep(ht,length(present)),pch='|',col=k[present],cex=3)
# points(present,rep(ht,length(present)),pch='|',col=c(rep(cleaved,22),rep(rest,208),rep(cleaved,23)),cex=3)


