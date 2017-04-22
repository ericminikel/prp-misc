options(stringsAsFactors=FALSE)
setwd('~/d/sci/src/prp-misc')

mutprev = read.table('data/all_mutation_prevalence.tsv',sep='\t',header=T)
mutclas = read.table('data/mutation_classification.tsv',sep='\t',header=T)
muts = mutclas[,c('variant','segregation','de_novo','homozygote_observed','enrichment')]
muts$classification = 'none'
muts$classification[muts$homozygote_observed=='x' | muts$enrichment=='x'] = 'increased'
muts$classification[muts$segregation=='x' | muts$de_novo == 'x'] = 'high'
muts$prev = 0
muts$prev = mutprev$case_count[match(muts$variant,mutprev$variant)]

# much of this code is adapted from https://github.com/ericminikel/prp-misc/blob/master/prp-struct-mutants.r

# post-translational modifications
sp = 1:22
final = 23:230
gpi = 231:253

# Zahn 2000 - regions of secondary structure
b1 = 128:131
h1 = 144:154
b2 = 161:164
h2 = 173:194
h3 = 200:228

# Octapeptide repeats
opr = 51:90

# colors for secondary structure
bcolor = '#FF6103' # cadmium orange
hcolor = '#236B8E' # steelblue
ocolor = '#B87333' # copper
cleaved = '#909090' # dark silver
rest = '#EEC900' # gold2

ss_labels = data.frame(mids=c(11.5,70.5,129.5,149,162.5,183.5,213.5,242),
                       labels=c('signal\npeptide','octarepeats','sheet 1','helix 1','sheet 2','helix 2','helix 3','GPI anchor'))

nres = 253

huprp_string = 'MANLGCWMLVLFVATWSDLGLCKKRPKPGGWNTGGSRYPGQGSPGGNRYPPQGGGGWGQPHGGGWGQPHGGGWGQPHGGGWGQPHGGGWGQGGGTHSQWNKPSKPKTNMKHMAGAAAAGAVVGGLGGYMLGSAMSRPIIHFGSDYEDRYYRENMHRYPNQVYYRPMDEYSNQNNFVHDCVNITIKQHTVTTTTKGENFTETDVKMMERVVEQMCITQYERESQAYYQRGSSMVLFSSPPVILLISFLIFLIVG'
huprp = strsplit(huprp_string,split='')[[1]]

parms = data.frame(codon=1:nres, col=rep(rest,nres))
parms$col[parms$codon %in% c(sp, gpi)] = cleaved
parms$col[parms$codon %in% c(opr)] = ocolor
parms$col[parms$codon %in% c(h1,h2,h3)] = hcolor
parms$col[parms$codon %in% c(b1,b2)] = bcolor

# colors for classification
classifications = data.frame(shortcode=c('high','increased','none'),
                             col=c('#A70700','#A707A7','#979797'),
                             display=c('evidence for high penetrance','evidence for increased risk','no evidence for increased risk'))
muts$col = classifications$col[match(muts$classification,classifications$shortcode)]

# cex values for text
cex_vals = data.frame(shortcode=c('zero','ten','hundred'),
                      display=c('0-9','10-99','100+'),
                      cex=c(1.2,1.8,3))
muts$cex = 1
muts$cex[muts$prev < 10] = cex_vals$cex[cex_vals$shortcode=='zero']
muts$cex[muts$prev >= 10 & muts$prev < 100] = cex_vals$cex[cex_vals$shortcode=='ten']
muts$cex[muts$prev >= 100] = cex_vals$cex[cex_vals$shortcode=='hundred']

muts$residue = as.integer(gsub('[^0-9]*','',muts$variant))
muts$residue[grepl('OPR',muts$variant)] = seq(51, 90, length.out=sum(grepl('OPR',muts$variant)))
muts$residue[muts$variant=='D178Efs25X'] = 178

png('figures/prnp_variants_by_evidence.png',width=1200,height=1600)
par(mar=c(1,6,4,1))
plot(NA,NA,xlim=c(1.5,3.5),ylim=c(nres+2,-1),axes=FALSE,ann=FALSE,xaxs='i',yaxs='i')
for (i in 1:nres) {
  rect(xleft=1.5,xright=2,ybottom=i,ytop=i+1,border=NA,col=parms$col[i])
  # adding text for every residue is too small to read unless you go to height=4000px and text(cex=1.2)
  # text(x=1,y=i+.5,pos=4,family='mono',font=2,labels=huprp[i],cex=1.2)
}

text(x=1.75,y=ss_labels$mids,labels=ss_labels$labels,col='white',font=2,cex=1.4)

axis(side=2, at=c(1,50,100,150,200,253), lwd=0, lwd.ticks=1, las=2,cex.axis=2)
mtext(side=2, line=2.5, text='PRNP codon number', cex=2)

mtext(side=3, line=1, text='Reportedly pathogenic variants in PRNP', font=2, cex=4)

nmut = nrow(muts)
last_residue = 0
offset_from = 0
rectangle_longitude = 2
starting_longitude = 2.1
longitude_increment = .1
max_longitude = 5
large_text_longitude = 3
longitude = starting_longitude
for (j in 1:nmut) {

  # figure out how far to the right to print text
  if (muts$residue[j] - last_residue < 3 & muts$residue[j] - offset_from < 8 & longitude < max_longitude) {
    longitude = longitude + longitude_increment * mean(c(muts$cex[j], muts$cex[j-1]))
  } else {
    longitude = starting_longitude
    offset_from = muts$residue[j]
  }
  
  if (muts$residue[j] != last_residue) {
    segments(x0=rectangle_longitude,x1=longitude,y0=muts$residue[j],y1=muts$residue[j],lwd=.5)
  }
  text(x=longitude, y=muts$residue[j], labels=muts$variant[j], col=muts$col[j], cex=muts$cex[j], font=2, pos=4)
  last_residue = muts$residue[j]
}

legend(x=3.5,xjust=1,y=1,legend=classifications$display,col=classifications$col,text.col=classifications$col,pch=15,cex=2,title='literature annotation',title.col='black')
legend(x=3.5,xjust=1,y=25,legend=cex_vals$display,pt.cex=cex_vals$cex,pch=20,col='black',title='prevalence in cases',cex=2)
dev.off()
