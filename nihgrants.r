require(RXKCD)
require(tm)
require(wordcloud)
require(RColorBrewer)
require(sqldf)

# much help from this example:
# http://onertipaday.blogspot.com/2011/07/word-cloud-in-r.html

setwd('c:/sci/003prigen/analysis/nih')

# colors for plotting
color_usd = '#A4BD99' # http://www.colourlovers.com/palette/860881/the_color_of_money
burnt_sienna = '#9C3108' # http://www.colourlovers.com/color/9C3108/Burnt_Sienna
meyer_lemon = '#FFDE00' # http://www.colorcombos.com/color-schemes/2/ColorCombo2.html'
tangerine = '#F28500' # http://colors.findthedata.org/q/686/10856/What-is-the-hex-value-of-Tangerine
cherry = '#C40000' # http://www.colourlovers.com/color/C40000/cherry_red
mojo = '#B95835' # http://www.colorcombos.com/color-schemes/303/ColorCombo303.html

# how to pre-process NIH ProjectREPORTer downloads so that R can open them
# 1. open file in a text editor
# 2. remove the comma at the end of the 5th line (the column names)
# 3. convert end-of-line to Unix

# for original Active Projects download:
# grants = read.table('c:/sci/003prigen/analysis/nih/SR_23Jan14_010649_4704980.csv',header=TRUE,skip=4,sep=',',quote='"')
# for the "prion" all years search:
grants = read.table('c:/sci/003prigen/analysis/nih/SR_23Jan14_010649_4704980.csv',header=TRUE,skip=4,sep=',',quote='"')
# make sql-friendly column names
colnames(grants) = tolower(gsub("\\.","_",colnames(grants)))

# grants contain project abstracts only.
# for full text you have to FOIA:
# http://report.nih.gov/faq.aspx?sid=2
# "If you'd like more information than what is available in the 
# public domain via RePORTER, you can submit a written Freedom 
# of Information Act (FOIA) request for a copy of a specific 
# grant application to the NIH Institute or Center (IC) that 
# funded the grant. The NIH FOIA website at 
# http://www.nih.gov/icd/od/foia/index.htm lists the names 
# and addresses of FOIA coordinators for the various ICs."


# some basic cleanup
# remove punctuation so that UC,SF matches UCSF
grants$organization_name = gsub(",","",grants$organization_name)
# remove excess spaces
grants$organization_name = gsub("  "," ",grants$organization_name)

# how many have TSE in category?
sum(grepl('TSE',grants[,1]))
# only 660. 
# when did the grants with 'TSE' in category get awarded?
table(grants$fy[grep('TSE',grants[,1])])
# 2008 2009 2010 2011 2012 
# 126  152  135  138  109
# I see - they only started applying the lable in 2008. weird.
# also the category is assigned based on an automated text mining tool

colnames(grants)

# http://exporter.nih.gov/about.aspx
# RePORTER started counting intramural funding only from 2007 on

# Note: see column definitions here: http://exporter.nih.gov/about.aspx
# AFAICT if the Activity starts with 'Z' it is intramural
grants$intramural = sapply(grants$activity,substr,1,1) == 'Z'


# number of grants by year
sql_query = "
select   fy, count(*) n_grants
from     grants
where    fy < 2014 --  2014 not complete
group by 1
order by 1
;"
n_by_year = sqldf(sql_query)
png('n.grants.by.year.png',width=600,height=400)
plot(n_by_year$fy, n_by_year$n_grants, type='b', lwd=3, pch=15, col=burnt_sienna,
     ylim=c(0,max(n_by_year$n_grants)),
     ylab='Number of grants', xlab='Year',
     main='Number of funded NIH grants containing the keyword "prion", by year')
dev.off()

sum(n_by_year$n_grants) # 3017
sum(n_by_year$n_grants[n_by_year$fy > 1999]) # 2443

length(unique(grants$application_id)) # 2989

# mean value of grants by year
sql_query = "
select   fy, avg(fy_total_cost_by_ic) meanval
from     grants
where    fy > 1999 and fy < 2014 -- 1999 no money, 2014 not complete
group by 1
order by 1
;"
val_by_year = sqldf(sql_query)
png('meanval.grants.by.year.png',width=600,height=400)
plot(val_by_year$fy, val_by_year$meanval, type='b', lwd=3, pch=15, col=color_usd,
     ylim=c(0,max(val_by_year$meanval,na.rm=TRUE)),yaxt='n',
     ylab='Mean value', xlab='Year',xaxt='n',
     main='Mean value of funded NIH grants containing the keyword "prion", by year')
axis(side=1, at=2000:2013, labels=2000:2013, las=2, cex=.8)
axis(side=2, at=(0:5)*10^5, labels=format_k_usd((0:5)*10^5), las=2, cex=.8)
dev.off()



# number of Prusiner grants by year
sql_query = "
select   fy, count(*) n_grants
from     grants
where    fy < 2014 --  2014 not complete
and      contact_pi___project_leader like '%PRUSINER%'
group by 1
order by 1
;"
n_by_year_p = sqldf(sql_query)
# points(n_by_year_p$fy, n_by_year_p$n_grants, type='b', lwd=3, pch=15, col='blue')



# total NIH support by year
sql_query = "
select   fy, sum(fy_total_cost_by_ic) total_funds
from     grants
where    fy > 1999 and fy < 2014 -- budget not avail prior to 2000; 2014 not complete
group by 1
order by 1
;"
funds_by_year = sqldf(sql_query)
png('usd.grants.by.year.png',width=600,height=400)
plot(funds_by_year$fy, funds_by_year$total_funds, type='b', lwd=3, pch=15, col=color_usd,
     ylim=c(0,max(by_year$total_funds)),yaxt='n',
     ylab='Total value of all grants', xlab='Year',xaxt='n',
     main='Total value of NIH grants containing the keyword "prion", by year')
axis(side=1, at=2000:2013, labels=2000:2013, las=2, cex=.8)
axis(side=2, at=c(0,4e7,8e7,12e7), labels=format_m_usd(c(0,4e7,8e7,12e7)))
dev.off()

sum(funds_by_year$total_funds)
# 723420904

top_n_plus_other = function(groupbycol, sumcol, tablename, n, where='') {
  sql_query = paste(" select ",groupbycol,", total from (
  select   ",groupbycol,", sum(",sumcol,") total
  from     ",tablename,"
  ",where,"
  group by 1
  order by 2 desc
  limit ",n,")
  union all
  select   'all others', sum(",sumcol,") total
  from     ",tablename,"
  ",where,"
  and      ",groupbycol," not in ( select ",groupbycol," from (
    select   ",groupbycol,", sum(",sumcol,") total
    from     ",tablename,"
    ",where,"
    group by 1
    order by 2 desc
    limit ",n,"));",sep='')
  return (sqldf(sql_query))
}



format_m_usd = function(intgr) {
  return ( paste("$",round(intgr/(1e6)),"M",sep="") )  
}

format_k_usd = function(intgr) {
  return ( paste("$",round(intgr/(1e3)),"K",sep="") )  
}


top10pi = top_n_plus_other('contact_pi___project_leader','fy_total_cost_by_ic','grants',10,
                 where='where fy > 1999 and fy < 2014')
top10pi$pi_surname = sapply(sapply(top10pi$contact_pi___project_leader,strsplit,","),"[[",1)
png('top10.pi.png',width=600,height=400)
barplot(top10pi$total[10:1], names.arg=tolower(top10pi$pi_surname[10:1]),
        horiz=TRUE,col=meyer_lemon,border=NA,las=1,cex.names=.8,xaxt='n',
        main='top 10 NIH-funded PIs in grants containing the keyword"prion"\ntotal funding, 2000 to 2013')
axis(side=1, at=c(0,4e7,8e7,12e7), labels=format_m_usd(c(0,4e7,8e7,12e7)))
dev.off()

# Gary Griffiths @NHLBI: http://www.nih.gov/catalyst/2006/06.01.01/page1.html
# Lois Greene @ NHLBI: http://www.nhlbi.nih.gov/research/intramural/researchers/pi/greene-lois/

png('top10.pi.with.others.png',width=600,height=400)
barplot(top10pi$total[11:1], names.arg=tolower(top10pi$pi_surname[11:1]),
        horiz=TRUE,col=meyer_lemon,border=NA,las=1,cex.names=.8,xaxt='n',
        main='top 10 NIH-funded PIs in grants containing the keyword"prion"\ntotal funding, 2000 to 2013')
axis(side=1, at=(0:5)*10^8, labels=format_m_usd((0:5)*10^8))
dev.off()

top10org = top_n_plus_other('organization_name','fy_total_cost_by_ic','grants',10,
                            where='where fy > 1999 and fy < 2014')
top10org$org_shortname = c('UCSF','NIAID','CASE','NHLBI','NIDDK','SCRIPPS','WASH U','MRI','CHICAGO','HOUSTON','ALL OTHERS')
png('top10.org.png',width=600,height=400)
barplot(top10org$total[10:1], names.arg=tolower(top10org$org_shortname[10:1]),
        horiz=TRUE,col=tangerine,border=NA,las=1,cex.names=.8,xaxt='n',
        main='top 10 NIH-funded organizations in grants containing the keyword"prion"\ntotal funding, 2000 to 2013')
axis(side=1, at=c(0,4e7,8e7,12e7), labels=format_m_usd(c(0,4e7,8e7,12e7)))
dev.off()


top5act = top_n_plus_other(groupbycol='activity',
                 sumcol='fy_total_cost_by_ic',
                 tablename='grants',
                 n=5,
                 where="where fy > 1999 and fy < 2014")
top5act$activity[6] = 'OTHER'
png('top5.granttype.png',width=600,height=400)
barplot(top5act$total[6:1], names.arg=toupper(top5act$activity[6:1]),
        horiz=TRUE,col=cherry,border=NA,las=1,cex.names=.8,xaxt='n',
        main='NIH prion grants by grant type\ntotal funding, 2000 to 2013')
axis(side=1, at=(0:3)*10^8, labels=format_m_usd((0:3)*10^8))
dev.off()

top5ic = top_n_plus_other(groupbycol='administering__ic',
                 sumcol='fy_total_cost_by_ic',
                 tablename='grants',
                 n=5,
                 where="where fy > 1999 and fy < 2014")
top5ic$administering__ic[6] = 'OTHER'
png('top5.nih_ic.png',width=600,height=400)
barplot(top5ic$total[6:1], names.arg=toupper(top5ic$administering__ic[6:1]),
        horiz=TRUE,col=mojo,border=NA,las=1,cex.names=.8,xaxt='n',
        main='NIH prion grants by NIH institute/center\ntotal funding, 2000 to 2013')
axis(side=1, at=(0:3)*10^8, labels=format_m_usd((0:3)*10^8))
dev.off()


### a few other exploratory analyses that didn't wind up so interesting:

# the TSE category is fairly limited.
# for instance, only ~10M of Prusiner's 105M is included
top_n_plus_other(groupbycol='contact_pi___project_leader',
                 sumcol='fy_total_cost_by_ic',
                 tablename='grants',
                 n=10,
                 where="where fy > 1999 and fy < 2014 and 
                 nih_spending_categorization like '%Transmissible Spongiform Encephalopathy%'")

top_n_plus_other(groupbycol='contact_pi___project_leader',
                 sumcol='fy_total_cost_by_ic',
                 tablename='grants',
                 n=10,
                 where="where fy > 1999 and fy < 2014 and 
                 project_abstract like '%PrPSc%'")


top_n_plus_other(groupbycol='administering__ic',
                 sumcol='fy_total_cost_by_ic',
                 tablename='grants',
                 n=5,
                 where="where fy > 1999 and fy < 2014")


# word clouds
# concatenate all text fields and replace punctuation (except -) with spaces
# see http://stackoverflow.com/a/8698368 for trick to remove punctuation
grants$alltext = gsub("[^[:alnum:][:space:]-]"," ",paste(grants$project_abstract,
                                              grants$project_title,
                                              grants$public_health_relevance))

# need > 1 column to pass to the Corpus function, so add a blank column
grants$blank = ""

grants_usecols = grants[,c("alltext","blank")]
grants_corpus = Corpus(DataframeSource(grants_usecols))
grants_corpus = tm_map(grants_corpus, tolower)

# I find Rwordcloud's stop word list too short, so I use the MySQL
# stop word list from: http://dev.mysql.com/doc/refman/5.1/en/fulltext-stopwords.html
mysql_stopwords = read.table('mysql_stopwords.txt',quote='',comment.char='',sep='\t')$V1

grants_corpus = tm_map(grants_corpus, function(x) removeWords(x, mysql_stopwords))

tdm = TermDocumentMatrix(grants_corpus)

# color palette for wordclouds
pal = brewer.pal(9, "YlOrBr")
pal = pal[-(1:2)]

# now to develop contrast clouds

# freqmat: each cell represents the number of times each word is used in each grant
freqmat = as.matrix(tdm) 
freqmat = freqmat[rowSums(freqmat) > 20,] # ignore words with global freq < 10

# convert to how many _distinct_ grants used it, rather than total instances
# distmat: each cell is 1 if the word was used at least once in the grant, 0 otherwise
distmat = 1 * (freqmat > 0)

# get list of NIH agencies / institutes that have funded at least 100 grants
agencies = names(table(grants$funding_ic)[table(grants$funding_ic) > 100])
agencies = agencies[-which(agencies=='')] # remove the blank one

# initialize a matrix to hold the degree of term enrichment
# for every term-agency combination
agency_enrichment_matrix = matrix(nrow=dim(freqmat)[1], ncol=length(agencies))
rownames(agency_enrichment_matrix) = rownames(freqmat)
colnames(agency_enrichment_matrix) = agencies

# add a number to the numerator and denominator to avoid
# dividing by zero. this in effect amounts to saying,
# "we only care about words whose frequency is not only highly
# contrasting between agencies, but whose frequency is also 
# high enough in the higher agency that if it were used X times
# in the other agencies, it would still be highly contrasting"
dbzero_factor = 3

minfreq = 5 # minimum frequency in the agency in question to bother plotting

for (agency in agencies) {
  # compare to the other top agencies, not to ALL funding agencies
  other_agencies = agencies[-which(agencies==agency)]
  # for agencies, use distmat so that many repeated uses in a single grant do not
  # count excessively much.
  other_agencies_freq = rowSums(distmat[,grants$funding_ic %in% other_agencies])
  this_agency_freq = rowSums(distmat[,grants$funding_ic==agency])
  # agency_ratio is an adjustment for how much text there is for each agency
  agency_ratio = 
    sum(nchar(grants$alltext[grants$funding_ic %in% other_agencies])) /  
    sum(nchar(grants$alltext[grants$funding_ic == agency]))
  agency_enrichment = agency_ratio * (this_agency_freq + dbzero_factor) / (other_agencies_freq + dbzero_factor)
  agency_enrichment[this_agency_freq < minfreq] = 0 # apply minimum frequency
  agency_enrichment_matrix[,agency] = agency_enrichment
}

# here, agency_ratio is very low (~1.5) for NINDS
# apparently NINDS makes up about 40% of all text in all grants by these 4 agencies
# so it's very difficult for a word to be enriched in it



# remove the agencies' names themselves from the enrichment matrix
# for this analysis only, these should be considered as stopwords.
# if (any(rownames(agency_enrichment_matrix) %in% tolower(agencies))) {
#   agency_enrichment_matrix = agency_enrichment_matrix[
#     -which(rownames(agency_enrichment_matrix) %in% tolower(agencies)),]
# }

png('contrast.worcloud.4.agencies.png',width=1200,height=1200)
par(mfrow=c(2,2), oma=c(1,1,1,1))
for (agency in agencies) {
  wordcloud(rownames(agency_enrichment_matrix),agency_enrichment_matrix[,agency], 
            scale=c(6,.1),min.freq=8,max.words=500, random.order=F, 
            use.r.layout=TRUE,rot.per=0.0, colors=pal, vfont=c("serif","plain"))
}
mtext('NIA',side=3,adj=0,outer=TRUE,family='sans',font=2)
mtext('NIAID',side=3,adj=1,outer=TRUE,family='sans',font=2)
mtext('NIGMS',side=1,adj=0,outer=TRUE,family='sans',font=2)
mtext('NINDS',side=1,adj=1,outer=TRUE,family='sans',font=2)
dev.off()


for (agency in agencies) {
  png(paste("contrast.wordcloud.agency.",agency,".png",sep=""),height=600,width=600)
  wordcloud(rownames(agency_enrichment_matrix),agency_enrichment_matrix[,agency], 
            scale=c(6,.1),min.freq=8,max.words=500, random.order=F, 
            use.r.layout=TRUE,rot.per=0.0, colors=pal, vfont=c("serif","plain"))
  dev.off()
}


grants$contact_pi___project_leader[grep('1syn',grants$project_abstract)]
# Pamela J. McLean http://www.massgeneral.org/neurology/researcher_profiles/mcclean_pamela.aspx

grants$contact_pi___project_leader[grep('quinacrine',grants$project_abstract)]


### similar analysis but for top funded pis
# get list of recipient pis w/ at least 100 grants

pis = top10pi$contact_pi___project_leader[c(-4,-9,-10,-11)]
pis

# initialize a matrix to hold the degree of term enrichment
# for every term-pi combination
pi_enrichment_matrix = matrix(nrow=dim(freqmat)[1], ncol=length(pis))
rownames(pi_enrichment_matrix) = rownames(freqmat)
colnames(pi_enrichment_matrix) = pis

dbzero_factor = 3
minfreq = 5
for (pi in pis) {
  # compare to the other top pis, not to ALL funding PIs
  other_pis = pis[-which(pis==pi)]
  # for PIs, use freqmat, so if a PI uses a word 80 times in one grant application,
  # that counts as 80, rather than 1. (opposite of how this was done for agencies, above)
  other_pis_freq = rowSums(freqmat[,grants$contact_pi___project_leader %in% other_pis])
  this_pi_freq = rowSums(freqmat[,grants$contact_pi___project_leader==pi])
  # apply an adjustment for how much text by that PI there was in total
  pi_ratio = 
    sum(nchar(grants$alltext[grants$contact_pi___project_leader %in% other_pis])) /  
    sum(nchar(grants$alltext[grants$contact_pi___project_leader == pi]))
  pi_enrichment = pi_ratio * (this_pi_freq + dbzero_factor) / (other_pis_freq + dbzero_factor)
  pi_enrichment[this_pi_freq < minfreq] = 0
  pi_enrichment_matrix[,pi] = pi_enrichment
}

# note: I inspected the pi_ratio for these investigators and it is
# MUCH higher for Caughey and Chesebro than for the others, 
# probably because their intramural grants do not have abstracts
# in other words, there was much less text to work with,
# so far fewer words were found to be signficantly enriched

# remove the pis' names themselves from the enrichment matrix
# commented out because this ultimately proved unimportant
# if (any(rownames(pi_enrichment_matrix) %in% tolower(pis))) {
#   pi_enrichment_matrix = pi_enrichment_matrix[
#     -which(rownames(pi_enrichment_matrix) %in% tolower(pis)),]
# }



# somehow even with random.order=FALSE, this is not entirely deterministic
# the same words always appear, but if you re-run it their positions shift around
for (pi in pis) {
  surname = tolower(top10pi$pi_surname[top10pi$contact_pi___project_leader==pi])
  png(paste("contrast.wordcloud.pi.",surname,".png",sep=""),height=600,width=600)
  par(oma=c(1,0,0,0))
  wordcloud(rownames(pi_enrichment_matrix),pi_enrichment_matrix[,pi], 
            scale=c(6,.1),min.freq=8,max.words=500, random.order=F, 
            use.r.layout=TRUE,
            rot.per=0.0, colors=pal, vfont=c("serif","plain"))
  mtext(surname,side=1,outer=TRUE,family='sans',font=2)
  dev.off()
}


# spot check a few cases to be able to explain the logic by which words
# are sized.

prusiner = grants$contact_pi___project_leader == 'PRUSINER, STANLEY B'
sum(prusiner)
sum(freqmat['sha',prusiner])
sum(freqmat['sha',!prusiner])
# 72 vs. 0

sum(freqmat['sha',prusiner])  / sum(prusiner)
sum(freqmat['sha',!prusiner]) / sum(!prusiner)


sum(freqmat['quinacrine',prusiner]) / sum(prusiner)
sum(freqmat['quinacrine',!prusiner]) / sum(!prusiner)

pi_enrichment_matrix[c("quinacrine","sha"),]

gambetti = grants$contact_pi___project_leader == 'GAMBETTI, PIERLUIGI '
sum(freqmat['deals',gambetti])
sum(freqmat['deals',!gambetti])

sum(freqmat['examined',gambetti])
sum(freqmat['examined',!gambetti])

caughey = grants$contact_pi___project_leader == 'CAUGHEY, BYRON '
sum(caughey)
sum(freqmat['prp-res',caughey])
sum(freqmat['prp-res',!caughey])

# quic is not in here?
pi_enrichment_matrix['quic',] # not found
pi_enrichment_matrix['pmca',] # Gambetti and Soto both use this term


### similar analysis but for top funded organizations
# get list of recipient orgs w/ at least 100 grants
orgs = names(table(grants$organization_name)[table(grants$organization_name) > 50])
orgs = orgs[-which(orgs=='')] # remove the blank one

# initialize a matrix to hold the degree of term enrichment
# for every term-org combination
org_enrichment_matrix = matrix(nrow=dim(freqmat)[1], ncol=length(orgs))
rownames(org_enrichment_matrix) = rownames(freqmat)
colnames(org_enrichment_matrix) = orgs

# add a number to the numerator and denominator to avoid
# dividing by zero. this in effect amounts to saying,
# "we only care about words whose frequency is not only highly
# contrasting between orgs, but whose frequency is also 
# high enough in the higher org that if it were used X times
# in the other orgs, it would still be highly contrasting"
dbzero_factor = 1
for (org in orgs) {
  # compare to the other top orgs, not to ALL funding orgs
  other_orgs = orgs[-which(orgs==org)]
  other_orgs_freq = (rowSums(freqmat[,grants$organization_name %in% other_orgs]) + dbzero_factor) /
    sum(grants$organization_name %in% other_orgs)
  this_org_freq = (rowSums(freqmat[,grants$organization_name==org]) + dbzero_factor) /
    sum(grants$organization_name==org)
  org_enrichment = this_org_freq / other_orgs_freq
  org_enrichment_matrix[,org] = org_enrichment
}

# remove the orgs' names themselves from the enrichment matrix
# for this analysis only, these should be considered as stopwords.
if (any(rownames(org_enrichment_matrix) %in% tolower(orgs))) {
  org_enrichment_matrix = org_enrichment_matrix[
    -which(rownames(org_enrichment_matrix) %in% tolower(orgs)),]
}

for (org in orgs) {
  png(paste("contrast.wordcloud.org.",org,".png",sep=""),height=600,width=600)
  wordcloud(rownames(org_enrichment_matrix),org_enrichment_matrix[,org], 
            scale=c(8,.3),min.freq=1,max.words=100, random.order=F, 
            use.r.layout=TRUE,
            rot.per=0.0, colors=pal, vfont=c("serif","plain"))
  dev.off()
}



# are there any words used almost exclusively by one investigator?
n_investigators = numeric(dim(distmat)[1])
for (i in 1:dim(distmat)[1]) {
  n_investigators[i] = length(unique(grants$contact_pi___project_leader[distmat[i,]==1]))
}
frq_invest = data.frame(word=rownames(distmat),
                              freq=rowSums(freqmat),
                              n_investigators=n_investigators)
frq_invest[frq_invest$freq > 80 & frq_invest$n_investigators < 3,]
# that wasn't very interesting, not going to mention in post

# are there words whose frequency has increased or decreased a lot over time?
# create a matrix for proportion of grants mentioning a term at least once each year
year_matrix = matrix(nrow=dim(distmat)[1], ncol=length(1990:2013))
rownames(year_matrix) = rownames(distmat)
year = 1990:2013
colnames(year_matrix) = as.character(year)
for (y in 1990:2013) {
  year_matrix[,as.character(y)] = rowSums(distmat[,grants$fy==y])  # / sum(grants$fy==y) # uncomment to do as *proportion* of grants
}





dz_terms = c('cjd','alzheimer','parkinson','als','huntington','tau')
used_dz_terms = dz_terms[dz_terms %in% rownames(year_matrix)]
dz_colors = c(rainbow(v=.6,n=length(used_dz_terms)))
png('human.dz.keywords.by.year.png',width=600,height=400)
par(mar=c(5,5,5,6))
plot(NA, NA, xlim=c(1997,2013), ylim=c(0,90),
     yaxt='n', ylab='Number of "prion" grants containing term',
     xaxt='n', xlab='Year',main="Human disease keywords by year")
axis(side=2,at=(0:10)*10,labels=(0:10)*10)
axis(side=1,at=1997:2013,labels=1997:2013,las=2,cex=.6)
for (i in 1:length(used_dz_terms)) {
  points(1997:2013, year_matrix[used_dz_terms[i],as.character(1997:2013)], 
         type='l', lwd=3, col=dz_colors[i])
}
mtext(side=4,at=year_matrix[used_dz_terms,'2013'],
      text=paste("  ",used_dz_terms),las=2,cex=.7,
      col=dz_colors,font=2,family='sans')
# legend('topleft',used_dz_terms,col=dz_colors,pch=NA,lwd=3)
dev.off()



### other code not currently in use for blog post


terms = c('yeast','mouse','hamster','patient')
used_terms = terms[terms %in% rownames(year_matrix)]
trm_colors = c(rainbow(v=.6,n=length(used_terms)))
png('keywords.by.year.1.png',width=600,height=400)
plot(NA, NA, xlim=c(1997,2013), ylim=c(0,.25),
     yaxt='n', ylab='Percent of "prion" grants containing term',
     xlab='Year',main="Keywords by year")
axis(side=2,at=(0:5)*.05,labels=paste((0:5)*5,"%"))
for (i in 1:length(used_dz_terms)) {
  points(1997:2013, year_matrix[used_terms[i],as.character(1997:2013)], 
         type='l', lwd=3, col=trm_colors[i])
}
legend('topright',used_terms,col=trm_colors,pch=NA,lwd=3)
dev.off()

# look for words with a significant slope up or down in usage
timeseries = data.frame(pval=numeric(), slope=numeric(),
                        avfreq=numeric())
for (word in rownames(year_matrix)) {
  wordfreq = year_matrix[word,]
  m = lm(wordfreq ~ year)
  pval = as.numeric(summary(m)$coefficients[2,4])
  slope = as.numeric(summary(m)$coefficients[2,1])
  avfreq = mean(wordfreq)
  timeseries = rbind(timeseries,c(pval,slope,avfreq))
}
colnames(timeseries) = c("pval","slope","avfreq")
timeseries$word = rownames(year_matrix)
# not very interesting


# tried to multiply budget by number of years, not clear if this is correct or not.
grants$budget_start_date[1:10]
strptime(grants$budget_start_date[1:10],format="%d-%b-%Y")
difftime(strptime(grants$budget_start_date[1:10],format="%d-%b-%Y",tz=""),
         strptime(grants$budget_end_date[1:10],format="%d-%b-%Y",tz=""),
         units="days")/365

# compute duration of funding period
grants$duration_y = as.numeric(difftime(
  strptime(grants$budget_end_date,format="%d-%b-%Y",tz=""),
  strptime(grants$budget_start_date,format="%d-%b-%Y",tz=""),
  units="days")/365.25)

hist(grants$duration_y)

# total cost over life of grant budget
grants$lifetime_cost = grants$fy_total_cost_by_ic * grants$duration_y
# Note: see column definitions here: http://exporter.nih.gov/about.aspx
# as far as I can tell, subproject budgets are reported twice,
# once in their own row and once lumped into the overall project's
# budget. Therefore we only need to count the one column
# grants$fy_total_cost_by_ic in order to capture the whole budget.

top_n_plus_other(groupbycol='contact_pi___project_leader',
                 sumcol='lifetime_cost',
                 tablename='grants',
                 n=10,
                 where="where fy > 1999 and fy < 2014")

top_n_plus_other(groupbycol='organization_name',
                 sumcol='lifetime_cost',
                 tablename='grants',
                 n=10,
                 where="where fy > 1999 and fy < 2014")


top_n_plus_other(groupbycol='contact_pi___project_leader',
                 sumcol='lifetime_cost',
                 tablename='grants',
                 n=10,
                 where="where fy > 1999 and fy < 2014
                 and intramural")
# darn, no costs are listed for intramural grants
table(grants$fy[grants$intramural])
# sketchy: intramural projects are only listed 
# for 1991-1999 and 2007-2013. None from 2000-2006
#1991 1992 1993 1994 1995 1996 1997 1998 1999 2007 2008 2009 2010 2011 2012 2013 
#1    6    3    1    5    6   15   15   13   26   29   34   27   25   24    4

