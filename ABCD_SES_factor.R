rm(list=ls())
source("setup.R")
library(psych)
library(dplyr)

mergecols = c("subjectkey","eventname")

#build ADI variable
abcd_rhds = read.abcd(file.path(ABCDDataDir,"abcd_rhds01.txt"))
abcd_rhds = abcd_rhds[abcd_rhds$eventname=="baseline_year_1_arm_1",c(mergecols,"reshist_addr1_adi_wsum")]

pdem = read.abcd(file.path(ABCDDataDir,"pdem02.txt"))

#adjust education in approximate years of education
#0-12 = 12
#13-14 = 12
#15-17 = 14
#18 = 16
#19 = 18
#20 = 20
#21 = 22
pdem$prnt_ed = recode(as.numeric(pdem$demo_prnt_ed_v2),'13'=12,'14'=12,'15'=14,'17'=14,'18'=16,'19'=18,'21'=22)
pdem$prtnr_ed = recode(as.numeric(pdem$demo_prtnr_ed_v2),'13'=12,'14'=12,'15'=14,'17'=14,'18'=16,'19'=18,'21'=22)
pdem$prnt_ed = na_if(pdem$prnt_ed,777)
pdem$prtnr_ed = na_if(pdem$prtnr_ed,777)
pdem$prnt_ed = na_if(pdem$prnt_ed,999)
pdem$prtnr_ed = na_if(pdem$prtnr_ed,999)

pdem$EdYearsHighest = apply(pdem[,c("prnt_ed","prtnr_ed")],1,max,na.rm=T)
pdem$EdYearsHighest = na_if(pdem$EdYearsHighest,-Inf)
pdem$EdYearsAverage = apply(pdem[,c("prnt_ed","prtnr_ed")],1,mean,na.rm=T)
pdem$EdYearsAverage[is.nan(pdem$EdYearsAverage)] = NA

#calculate income and midpoints
breaks = c(0,5000,12000,16000,25000,35000,50000,75000,100000,200000,Inf)
midpoints = (breaks[-1] + breaks[-11]) / 2
midpoints[length(midpoints)] = 200000

#due to inconsistent missing data, use combined if it exists, if not check for each income
#to calculate combined, since there are some cases where combined is NA but one or both 
#incomes are filled in
#if both are available, take midpoint of each, combine them, and then use the midpoint of the new bin

pdem$comb_inc = na_if(pdem$demo_comb_income_v2,777)
pdem$comb_inc = na_if(pdem$comb_inc,999)
pdem$prnt_inc = na_if(pdem$demo_prnt_income_v2,777)
pdem$prnt_inc = na_if(pdem$prnt_inc,999)
pdem$prtnr_inc = na_if(pdem$demo_prtnr_income_v2,777)
pdem$prtnr_inc = na_if(pdem$prtnr_inc,999)

pdem$comb_inc_mp = midpoints[pdem$comb_inc]
pdem$prnt_inc_mp = midpoints[pdem$prnt_inc]
pdem$prtnr_inc_mp = midpoints[pdem$prtnr_inc]

pdem$comb_inc_mp_backup = midpoints[cut(rowSums(cbind(pdem$prnt_inc_mp,pdem$prtnr_inc_mp),na.rm=T),breaks,right=F,labels=F)]
pdem$comb_inc_mp_backup[is.na(pdem$prnt_inc_mp) & is.na(pdem$prtnr_inc_mp)] = NA

pdem$IncCombinedMidpoint = pdem$comb_inc_mp
pdem$IncCombinedMidpoint[is.na(pdem$comb_inc_mp)] = pdem$comb_inc_mp_backup[is.na(pdem$comb_inc_mp)]


#household size
#lpds = read.abcd(file.path(ABCDDataDir,"abcd_lpds01.txt"))
#fam_roster_2c_v2 through 15c
#1 = Husband or wife
#2 = Son-in-law or daughter-in-law
#3 = Biological son or daughter
#4 = Other relative
#5 = Adopted son or daughter
#6 = Roomer or boarder
#7 = Stepson or stepdaughter
#8 = Housemate or roommate 
#9 = Brother or sister
#10 = Unmarried partner
#11 = Father or mother
#12 = Foster child
#13 = Grandchild
#14 = Other nonrelative
#15 = Parent-in-law
relidx = grepl("fam_roster_.*c_v2",names(pdem))
pdem$adult = 1+apply(pdem[,relidx],1,function(x) sum(x %in% c(1,2,4,6,8,9,10,11,14,15)))
pdem$child = apply(pdem[,relidx],1,function(x) sum(x %in% c(3,5,7,12,13)))

#Income-to-needs ratio was calculated as the median value of the income band divided by the federal poverty line for the respective household size. 
#federal poverty line is 12880 for 1 person plus 4540 for each additional person (or 8340 + 4540*N)
#https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2021-poverty-guidelines
pdem$povertyline = 8340 + (pdem$adult + pdem$child)*4540
pdem$Income2Needs = pdem$IncCombinedMidpoint / pdem$povertyline

cols = c(mergecols,"Income2Needs","IncCombinedMidpoint","EdYearsHighest","EdYearsAverage")
pdem = pdem[,cols]

ses = multi.merge(pdem,abcd_rhds,by=mergecols)

good = !is.na(ses$EdYearsAverage) & !is.na(ses$Income2Needs) & !is.na(ses$reshist_addr1_adi_wsum)

mat = ses[good,c("EdYearsAverage","Income2Needs","reshist_addr1_adi_wsum")]
zmat = scale(mat)

f = fa(zmat)
fScores = factor.scores(zmat,f)$scores

ses$ses_fac = NA
ses$ses_fac[good] = fScores

write.csv(ses,"Data/ABCD_ses.csv",row.names=F,na="NaN")


















