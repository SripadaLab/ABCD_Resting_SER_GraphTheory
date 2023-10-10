rm(list=ls())
source("setup.R")

mergecols = c("subjectkey","eventname")

#load family data
fam = read.abcd(file.path(ABCDDataDir,"acspsw03.txt"))
fam = fam[,c(mergecols,"rel_family_id")]

#load site data
site= read.abcd(file.path(ABCDDataDir,"abcd_lt01.txt"))
site = site[,c(mergecols,"site_id_l")]

#load meanFD data and DOF totals
motion = read.csv(file.path(DataDir,"meanfd.csv"))
motion$dofavg = motion$dof/motion$runs
motion$censoredavg = motion$censored/motion$runs
motion$subjectkey = gsub("NDAR","NDAR_",motion$subjectkey)
motion$eventname = "baseline_year_1_arm_1"

#generate nuisance covariates from raw ABCD files
pdem = read.abcd(file.path(ABCDDataDir,"pdem02.txt"))
cols = c(mergecols,"interview_age","sex","demo_sex_v2",
         "demo_comb_income_v2","demo_prnt_marital_v2",
         "demo_prnt_ed_v2","demo_prtnr_ed_v2","demo_ethn_v2",
         paste0("demo_race_a_p___",c(10:25,77,99)))
pdem = pdem[,cols]
pdem$re_white = as.numeric(pdem$demo_race_a_p___10==1)
pdem$re_black = as.numeric(pdem$demo_race_a_p___11==1)
pdem$re_hisp = as.numeric(pdem$demo_ethn_v2==1)
pdem$re_hisp[is.na(pdem$re_hisp)] = 0

pdem$re_asian = as.numeric(rowSums(pdem[,c(paste0("demo_race_a_p___",c(14:24)))])>0)
pdem$re_other = as.numeric(rowSums(pdem[,c(paste0("demo_race_a_p___",c(12:13,25)))])>0)

temp = with(pdem,re_asian + 2*re_black + 4*re_hisp + 8*re_other + 16*re_white)
temp[temp==1] = "Asian"
temp[temp==2] = "Black"
temp[temp==4] = "Hispanic"
temp[temp==8] = "Other"
temp[temp==16] = "White"
b=1:31
d = c(1:31)[bitwAnd(b,4)>0]
temp[temp %in% d] = "Hispanic"
temp[!(temp %in% c("Asian","Black","Hispanic","Other","White"))] = "Other"
pdem$RaceEthnicity = as.factor(temp)

pdem$Sex = as.factor(pdem$sex)

pdem$Age = pdem$interview_age/12

pdem = pdem[,c(mergecols,"Age","Sex","RaceEthnicity")]

ses = read.csv(file.path(DataDir,"ABCD_ses.csv"))

#merge everything together
data = multi.merge(fam,site,motion,pdem,ses,by=mergecols)

data = data[data$eventname=="baseline_year_1_arm_1",]

tr = 0.8
data$GoodTime = (data$TRs - data$censored)*tr/60
data$Include.rest = data$GoodTime >= 8 & data$runs>=2

#exclude for any data problems, like NaN ROIs etc
nansubs = read.csv(file.path(DataDir,"nan_subs.csv"))
data$Include.data = !(data$subjectkey %in% nansubs$subjectkey)

data$Include = data$Include.rest & data$Include.data

sum(data$Include,na.rm=T)

data = data[data$Include==T & !is.na(data$Include),]
data$Subject = gsub("NDAR_","NDAR",data$subjectkey)

sum(data$Include)

#exclude for missing ses variables
good = !is.na(data$Income2Needs) & !is.na(data$EdYearsAverage) & !is.na(data$reshist_addr1_adi_wsum) & !is.na(data$ses_fac)

data = data[good,]

#check for families that cross site and drop
t = table(data$rel_family_id,data$site_id_l)
t = t>0
sum(rowSums(t)>1)
fams = rownames(t)[rowSums(t)>1]
data = data[!(data$rel_family_id %in% fams),]

data$abcd_site = as.character(data$site_id_l)
table(data$abcd_site)
data$abcd_site_num = as.numeric(gsub('site','',data$abcd_site))

t = table(data$abcd_site)
sites = names(t)[t>=75]
data = data[data$abcd_site %in% sites,]

dim(data)

write.csv(data,file.path(DataDir,"ABCD_rest.csv"),row.names=FALSE,na="NaN")
