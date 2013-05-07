library(foreign)
library(gtools)
library(lme4)
library(arm)
library(effects)

dta <- read.csv("icnarc.csv")

vars <- c("diedicu","diedhosp","IMlo","prop.occ.c","mean.daily.transfer",
          "N.dc.perbed","num.consult.perbed","intensivist","meanap2probuk",
          "avyulos","trust.code","yulos","prop.supernum","ratio.bank.total.spend",
          "ratio.agency.total.spend","ratio.ot.total.spend","ratio.ft",
          "N.morning","N.pnoon","N.night","ratio.aux.total.spend",
          "ave.cost.nurse","ratio.pbq.wte","ahsurv","N.total.perbed","N.supernum.perbed")
dta.ss <- subset(dta,select=vars)
dta <- dta.ss

dta3 <- read.table("C:\\Users\\dbarron\\Google Drive\\ICNARC\\docmerged.txt")
extra <- read.dta('extradata.dta')
dta4 <- merge(extra,dta3,by='patientid')
dta4$diedhosp <- car::recode(dta4$ahsurv, "'Died'=1;'Survived'=0", as.factor.result=FALSE)
ss8 <- dta4$yulos >= 8 & dta4$trust.code != 7625


xtabs(~degree,dta4)
xtabs(~diploma,dta4)

senior.nurse.wte <- with(dta4,I.grade.WTE.inpost+H.grade.WTE.inpost + G.grade.WTE.inpost)
junior.nurse.wte <- with(dta4, F.grade.WTE.inpost + E.grade.WTE.inpost+D.grade.WTE.inpost + C.grade.WTE.inpost)
ratio.senior.junior.wte <- senior.nurse.wte/junior.nurse.wte
hist(ratio.senior.junior.wte)
ix <- which(ratio.senior.junior.wte == 1)
dta[ix,"trust.code"]
#nsuper <- dta3$N.supernum.perbed
#dta <- data.frame(dta,N.supernum.perbed=nsuper)
#diedhosp <- car::recode(dta$ahsurv, "'Died'=1;'Survived'=0", as.factor.result=FALSE)
#dta <- data.frame(dta,diedhosp)
#write.csv(dta,"icnarc.csv")


########################
## qualifications
##########################
xtabs(~degree,dta4)
xtabs(~diploma,dta4)
xtabs(~enb.qual6,dta4)
xtabs(~N.inpost.WTE,dta4)
ratio.enb6 <- with(dta4, enb.qual6/N.inpost.WTE)
ratio.degree <- with(dta4, degree/N.inpost.WTE)
ratio.diploma <- with(dta4, diploma/N.inpost.WTE)

ss0 <- dta4$yulos>0
ss <- dta4$yulos>0 & dta4$trust.code != 7625
#ss <- dta$trust.code != 7625
excl <- dta4$trust.code == 7625
ss8 <- dta4$yulos >= 8 & !excl
ss80 <- dta4$yulos >= 8


################################
### Quantiles
##############################

#qts <- quantile(unique(dta$N.dc.perbed),na.rm=TRUE)
qts <- quantile(dta4$N.dc.perbed,na.rm=TRUE)

N.dc.quart <- cut(dta4$N.dc.perbed,qts)



display(cost.icu <- lmer(diedicu ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ave.cost.nurse + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8),digits=3)

display(cost.hosp <- lmer(diedhosp ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ave.cost.nurse + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8),digits=3)
                

display(pbq.icu <- lmer(diedicu ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ratio.pbq.wte + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8),digits=3)

display(pbq.hosp <- lmer(diedhosp ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ratio.pbq.wte + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8),digits=3)

ef.cost.icu <- effect('N.dc.quart:ave.cost.nurse',cost.icu)
ef.cost.hosp <- effect('N.dc.quart:ave.cost.nurse',cost.hosp)
ef.pbq.icu <- effect('N.dc.quart:ratio.pbq.wte',pbq.icu)
ef.pbq.hosp <- effect('N.dc.quart:ratio.pbq.wte',pbq.hosp)

plot(ef.pbq.icu)

display(ot.icu <- lmer(diedicu ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ratio.ot.total.spend + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8),digits=3)

display(ot.hosp <- lmer(diedhosp ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ratio.ot.total.spend + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8),digits=3)

ef.ot.icu <- effect('N.dc.quart:ratio.ot.total.spend',ot.icu)
ef.ot.hosp <- effect('N.dc.quart:ratio.ot.total.spend',ot.hosp)

plot(ef.ot.icu)
plot(ef.ot.hosp)


display(bank.icu <- lmer(diedicu ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ratio.bank.total.spend + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8),digits=3)

display(bank.hosp <- lmer(diedhosp ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ratio.bank.total.spend + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8),digits=3)

ef.bank.icu <- effect('N.dc.quart:ratio.bank.total.spend',bank.icu)
ef.bank.hosp <- effect('N.dc.quart:ratio.bank.total.spend',bank.hosp)

plot(ef.bank.icu)
plot(ef.bank.hosp)

display(agency.icu <- lmer(diedicu ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ratio.agency.total.spend + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8),digits=3)

display(agency.hosp <- lmer(diedhosp ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ratio.agency.total.spend + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8),digits=3)

ef.agency.icu <- effect('N.dc.quart:ratio.agency.total.spend',agency.icu)
ef.agency.hosp <- effect('N.dc.quart:ratio.agency.total.spend',agency.hosp)

plot(ef.agency.icu)
plot(ef.agency.hosp)

#dta3 <- read.table("C:\\Users\\dbarron\\Google Drive\\ICNARC\\docmerged.txt")
#xtabs(~degree,dta4)
#xtabs(~diploma,dta4)

senior.nurse.wte <- with(dta3,I.grade.WTE.inpost+H.grade.WTE.inpost + G.grade.WTE.inpost)
junior.nurse.wte <- with(dta3, F.grade.WTE.inpost + E.grade.WTE.inpost+D.grade.WTE.inpost + C.grade.WTE.inpost)
ratio.senior.junior.wte <- senior.nurse.wte/junior.nurse.wte
hist(ratio.senior.junior.wte)
ix <- which(ratio.senior.junior.wte == 1)
dta[ix,"trust.code"]

# Trust 7671 is an outlier

excl <- dta$trust.code == 7625 | dta$trust.code == 7671
ss8 <- dta$yulos >= 8 & !excl

display(senior.icu <- lmer(diedicu ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ratio.senior.junior.wte + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta, family=binomial(), na.action=na.omit, subset=ss8),digits=3)

display(senior.hosp <- lmer(diedhosp ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ratio.senior.junior.wte + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta, family=binomial(), na.action=na.omit, subset=ss8),digits=3)

ef.senior.icu <- effect('N.dc.quart:ratio.senior.junior.wte',senior.icu)
ef.senior.hosp <- effect('N.dc.quart:ratio.senior.junior.wte',senior.hosp)

plot(ef.senior.icu)
plot(ef.senior.hosp)


excl <- dta$trust.code == 7625 | dta$trust.code == 7103
ss8 <- dta$yulos >= 8 & !excl

display(enb6.icu <- lmer(diedicu ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ratio.enb6 + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta, family=binomial(), na.action=na.omit, subset=ss8),digits=3)

display(enb6.hosp <- lmer(diedhosp ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ratio.enb6 + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta, family=binomial(), na.action=na.omit, subset=ss8),digits=3)

ef.enb6.icu <- effect('N.dc.quart:ratio.enb6',enb6.icu)
ef.enb6.hosp <- effect('N.dc.quart:ratio.enb6',enb6.hosp)

plot(ef.enb6.icu)
plot(ef.enb6.hosp)


excl <- dta$trust.code == 7625 
ss8 <- dta$yulos >= 8 & !excl


display(diploma.icu <- lmer(diedicu ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ratio.diploma + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta, family=binomial(), na.action=na.omit, subset=ss8),digits=3)

display(diploma.hosp <- lmer(diedhosp ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ratio.diploma + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta, family=binomial(), na.action=na.omit, subset=ss8),digits=3)

ef.diploma.icu <- effect('N.dc.quart:ratio.diploma',diploma.icu)
ef.diploma.hosp <- effect('N.dc.quart:ratio.diploma',diploma.hosp)

plot(ef.diploma.icu)
plot(ef.diploma.hosp)

excl <- dta$trust.code == 7625 
ss8 <- dta$yulos >= 8 & !excl


display(degree.icu <- lmer(diedicu ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ratio.degree + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta, family=binomial(), na.action=na.omit, subset=ss8),digits=3)

display(degree.hosp <- lmer(diedhosp ~  IMlo + prop.occ.c + mean.daily.transfer + N.dc.quart*ratio.degree + N.supernum.perbed + num.consult.perbed + intensivist +  meanap2probuk +  I(avyulos/100) + (1|trust.code), data=dta, family=binomial(), na.action=na.omit, subset=ss8),digits=3)

ef.degree.icu <- effect('N.dc.quart:ratio.degree',degree.icu)
ef.degree.hosp <- effect('N.dc.quart:ratio.degree',degree.hosp)

plot(ef.degree.icu)
plot(ef.degree.hosp)