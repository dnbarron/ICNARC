library(gtools)
library(lme4)
library(arm)

dta <- read.csv("icnarc.csv")

vars <- c("diedicu","IMlo","prop.occ.c","mean.daily.transfer",
          "N.dc.perbed","num.consult.perbed","intensivist","meanap2probuk",
          "avyulos","trust.code","yulos","prop.supernum","ratio.bank.total.spend",
          "ratio.agency.total.spend","ratio.ot.total.spend","ratio.ft",
          "N.morning","N.pnoon","N.night","ratio.aux.total.spend",
          "ave.cost.nurse","ratio.pbq.wte","ahsurv","diedhosp")
dta.ss <- subset(dta,select=vars)
dta <- dta.ss

#diedhosp <- car::recode(dta$ahsurv, "'Died'=1;'Survived'=0", as.factor.result=FALSE)
#dta <- data.frame(dta,diedhosp)
#write.csv(dta,"icnarc.csv")

ss0 <- dta$yulos>0
ss <- dta$yulos>0 & dta$trust.code != 7625
#ss <- dta$trust.code != 7625
ss8 <- dta$yulos >= 8 & dta$trust.code != 7625
ss80 <- dta$yulos >= 8


###########################
## Baseline models
#############################

display(mod1 <- lmer(diedicu ~  IMlo + prop.occ.c + mean.daily.transfer + 
  N.dc.perbed + num.consult.perbed + intensivist + meanap2probuk + 
  I(avyulos/100)  + (1|trust.code), 
                     data=dta, family=binomial(), na.action=na.omit, subset=ss8), digits=3)

## Add interaction between num. nurses and IMlo
mod1a <- update(mod1, .~. + IMlo*N.dc.perbed)
display(mod1a, digits=3)
summary(mod1a,digits=3,correlation=FALSE)

mod1b <- update(mod1a, . ~ . + IMlo*num.consult.perbed)
summary(mod1b)


##################################
## Hospital mortality
##################################


display(mod2 <- lmer(diedhosp ~  IMlo + prop.occ.c + mean.daily.transfer + 
  N.dc.perbed + num.consult.perbed + intensivist + meanap2probuk + 
  I(avyulos/100)  + (1|trust.code), 
                     data=dta, family=binomial(), na.action=na.omit, subset=ss8), digits=3)

## Add interaction between num. nurses and IMlo
mod2a <- update(mod2, .~. + IMlo*N.dc.perbed)
display(mod2a, digits=3)
summary(mod2a,digits=3)

mod2b <- update(mod2a, . ~ . + IMlo*num.consult.perbed)
summary(mod2b)

############################
## Plots
##########################################
library(plyr)
library(ggplot2)

death.dta <- ddply(dta,.(trust.code), summarize,
                   num.patients.icu=sum(table(diedicu)),
                   num.deaths.icu=sum(diedicu, na.rm=TRUE),
                   num.patients.hosp=sum(table(diedhosp)),
                   num.deaths.hosp=sum(diedhosp, na.rm=TRUE))
prop.died.icu <- with(death.dta, num.deaths.icu/num.patients.icu)
ix <- order(prop.died.icu)

prop.died.hosp <- with(death.dta, num.deaths.hosp/num.patients.hosp)
death.dta <- data.frame(death.dta,prop.died.icu,prop.died.hosp)[ix,]

icu <- 1:length(prop.died.icu)                    
death.dta <- data.frame(death.dta,icu)                    

ggplot(death.dta[ix,], aes(x=icu,y=prop.died.icu)) + geom_point(colour="blue",shape=15) + labs(list(x="",y="Proportion died")) + scale_x_continuous(labels=NULL) + opts(axis.ticks = theme_blank())


ix2 <- order(death.dta$prop.died.hosp)
n <- length(prop.died.icu)
icu <- 1:n
death.dta <- death.dta[ix2,]
death.dta$icu <- icu

ggplot(death.dta, aes(x=icu,y=prop.died.hosp)) + geom_point(colour="blue",shape=15) + labs(list(x="",y="Proportion died")) + scale_x_continuous(labels=NULL) + opts(axis.ticks = theme_blank())


ggplot(dta,aes(x=IMlo)) + geom_density() + geom_hline(aes(yintercept=0),colour='white') + xlim(c(-7,7)) + ylab("Density") + xlab("IM log odds")

staff.dta <- ddply(dta,.(trust.code),summarize,
                   num.dc=mean(N.dc.perbed,na.rm=TRUE),
                   num.consult=mean(num.consult.perbed,na.rm=TRUE))
ix <- order(staff.dta$num.dc)
staff.dta <- staff.dta[ix,]
staff.dta$icu <- 1:n
staff.dta <- staff.dta[1:(n-2),]
staff.dta2 <- data.frame(Number=c(staff.dta$num.dc,staff.dta$num.consult))
staff.dta2$Staff <- gl(2,67,labels=c("Nurses","Consultants"))
staff.dta2$ICU <- rep(1:(n-2),2)
ggplot(staff.dta2,aes(x=ICU,y=Number,fill=Staff)) + geom_bar(stat='Identity',position='dodge')  + labs(list(x="",y="Number")) + scale_x_continuous(labels=NULL) + opts(axis.ticks = theme_blank())

library(effects)
effs <- allEffects(mod1a)
plot(effs,'IMlo:N.dc.perbed')
eff.1 <- effect("IMlo:N.dc.perbed",mod1a,xlevels=list(IMlo=-3:3))
plot(eff.1,x.var="N.dc.perbed",rug=FALSE)

IMlo <- factor(rep(-3:3,10))
N.dc <- vector(length=0)
for (i in 1:10){
  N.dc <- c(N.dc,rep(eff.1$variables$N.dc.perbed$levels[i],7))
}
ef.dta <- data.frame(IMlo,N.dc,fit=eff.1$fit,upper=eff.1$upper,lower=eff.1$lower)
ggplot(ef.dta,aes(x=N.dc,y=exp(fit),colour=IMlo)) + geom_line() + xlim(c(2,8)) + labs(x="Number of direct care nurses per bed",y="Odds ratio")


