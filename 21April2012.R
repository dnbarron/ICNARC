library(gtools)
library(lme4)
library(arm)

dta <- read.csv("icnarc.csv")

vars <- c("diedicu","IMlo","prop.occ.c","mean.daily.transfer",
          "N.dc.perbed","num.consult.perbed","intensivist","meanap2probuk",
          "avyulos","trust.code","yulos","prop.supernum","ratio.bank.total.spend",
          "ratio.agency.total.spend","ratio.ot.total.spend","ratio.ft",
          "N.morning","N.pnoon","N.night","ratio.aux.total.spend",
          "ave.cost.nurse","ratio.pbq.wte","ahsurv")
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
####
## Direct care v. supernumary
#####
mod2 <- update(mod1, .~. + prop.supernum)
mod2a <- update(mod1a, .~. + prop.supernum)
display(mod2, digits=3)  # .001 (.574)
display(mod2a, digits=3)  # -.029 (.576)

##############
## Bank, agency, overtime
###############################
mod3 <- update(mod1, .~. + ratio.bank.total.spend + ratio.agency.total.spend + 
  ratio.ot.total.spend )
display(mod3, digits = 3)
# banking   -1.35 (1.25)
# agency    -.267 (.749)
# ot        .186 (4.888)

mod3a <- update(mod3, .~. - ratio.agency.total.spend - ratio.ot.total.spend)
mod3b <- update(mod3, .~. - ratio.bank.total.spend - ratio.ot.total.spend)
mod3c <- update(mod3, .~. - ratio.agency.total.spend - ratio.bank.total.spend)
display(mod3a, digits=3)  # bank -1.40 (1.23)
display(mod3b, digits=3)  # agency -.348 (.750)
display(mod3c, digits=3)  # ot 1.07 (4.88)

#### with interaction

mod3i <- update(mod3, .~. + N.dc.perbed*IMlo )
display(mod3i, digits = 3)
# banking   -1.36 (1.25)
# agency    -.285 (.752)
# ot        .104 (4.89)

mod3ai <- update(mod3i, .~. - ratio.agency.total.spend - ratio.ot.total.spend)
mod3bi <- update(mod3i, .~. - ratio.bank.total.spend - ratio.ot.total.spend)
mod3ci <- update(mod3i, .~. - ratio.agency.total.spend - ratio.bank.total.spend)
display(mod3ai, digits=3)  # bank -1.41 (1.23)
display(mod3bi, digits=3)  # agency -.364 (.753)
display(mod3ci, digits=3)  # ot 1.01 (4.89)


############################################
## full time v. part time
#########################################
mod4 <- update(mod1, .~. + ratio.ft)
display(mod4, digits=3)  #  -.445 (.317)

mod4i <- update(mod1a, .~. + ratio.ft)
display(mod4i, digits=3)   #  -.445 (.317)

##########################################
## Morning, afternoon, night
#######################################

mod5 <- update(mod1, .~. + N.morning)
mod5i <- update(mod1a, .~. + N.morning)
display(mod5, digits=3)  # .009 (.021)
display(mod5i, digits=3)  # .008 (.021)

mod5a <- update(mod1, .~. + N.pnoon)
mod5ai <- update(mod1a, .~. + N.pnoon)
display(mod5a, digits=3)  # .011 (.023)
display(mod5ai, digits=3)  # .010 (.023)

mod5b <- update(mod1, .~. + N.night)
mod5bi <- update(mod1a, .~. + N.night)
display(mod5b, digits=3)   # .006 (.023)
display(mod5bi, digits=3)  # .005 (.023)

###########################################
### Qualified v auxill
############################################
mod6 <- update(mod1, .~. + ratio.aux.total.spend)
mod6i <- update(mod1a, .~. + ratio.aux.total.spend)
display(mod6, digits=3)  #  .083 (.648)
display(mod6i, digits=3)  # .077 (.649)

#################################################
## Average cost of a nurse
##############################################

mod7 <- update(mod1, .~. + ave.cost.nurse)
mod7i <- update(mod1a, .~. + ave.cost.nurse)
display(mod7, digits=3)  # .004 (.009)
display(mod7i, digits=3)  # .004 (.009)

###############################################
## Degree, post-basic quals
###########################################

mod8 <- update(mod1, .~. + ratio.pbq.wte)
mod8i <- update(mod1a, .~. + ratio.pbq.wte)
display(mod8, digits=3)  # -.121 (.135)
display(mod8i, digits=3)  # -.130 (.135)


######################################################################
## Same models, remove N.dc.perbed
######################################################################
mod1N <- update(mod1, .~. - N.dc.perbed)
mod1aN <- update(mod1a, .~. - N.dc.perbed)

mod2N <- update(mod2, .~. - N.dc.perbed)
mod2aN <- update(mod2a, .~. - N.dc.perbed)

mod3N <- update(mod3, .~. - N.dc.perbed)
mod3aN <- update(mod3a, .~. - N.dc.perbed)
mod3bN <- update(mod3b, .~. - N.dc.perbed)
mod3cN <- update(mod3c, .~. - N.dc.perbed)

mod3Ni <- update(mod3i, .~. - N.dc.perbed)
mod3aNi <- update(mod3ai, .~. - N.dc.perbed)
mod3bNi <- update(mod3bi, .~. - N.dc.perbed)
mod3cNi <- update(mod3ci, .~. - N.dc.perbed)

mod4N <- update(mod4, .~. - N.dc.perbed)
mod4Ni <- update(mod4i, .~. - N.dc.perbed)

mod5N <- update(mod5, .~. - N.dc.perbed)
mod5aN <- update(mod5a, .~. - N.dc.perbed)
mod5bN <- update(mod5b, .~. - N.dc.perbed)

mod5Ni <- update(mod5i, .~. - N.dc.perbed)
mod5aNi <- update(mod5ai, .~. - N.dc.perbed)
mod5bNi <- update(mod5bi, .~. - N.dc.perbed)

mod6N <- update(mod6, .~. - N.dc.perbed)
mod6Ni <- update(mod6i, .~. - N.dc.perbed)

mod7N <- update(mod7, .~. - N.dc.perbed)
mod7Ni <- update(mod7i, .~. - N.dc.perbed)

mod8N <- update(mod8, .~. - N.dc.perbed)
mod8Ni <- update(mod8i, .~. - N.dc.perbed)

display(mod1N)
display(mod1aN)
display(mod2N)  # .21 (.59)
display(mod2aN)  # .19 (.59)
display(mod3N)  #bank -1.28 (1.31); agency .05 (.78); ot -.48 (5.18)
display(mod3aN)  # bank -1.25 (1.29)
display(mod3bN)  # agency -.01 (.78)
display(mod3cN)  # ot .14 (5.15)
display(mod3Ni)  #bank -1.28 (1.31); agency .05 (.78); ot -.57 (5.19)
display(mod3aNi)  # bank -1.25 (1.30)
display(mod3bNi)  # agency -.02 (.78)
display(mod3cNi)  # ot .06 (5.16)
display(mod4N)  # -.29 (.33)
display(mod4Ni)  #  -.29 (.33)
display(mod5N)  # .01 (.02)
display(mod5aN)  # .01 (.02)
display(mod5bN)  # .00 (.02)
display(mod5Ni)  # .00 (.02)
display(mod5aNi)  #  .00 (.02)
display(mod5bNi)  # .00 (.02)
display(mod6N)  # -.05 (.68)
display(mod6Ni)  # -.06 (.68)
display(mod7N, dig=3)  # .008 (.009) 
display(mod7Ni, dig=3)  # .008 (.009)
display(mod8N)  # -.12 (.14)
display(mod8Ni)  # -.12 (.14)


# subset by risk
qs <- quantile(dta$IMlo, c(0,.33,.67,1), na.rm=TRUE)
risk <- cut(dta$IMlo,breaks=qs,labels=c("Low","Medium","High"))

### Low risk

display(mod1NLow <- lmer(diedicu ~  IMlo + prop.occ.c + mean.daily.transfer + 
  num.consult.perbed + intensivist + meanap2probuk + 
  I(avyulos/100)  + (1|trust.code), 
                     data=dta, family=binomial(), na.action=na.omit, subset=ss8&risk=="Low"), digits=3)
mod2NLow <- update(mod1NLow, .~. + prop.supernum)

mod3NLow <- update(mod1NLow, .~. + ratio.bank.total.spend + ratio.agency.total.spend + 
  ratio.ot.total.spend )
mod3aNLow <- update(mod3NLow, .~. - ratio.agency.total.spend - ratio.ot.total.spend)
mod3bNLow <- update(mod3NLow, .~. - ratio.bank.total.spend - ratio.ot.total.spend)
mod3cNLow <- update(mod3NLow, .~. - ratio.agency.total.spend - ratio.bank.total.spend)
mod4NLow <- update(mod1NLow, .~. + ratio.ft)
mod5NLow <- update(mod1NLow, .~. + N.morning)
mod5aNLow <- update(mod1NLow, .~. + N.pnoon)
mod5bNLow <- update(mod1NLow, .~. + N.night)
mod6NLow <- update(mod1NLow, .~. + ratio.aux.total.spend)
mod7NLow <- update(mod1NLow, .~. + ave.cost.nurse)
mod8NLow <- update(mod1NLow, .~. + ratio.pbq.wte)

display(mod1NLow)
display(mod2NLow)  # -2.14 (3.11)
display(mod3NLow)  #bank -19.3 (6.33); agency .66 (2.59); ot -19.5 (16.2)
display(mod3aNLow)  # bank -17.9 (6.44)
display(mod3bNLow)  # agency .26 (3.04)
display(mod3cNLow)  # ot -11.4 (18.2)
display(mod4NLow)  # .85 (1.31)
display(mod5NLow)  # -.10 (.08)
display(mod5aNLow)  # -.09 (.09)
display(mod5bNLow)  # -.10 (.09)
display(mod6NLow)  # -1.85 (2.82)
display(mod7NLow, dig=3)  # -.10 (.092) 
display(mod8NLow)  # -.38 (.58)


########## Medium risk
display(mod1NMed <- lmer(diedicu ~  IMlo + prop.occ.c + mean.daily.transfer + 
  num.consult.perbed + intensivist + meanap2probuk + 
  I(avyulos/100)  + (1|trust.code), 
                         data=dta, family=binomial(), na.action=na.omit, subset=ss8&risk=="Medium"), digits=3)

mod2NMed <- update(mod1NMed, .~. + prop.supernum)

mod3NMed <- update(mod1NMed, .~. + ratio.bank.total.spend + ratio.agency.total.spend + 
  ratio.ot.total.spend )
mod3aNMed <- update(mod3NMed, .~. - ratio.agency.total.spend - ratio.ot.total.spend)
mod3bNMed <- update(mod3NMed, .~. - ratio.bank.total.spend - ratio.ot.total.spend)
mod3cNMed <- update(mod3NMed, .~. - ratio.agency.total.spend - ratio.bank.total.spend)
mod4NMed <- update(mod1NMed, .~. + ratio.ft)
mod5NMed <- update(mod1NMed, .~. + N.morning)
mod5aNMed <- update(mod1NMed, .~. + N.pnoon)
mod5bNMed <- update(mod1NMed, .~. + N.night)
mod6NMed <- update(mod1NMed, .~. + ratio.aux.total.spend)
mod7NMed <- update(mod1NMed, .~. + ave.cost.nurse)
mod8NMed <- update(mod1NMed, .~. + ratio.pbq.wte)

display(mod1NMed)
display(mod2NMed)  # -.22 (.90)
display(mod3NMed)  #bank -3.24 (1.82); agency .37 (1.07); ot .69 (6.39)
display(mod3aNMed)  # bank -3.18 (1.80)
display(mod3bNMed)  # agency .11 (1.10)
display(mod3cNMed)  # ot 2.12 (6.58)
display(mod4NMed)  # -.17 (.48)
display(mod5NMed)  # .01 (.03)
display(mod5aNMed)  # .01 (.03)
display(mod5bNMed)  # .01 (.03)
display(mod6NMed)  # .55 (.85)
display(mod7NMed)  # .01 (.02) 
display(mod8NMed)  # -.34 (.19)

########### High risk
display(mod1NHigh <- lmer(diedicu ~  IMlo + prop.occ.c + mean.daily.transfer + 
  num.consult.perbed + intensivist + meanap2probuk + 
  I(avyulos/100)  + (1|trust.code), 
                         data=dta, family=binomial(), na.action=na.omit, subset=ss8&risk=="High"), digits=3)

mod2NHigh <- update(mod1NHigh, .~. + prop.supernum)

mod3NHigh <- update(mod1NHigh, .~. + ratio.bank.total.spend + ratio.agency.total.spend + 
  ratio.ot.total.spend )
mod3aNHigh <- update(mod3NHigh, .~. - ratio.agency.total.spend - ratio.ot.total.spend)
mod3bNHigh <- update(mod3NHigh, .~. - ratio.bank.total.spend - ratio.ot.total.spend)
mod3cNHigh <- update(mod3NHigh, .~. - ratio.agency.total.spend - ratio.bank.total.spend)
mod4NHigh <- update(mod1NHigh, .~. + ratio.ft)
mod5NHigh <- update(mod1NHigh, .~. + N.morning)
mod5aNHigh <- update(mod1NHigh, .~. + N.pnoon)
mod5bNHigh <- update(mod1NHigh, .~. + N.night)
mod6NHigh <- update(mod1NHigh, .~. + ratio.aux.total.spend)
mod7NHigh <- update(mod1NHigh, .~. + ave.cost.nurse)
mod8NHigh <- update(mod1NHigh, .~. + ratio.pbq.wte)

display(mod1NHigh)
display(mod2NHigh)  # .22 (.55)
display(mod3NHigh)  #bank -.50 (1.23); agency .00 (.73); ot 1.28 (4.67)
display(mod3aNHigh)  # bank -.55 (1.80)
display(mod3bNHigh)  # agency -.06 (.73)
display(mod3cNHigh)  # ot 1.54 (4.60)
display(mod4NHigh)  # -.43 (.30)
display(mod5NHigh)  # .00 (.02)
display(mod5aNHigh)  # .01 (.02)
display(mod5bNHigh)  # .00 (.02)
display(mod6NHigh)  # -.40 (.62)
display(mod7NHigh)  # .01 (.01) 
display(mod8NHigh)  # -.03 (.13)


##########################################################
### Final table
######################################################

display(mod1 <- lmer(diedicu ~  IMlo + prop.occ.c + mean.daily.transfer + 
  N.total.perbed + num.consult.perbed + intensivist + meanap2probuk + 
  I(avyulos/100)  + (1|trust.code), 
                     data=dta, family=binomial(), subset=ss8), digits=3)

mod2 <- update(mod1, .~. - N.total.perbed + N.dc.perbed)
mod3 <- update(mod2, .~. + N.Supernum)
mod3.a <- update(mod2, .~. + prop.supernum)
mod4 <- update(mod2, .~. + ratio.aux.total.spend)
mod5 <- update(mod2, .~. + ratio.pbq.wte)
mod6 <- update(mod2, .~. + ave.cost.nurse)
display(mod2)
display(mod3)
display(mod3.a)
display(mod4)
display(mod5)
display(mod6)

mod3N <- update(mod3, .~. - N.dc.perbed)
mod3.aN <- update(mod3.a, .~. - N.dc.perbed)
mod4N <- update(mod4, .~. - N.dc.perbed)
mod5N <- update(mod5, .~. - N.dc.perbed)
mod6N <- update(mod6, .~. - N.dc.perbed)

display(mod3N)
display(mod4N)
display(mod5N)
display(mod6N)

library(effects)
ae <- effect('N.dc.perbed',mod2)
plot(ae,xlab="Number of direct care nurses per bed",ylab="Probability of mortality",main="")
b <- fixef(mod2)
lp.hat <- b[1] + b[2]*1.51 + b[3]*.833 + b[4]*.058 + b[5]*.4 + b[6] + b[7]*.289 + b[8]*1

lp.hat <- lp.hat+b[9]*seq(.2,8.4,by=.1)
p.hat <- invlogit(lp.hat)
p.dta <- data.frame(N.dc.perbed=seq(.2,8.4,by=.1),Prob=p.hat)
library(ggplot2)
ggplot(p.dta, aes(x=N.dc.perbed,y=Prob)) + geom_line()  + 
  labs(x="Number of direct care nurses per bed",y="Probability of mortality")

xtable.mer <- function(x, caption=NULL, label=NULL, align=NULL, digits=NULL,
                    display=NULL)
{
  xx <- summary(x)
  x <- data.frame(xx@coefs, check.names = FALSE)
  class(x) <- c("xtable", "data.frame")
  caption(x) <- caption
  label(x) <- label
  align(x) <- switch(1 + is.null(align), align, c("r", "r", 
                                                  "r", "r", "r"))
  digits(x) <- switch(1 + is.null(digits), digits, c(0, 4, 
                                                     4, 2, 4))
  display(x) <- switch(1 + is.null(display), display, c("s", 
                                                        "f", "f", "f", "f"))
  return(x)
}

                    
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
                
icu <- 1:length(prop.died)                    
death.dta <- data.frame(death.dta,icu)                    
                    
ggplot(death.dta[ix,], aes(x=icu,y=prop.died.icu)) + geom_point(colour="blue",shape=15) + labs(list(x="",y="Proportion died")) + scale_x_continuous(labels=NULL) + opts(axis.ticks = theme_blank())


ggplot(dta,aes(x=IMlo)) + geom_density() + geom_hline(aes(yintercept=0),colour='white') + xlim(c(-7,7)) + ylab("Density") + xlab("IM log odds")

staff.dta <- ddply(dta,.(trust.code),summarize,
                   num.dc=mean(N.dc.perbed,na.rm=TRUE),
                   num.consult=mean(num.consult.perbed,na.rm=TRUE))
ix <- order(staff.dta$num.dc)
staff.dta <- staff.dta[ix,]
staff.dta$icu <- 1:n
staff.dta2 <- data.frame(Number=c(staff.dta$num.dc,staff.dta$num.consult))
staff.dta2$Staff <- gl(2,n,labels=c("Nurses","Consultants"))
staff.dta2$ICU <- rep(1:n,2)
ggplot(staff.dta2,aes(x=ICU,y=Number,fill=Staff)) + geom_bar(stat='Identity',position='dodge') + ylim(c(0,8))