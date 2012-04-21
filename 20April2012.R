library(gtools)
library(lme4)
library(arm)

write.csv(dta,"icnarc.csv")

ss0 <- dta$yulos>0
ss <- dta$yulos>0 & dta$trust.code != 7625
#ss <- dta$trust.code != 7625
ss8 <- dta$yulos >= 8 & dta$trust.code != 7625
ss80 <- dta$yulos >= 8

# ICU mortality
ss <- dta$yulos>=0 & dta$trust.code != 7625
ss2 <- dta$yulos>=2 & dta$trust.code != 7625
ss20 <- dta$yulos>=2 & dta$trust.code
ss8 <- dta$yulos>=8 & dta$trust.code != 7625
options(digits=3)

display(mod1.s8 <- lmer(diedicu ~  IMlo + prop.occ.c + mean.daily.transfer + 
  N.dc.perbed + num.consult.perbed + intensivist + meanap2probuk + 
  I(avyulos/100)  + (1|trust.code), 
  data=dta, family=binomial(), na.action=na.omit, subset=ss8), digits=3)

display(mod1.s8, digits=3)
display(mod2.s8 <- update(mod1.s8, .~. + ave.cost.nurse) )
display(mod3.s8 <- update(mod1.s8, .~. + ratio.bank.total.spend),digits=3 )
display(mod4.s8 <- update(mod1.s8, .~. + ratio.agency.total.spend),digits=3 )
display(mod5.s8 <- update(mod1.s8, .~. + ratio.aux.total.spend),digits=3 )
display(mod6.s8 <- update(mod1.s8, .~. + ratio.ot.total.spend),digits=3 )


display(mod1.s8.hosp <- update(mod1.s8,diedhosp ~ .), digits=3)
display(mod2.s8.hosp <- update(mod2.s8,diedhosp ~ .), digits=3)
display(mod3.s8.hosp <- update(mod3.s8,diedhosp ~ .), digits=3)
display(mod4.s8.hosp <- update(mod4.s8,diedhosp ~ .), digits=3)
display(mod5.s8.hosp <- update(mod5.s8,diedhosp ~ .), digits=3)
display(mod6.s8.hosp <- update(mod6.s8,diedhosp ~ .), digits=3)

mod <- glmer(diedicu ~ IMlo + I(avyulos/100) + teaching.hosp + dist.next.unit +
  (1|trust.code), data=dta, family=binomial(),subset=ss8)
#display(mod, digits=3)

mod2 <- update(mod, .~. + prop.occ.c + transin.perbed.perday + 
  read.perbed.perday + mean.daily.admit + N.flex)

mod3 <- update(mod2, .~. + intensivist + total.support.staff.perbed)

mod4 <- update(mod3, .~. + N.dc.perbed + ratio.aux.total.spend + I(ave.cost.nurse/1000) +
  liaison + N.shortage)

display(mod)
display(mod2)
display(mod3)
display(mod4,digits=3)

mod5 <- update(mod4, .~. - transin.perbed.perday - read.perbed.perday - I(ave.cost.nurse/1000))
display(mod5, digits=3)

hist(dta$IMprob)
thirds <- quantile(dta$IMprob,c(0,.33,.67,1),na.rm=TRUE)
risk <- cut(dta$IMprob,c(0,.081,.362,1),labels=c("Low","Medium","High"))

mod.low <- glmer(diedicu ~ IMlo + I(avyulos/100) + teaching.hosp + dist.next.unit +
  (1|trust.code), data=dta, family=binomial(),subset=ss8&risk=="Low")
#display(mod, digits=3)

mod2.low <- update(mod.low, .~. + prop.occ.c + transin.perbed.perday + 
  read.perbed.perday + mean.daily.admit + N.flex)

mod3.low <- update(mod2.low, .~. + intensivist + total.support.staff.perbed)

mod4.low <- update(mod3.low, .~. + N.dc.perbed + ratio.aux.total.spend + I(ave.cost.nurse/1000) +
  liaison + N.shortage)

display(mod.low)
display(mod2.low)
display(mod3.low)
display(mod4.low)


mod.high <- glmer(diedicu ~ IMlo + I(avyulos/100) + teaching.hosp + dist.next.unit +
  (1|trust.code), data=dta, family=binomial(),subset=ss8&risk=="High")
#display(mod, digits=3)

mod2.high <- update(mod.high, .~. + prop.occ.c + transin.perbed.perday + 
  read.perbed.perday + mean.daily.admit + N.flex)

mod3.high <- update(mod2.high, .~. + intensivist + total.support.staff.perbed)

mod4.high <- update(mod3.high, .~. + N.dc.perbed + ratio.aux.total.spend + I(ave.cost.nurse/1000) +
  liaison + N.shortage)

display(mod.high)
display(mod2.high)
display(mod3.high)
display(mod4.high)