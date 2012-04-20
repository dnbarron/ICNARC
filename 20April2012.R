library(gtools)
library(lme4)
library(arm)

ss0 <- dta$yulos>0
ss <- dta$yulos>0 & dta$trust.code != 7625
#ss <- dta$trust.code != 7625
ss8 <- dta$yulos >= 8 & dta$trust.code != 7625
ss80 <- dta$yulos >= 8

# ICU mortality
ss <- dta$yulos>=0 & dta$trust.code != 7625
ss2 <- dta$yulos>=2 & dta$trust.code != 7625
ss8 <- dta$yulos>=8 & dta$trust.code != 7625

display(mod1.ap.ss0 <- lmer(diedicu ~  IMlo + prop.occ.c + +mean.daily.transfer + N.dc.perbed + num.consult.perbed + 
  intensivist + meanap2probuk + I(avyulos/100) + ratio.bank.total.spend + ave.cost.nurse
                     + (1|trust.code), data=dta, family=binomial(), na.action=na.omit, subset=ss8, model=TRUE))
