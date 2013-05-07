library(foreign)
library(arm)
library(ggplot2)
library(GGally)
library(effects)

extra <- read.dta('extradata.dta')
dta3 <- read.table("C:\\Users\\dbarron\\Google Drive\\ICNARC\\docmerged.txt")

dta4 <- merge(extra,dta3,by='patientid')
dta4$diedhosp <- car::recode(dta4$ahsurv, "'Died'=1;'Survived'=0", as.factor.result=FALSE)

dta4$total.transfers.perbed <- with(dta4, transin.perbed.perday + transout.perbed.perday)
dta4$admissions.perbed.perday <- with(dta4, mean.daily.admit/icubeds)

ss8 <- dta4$yulos >= 8 & dta4$trust.code != 7625
# Trust 7625 excluded because it has a value of N.dc.perbed of 14.2, which is roughtly twice as large as the next largest. This seems likely to be an error in the data

#ss8 <- dta4$yulos >= 8 
########### Use NHD variables instead of consultant headcount
## The shared.NHD variable is given half weight

dta4$clinical.NHD <- with(dta4, num.unit.NHD + .5*num.shared.NHD)
dta4$clinical.NHD.perbed <- with(dta4, clinical.NHD/icubeds)

or.CI <- function(m){
  se <- sqrt(diag(vcov(m)))
  b <- fixef(m)
  z <- b/se
  or <- exp(b)
  CI.up <- b + 1.96 * se
  CI.low <- b - 1.96 * se
  df <- data.frame(OR=or,lowerCI=exp(CI.low),upperCI=exp(CI.up))
  row.names(df) <- names(b)
  df
}




###########################
## Model 1  ICU
##### NB transin is used rather than total transfers because we don't have data on transfers out from 4 units. Multiply by seven to get transfers per week per bed, on a similar scale to other variables

display(H1.icu <- lmer(diedicu ~  IMlo + prop.occ.c + admissions.perbed.perday + I(transin.perbed.perday*7) + total.support.staff.perbed + N.dc.perbed + clinical.NHD.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)


# Odds ratios
display(H1.icu)          
print(or.CI(H1.icu),digits=3)

###########################
## Model 2  ICU


display(H2.icu <- lmer(diedicu ~  IMlo*N.dc.perbed + prop.occ.c + admissions.perbed.perday +  I(transin.perbed.perday*7) + total.support.staff.perbed + clinical.NHD.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)

display(H2.icu,digits=3)
print(or.CI(H2.icu),digits=3)

###########################
## Model 3  ICU

display(H3.icu <- lmer(diedicu ~  IMlo*N.dc.perbed + prop.occ.c + admissions.perbed.perday + I(transin.perbed.perday*7) + total.support.staff.perbed + IMlo*clinical.NHD.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)

display(H3.icu, dig=3)          
print(or.CI(H3.icu),digits=3)


###########################
## Model 1  Hospital
##### NB transin is used rather than total transfers because we don't have data on transfers out from 4 units. Multiply by seven to get transfers per week per bed, on a similar scale to other variables

display(H1.hosp <- lmer(diedhosp ~  IMlo + prop.occ.c + admissions.perbed.perday + I(transin.perbed.perday*7) + total.support.staff.perbed + N.dc.perbed + clinical.NHD.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)


# Odds ratios
display(H1.hosp, dig=3)          
print(or.CI(H1.hosp),digits=3)

###########################
## Model 2  Hospital


display(H2.hosp <- lmer(diedhosp ~  IMlo*N.dc.perbed + prop.occ.c + admissions.perbed.perday +  I(transin.perbed.perday*7) + total.support.staff.perbed + clinical.NHD.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)

display(H2.hosp,digits=3)
print(or.CI(H2.hosp),digits=3)

###########################
## Model 3  Hospital

display(H3.hosp <- lmer(diedhosp ~  IMlo*N.dc.perbed + prop.occ.c + admissions.perbed.perday + I(transin.perbed.perday*7) + total.support.staff.perbed + IMlo*clinical.NHD.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)

display(H3.hosp, dig=3)          
print(or.CI(H3.hosp),digits=3)
