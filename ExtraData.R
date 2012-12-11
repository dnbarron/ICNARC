library(foreign)
library(arm)

extra <- read.dta('extradata.dta')
dta3 <- read.table("C:\\Users\\dbarron\\Google Drive\\ICNARC\\docmerged.txt")

dta4 <- merge(extra,dta3,by='patientid')
dta4$diedhosp <- car::recode(dta4$ahsurv, "'Died'=1;'Survived'=0", as.factor.result=FALSE)

ss8 <- dta4$yulos >= 8 & dta4$trust.code != 7625

or.CI <- function(m){
  se <- sqrt(diag(vcov(m)))
  b <- fixef(m)
  z <- b/se
  or <- exp(b)
  CI.up <- b + 1.96 * se
  CI.low <- b - 1.96 * se
  list(OR=or,lowerCI=exp(CI.low),upperCI=exp(CI.up))
}


###########################
## Model 1  ICU
#############################

display(H1.icu <- lmer(diedicu ~  IMlo + prop.occ.c + mean.daily.transfer +                          N.dc.perbed + num.consult.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit,            subset=ss8), digits=3)

# Odds ratios

print(or.CI(H1.icu),digits=3)

  ###########################
## Model 2  ICU
#############################


display(H2.icu <- lmer(diedicu ~  IMlo*N.dc.perbed + prop.occ.c + mean.daily.transfer +                           num.consult.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)

print(or.CI(H2.icu),digits=3)

###########################
## Model 3  ICU
#############################

display(H3.icu <- lmer(diedicu ~  IMlo*N.dc.perbed + prop.occ.c + mean.daily.transfer +                           IMlo*num.consult.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)

print(or.CI(H3.icu),digits=3)

###########################
## Model 4  ICU
#############################
H4.icu <- update(H3.icu, . ~ . + ave.cost.nurse + ratio.pbq.wte - IMlo:num.consult.perbed)
summary(H4.icu)

print(or.CI(H4.icu),digits=3)


###########################
## Model 1  Hospital
############################4#

H1.hosp <- lmer(diedhosp ~  IMlo + prop.occ.c + mean.daily.transfer +                          N.dc.perbed + num.consult.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8)

print(or.CI(H1.hosp),digits=3)

###########################
## Model 2  Hospital
#############################


H2.hosp <- lmer(diedhosp ~  IMlo*N.dc.perbed + prop.occ.c + mean.daily.transfer +                           num.consult.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8)

print(or.CI(H2.hosp),digits=3)

###########################
## Model 3  Hospital
#############################

H3.hosp <- lmer(diedhosp ~  IMlo*N.dc.perbed + prop.occ.c + mean.daily.transfer +                           IMlo*num.consult.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8)

print(or.CI(H3.hosp),digits=3)

###########################
## Model 4  Hospital
#############################
H4.hosp <- update(H3.hosp, . ~ . + ave.cost.nurse + ratio.pbq.wte - IMlo:num.consult.perbed)
print(or.CI(H4.hosp),digits=3)
