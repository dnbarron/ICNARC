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

ss8 <- dta4$yulos >= 8 & dta4$trust.code != 7625
# Trust 7625 excluded because it has a value of N.dc.perbed of 14.2, which is roughtly twice as large as the next largest. This seems likely to be an error in the data

#ss8 <- dta4$yulos >= 8 


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
## Model 0  ICU
#############################

display(H0.icu <- lmer(diedicu ~  IMlo + prop.occ.c + I(transin.perbed.perday*7) + total.support.staff.perbed + N.dc.perbed + num.consult.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)

print(or.CI(H0.icu),digits=3)


###########################
## Model 1  ICU
#############################
##### NB transin is used rather than total transfers because we don't have data on transfers out from 4 units. Multiply by seven to get transfers per week per bed, on a similar scale to other variables

display(H1.icu <- lmer(diedicu ~  IMlo + prop.occ.c + I(transin.perbed.perday*7) +                         N.dc.perbed + num.consult.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)

display(H1.icu.out <- lmer(diedicu ~  IMlo + prop.occ.c + I(transout.perbed.perday*7) +                         N.dc.perbed + num.consult.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)

display(H1.icu.both <- lmer(diedicu ~  IMlo + prop.occ.c + I(transin.perbed.perday*7) + I(transout.perbed.perday*7) +  N.dc.perbed + num.consult.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)

# Odds ratios

print(or.CI(H1.icu),digits=3)

###########################
## Model 2  ICU
#############################


display(H2.icu <- lmer(diedicu ~  IMlo*N.dc.perbed + prop.occ.c +  I(transin.perbed.perday*7) + num.consult.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)

print(or.CI(H2.icu),digits=3)

###########################
## Model 3  ICU
#############################

display(H3.icu <- lmer(diedicu ~  IMlo*N.dc.perbed + prop.occ.c + I(transin.perbed.perday*7) + IMlo*num.consult.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)

print(or.CI(H3.icu),digits=3)

###########################
## Model 4  ICU
#############################
H4.icu <- update(H3.icu, . ~ . + ave.cost.nurse + ratio.pbq.wte - IMlo:num.consult.perbed)
summary(H4.icu)

print(or.CI(H4.icu),digits=3)


###########################
## Model 0  Hospital
############################4#

H0.hosp <- lmer(diedhosp ~  IMlo + prop.occ.c + I(transin.perbed.perday*7) + total.support.staff.perbed + N.dc.perbed + num.consult.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8)

display(H0.hosp, digits=3)
print(or.CI(H0.hosp),digits=3)


###########################
## Model 1  Hospital
############################4#

H1.hosp <- lmer(diedhosp ~  IMlo + prop.occ.c + I(transin.perbed.perday*7) +                          N.dc.perbed + num.consult.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8)

display(H1.hosp, digits=3)
print(or.CI(H1.hosp),digits=3)

###########################
## Model 2  Hospital
#############################


H2.hosp <- lmer(diedhosp ~  IMlo*N.dc.perbed + prop.occ.c + I(transin.perbed.perday*7) +                           num.consult.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8)

display(H2.hosp, digits=3)
print(or.CI(H2.hosp),digits=3)

###########################
## Model 3  Hospital
#############################

H3.hosp <- lmer(diedhosp ~  IMlo*N.dc.perbed + prop.occ.c + I(transin.perbed.perday*7) +                           IMlo*num.consult.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8)

display(H3.hosp, digits=3)
print(or.CI(H3.hosp),digits=3)

###########################
## Model 4  Hospital
#############################
H4.hosp <- update(H3.hosp, . ~ . + ave.cost.nurse + ratio.pbq.wte - IMlo:num.consult.perbed)
print(or.CI(H4.hosp),digits=3)


############################
## Descriptive stats
###################################
vars <- c('IMlo','prop.occ.c','mean.daily.transfer','N.dc.perbed','num.consult.perbed','intensivist','IMlomean.x','avyulos')

ix <- complete.cases(dta4[,c(vars,'diedicu')])
dta4.nomiss <- dta4[ix,c(vars,'diedicu','diedhosp','trust.code','yulos')]
ss8 <- dta4.nomiss$yulos >= 8 & dta4.nomiss$trust.code != 7625
dta4.nomiss <- dta4.nomiss[ss8,]
dim(dta4.nomiss)

xtabs(~diedicu + diedhosp, dta4.nomiss, na.action=na.pass, exclude=NULL)

with(dta4.nomiss,length(unique(trust.code)))
ddply(dta4.nomiss,.(trust.code),summarize,
      mean.Ndc=mean(N.dc.perbed))
ggpairs(dta4.nomiss, c(1,2,3,4,5))

crude.icu.mort <- ddply(dta4.nomiss, .(trust.code), summarize,
      icu.mort.rate = sum(diedicu)/length(diedicu))
crude.hosp.mort <- ddply(dta4.nomiss, .(trust.code), summarize,
      hosp.mort.rate = sum(diedhosp,na.rm=TRUE)/length(diedhosp))
ix <- order(crude.icu.mort[,2])
ixh <- order(crude.hosp.mort[,2])

crude.icu.mort <- crude.icu.mort[ix,]
crude.hosp.mort <- crude.hosp.mort[ixh,]

crude.icu.mort$ID <- 1:dim(crude.icu.mort[1])
crude.hosp.mort$ID <- 1:dim(crude.hosp.mort[1])

min(crude.icu.mort[,2])
max(crude.icu.mort[,2])
ggplot(data=crude.icu.mort,aes(x=ID,y=icu.mort.rate)) + geom_point()
ggplot(data=crude.hosp.mort,aes(x=ID,y=hosp.mort.rate)) + geom_point()

crude.mort <- merge(crude.icu.mort,crude.hosp.mort,by="trust.code")
with(crude.mort,cor(icu.mort.rate,hosp.mort.rate))


#######################################
## Graphs
###################################
library(ggplot2)
ggplot(data=dta4,aes(x=IMlo)) + geom_histogram(binwidth=.3) + xlab('IM log odds') + ylab('Frequency') 

library(effects)
ef <- effect('IMlo:N.dc.perbed',H2.icu,se=FALSE,xlevels=list(IMlo=c(-3:3)))
plot(ef)
ef.dta <- data.frame(fit=ef$fit,IMlo=factor(ef$x$IMlo),N.dc.perbed=ef$x$N.dc.perbed)
ggplot(data=ef.dta,aes(x=N.dc.perbed,y=exp(fit),colour=IMlo)) + geom_line() + xlab('Number of direct care nurses') + ylab('Odds ratio')

###############################
## Predicted values
#################################

## reduction in prob of mortality as N.dc.perbed goes from 4 to 6 with IMlo of -3, column 2 table 5

b <- fixef(H2.icu)

x <- c(1,-3,4,.83,.13,.45,0,-.69,.1,-12)
       
with(dta4.nomiss, mean(avyulos)

     lp <- sum(x*b)
     or.hat <- exp(lp)
     invlogit(lp)*1000
     
x.6 <- c(1,-3,6,.83,.13,.45,0,-.69,.1,-12)
     
lp.6 <- sum(x.6*b)
or.hat.6 <- exp(lp.6)
invlogit(lp.6)*1000
     
## IMlo = 2     
     x <- c(1,2,4,.83,.13,.45,0,-.69,.1,-12)
     
     with(dta4.nomiss, mean(avyulos)
          
          lp <- sum(x*b)
          or.hat <- exp(lp)
          invlogit(lp)*30
          
    x.6 <- c(1,2,6,.83,.13,.45,0,-.69,.1,-12)
          
          lp.6 <- sum(x.6*b)
          or.hat.6 <- exp(lp.6)
          invlogit(lp.6)*30
        
with(dta4.nomiss, quantile(IMlo,.96))
          
          
###############
## correlation
################
          
vns <- c('prop.occ.c','mean.daily.transfer','N.dc.perbed','num.consult.perbed','IMlomean.x','avyulos')
cor.dat <- subset(dta4,select=vns,subset=ss8)          
  cor(cor.dat,use='c')
          
### Histogram
          
ggplot(data=dta4,aes(x=IMlo)) + geom_histogram() + ylab('Frequency')
          
#  check transfer variables
vnames <- names(dta4)
any(match('mean.daily.transfer.out',vnames))
          
transfers.perbed <- dta4[,"transin.perbed.perday"] + dta4[,"transout.perbed.perday"]
          
########### Use NHD variables instead of consultant headcount
## The shared.NHD variable is given half weight
          
dta4$clinical.NHD <- with(dta4, num.unit.NHD + .5*num.shared.NHD)
dta4$clinical.NHD.perbed <- with(dta4, clinical.NHD/icubeds)
          
###########################
## Model 0  ICU
          
display(H0.icu <- lmer(diedicu ~  IMlo + prop.occ.c + I(transin.perbed.perday*7) + total.support.staff.perbed + N.dc.perbed + clinical.NHD.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)
          
print(or.CI(H0.icu),digits=3)
display(H0.icu)          
          
###########################
## Model 1  ICU
##### NB transin is used rather than total transfers because we don't have data on transfers out from 4 units. Multiply by seven to get transfers per week per bed, on a similar scale to other variables
          
display(H1.icu <- lmer(diedicu ~  IMlo + prop.occ.c + I(transin.perbed.perday*7) +                         N.dc.perbed + clinical.NHD.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)
          
          
# Odds ratios
display(H1.icu)          
print(or.CI(H1.icu),digits=3)
          
###########################
## Model 2  ICU
          
          
display(H2.icu <- lmer(diedicu ~  IMlo*N.dc.perbed + prop.occ.c +  I(transin.perbed.perday*7) + clinical.NHD.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)

display(H2.icu,digits=3)
print(or.CI(H2.icu),digits=3)
          
###########################
## Model 3  ICU
          
display(H3.icu <- lmer(diedicu ~  IMlo*N.dc.perbed + prop.occ.c + I(transin.perbed.perday*7) + IMlo*clinical.NHD.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8), digits=3)
  
display(H3.icu, dig=3)          
print(or.CI(H3.icu),digits=3)
          

###########################
## Model 0  Hospital
          
H0.hosp <- lmer(diedhosp ~  IMlo + prop.occ.c + I(transin.perbed.perday*7) + total.support.staff.perbed + N.dc.perbed + clinical.NHD.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8)
          
display(H0.hosp, digits=3)
print(or.CI(H0.hosp),digits=3)
          
          
###########################
## Model 1  Hospital
          
H1.hosp <- lmer(diedhosp ~  IMlo + prop.occ.c + I(transin.perbed.perday*7) +                          N.dc.perbed + clinical.NHD.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8)
          
display(H1.hosp, digits=3)
print(or.CI(H1.hosp),digits=3)
          
  ###########################
## Model 2  Hospital
          
          
H2.hosp <- lmer(diedhosp ~  IMlo*N.dc.perbed + prop.occ.c + I(transin.perbed.perday*7) +                           clinical.NHD.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8)
          
display(H2.hosp, digits=3)
print(or.CI(H2.hosp),digits=3)
          
###########################
## Model 3  Hospital
          
H3.hosp <- lmer(diedhosp ~  IMlo*N.dc.perbed + prop.occ.c + I(transin.perbed.perday*7) +                           IMlo*clinical.NHD.perbed + intensivist + IMlomean.x + I(avyulos/100)  + (1|trust.code), data=dta4, family=binomial(), na.action=na.omit, subset=ss8)
          
display(H3.hosp, digits=3)
print(or.CI(H3.hosp),digits=3)
          
          
