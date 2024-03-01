##Linear Regression R assignment
library(poliscidata)
install.packages("mmtable")

library(tidyverse)
library(mmtable)

###############################Linear Models with one or multuple IVs
######comands include, relevel(), lm(), (weighted data) svyglm()

#simple linear model lm(DV~IV, data=x)
lm(womleg_2015~college, data=states)
#summary(lm()) if you want more info
summary(lm(womleg_2015~college, data=states))

#multiple regressions
#more than one IV
#lm(DV~IV+CV, data==)
describe(states$attend_pct)
summary(lm(womleg_2015~college+ attend_pct, data=states))


#multiple levels categorical IV
#regionun is 6 categorical names
describe(world$gender_unequal)
describe(world$regionun)
freq(world$regionun)
summary(lm(gender_unequal~regionun, data=world))

#releveling
levels(world$regionun)
world$regionun.usref=relevel(world$regionun, ref= "USA/Canada")
levels(world$regionun.usref)
summary(lm(gender_unequal~regionun.usref, data=world))


#weighted dataset
nesD=svydesign(id=~1, data=nes, weights=~wt)
describe(nes$obama_therm, weights=nes$wt)
freq(nes$gender, nes$wt)
svyglm(obama_therm~gender, design=nesD, na.action="na.omit")
#gives extra info with summary()
summary(svyglm(obama_therm~gender, design=nesD, na.action="na.omit"))
#to get fit stats(r2, and adjusted r2), fit.svyglm(svyglm(...))
fit.svyglm(svyglm(obama_therm~gender, design=nesD, na.action="na.omit"))


#now multiple regression on weighted data
describe(nes$fedspend_scale, weights=nes$wt)
fedspend.model1=svyglm(fedspend_scale~ft_dem, design=nesD, na.action="na.omit")
summary(fedspend.model1)
fit.svyglm(fedspend.model1)



#add a second IV
fedspend.model2=svyglm(fedspend_scale~ft_dem+polknow_combined, design=nesD, na.action="na.omit")
summary(fedspend.model2)



#adding an interaction term
nes$relig_import.n=as.numeric(nes$relig_import=="1. Important")

nesD=svydesign(id=~1, data=nes, weights=~wt)
summary(svyglm(abort_scale~ftgr_xian+relig_import.n+ftgr_xian*relig_import.n, design=nesD, na.action="na.omit"))

##interpreting beta for interaction term
##########the adjustment you need to make to the beta of one constituent term for every increase in the other constituent term

#you add this adjustment to both other betas, in this example to the 
#christian feeling therm and the dummy for the importance of religion






#with nomina IV first
gender.model=lm(gender_unequal~regionun, data=world)
#now use model to generate DV results
prediction.frame=expand.grid(regionun=levels(world$regionun))


####EXPAND.GRID DOESNT WORK WITH NUMERIC VARIABLES
#making an object with estimate of y from regression, making 95 CIs too
#finding specific estimates of something
predictions=predict.lm(gender.model, newdata=prediction.frame, interval="confidence", level=.95)
predictions

plot(x=1:6, y=predictions[,"fit"], ylim=c(0,1), axes=F, xlab="Region", ylab="Gender Inequality", pch=16, main="World Gender Inequality")

#ad cis
segments(x0=1:6, y0=predictions[,"lwr"], x1=1:6,
         y1=predictions[,"upr"])
#now lcean up the grpah
axis(side=1, at=1:6, labels=prediction.frame[,1], cex.axis=.7)
axis(side=2, las=1)



#both Ivs are nominal or ordinal
obama.model=svyglm(obama_therm~gender+pid_3, design=nesD,
                   na.action="na.omit")
prediction.frame=expand.grid(gender=levels(nes$gender), pid_3=levels(nes$pid_3))
prediction.frame

predictions=predict.lm(obama.model,newdatga=prediction.frame,
                       interval="confidence", level=.95)
plot(x=1:6, y=predictions[,"fit"], ylim=c(0,100), axes=F, xlab="Individuals", 
     ylab="Feeling about Obama", main="Obama Approval \n by gender and party", pch=16)
