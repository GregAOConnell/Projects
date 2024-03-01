#linear HW R
library(poliscidata)
library(tidyverse)
#######################Question 1###############################3
#Creating the dummy
levels(nes$own_dog)
nes$own_dog.d <- as.numeric((nes$own_dog=="Yes"))

#yes is 1 and check work
freq(nes$own_dog.d)
freq(nes$own_dog)
#add weight with design dset
nesD=svydesign(id=~1, data=nes, weights=~wt)

#regression
svyglm(fedspend_scale~own_dog.d, design=nesD, na.action="na.omit")
#statistical significance
summary(svyglm(fedspend_scale~own_dog.d, design=nesD, na.action="na.omit"))





################Question 2#######################################
#simple linear model, summary to get significance values
lm(abortlaw10~permit+womleg_2015, data=states)
summary(lm(abortlaw10~permit+womleg_2015, data=states))

#################Question 3#####################################
#start with gssD
gssD=svydesign(id=~1, data=gss, weights=~wtss)
#now regress
summary(svyglm(authoritarianism~age+social_trust, design=gssD, na.action="na.omit"))
?gss


###############Question 4#######################################
#make the model first

blkleg.model <- lm(blkleg~blkpct10+south, data=states)
summary(blkleg.model)

#plot
plot(x=states$blkleg, y=states$blkpct10,
     xlab = "Percentage of Black Legilsators in each State Government",
     ylab="Proportion of State population that is Black",
     main="Proportion of Black Population Correlation with Proportion of Black Legislators \n for Southern and Non-Southern States")
#stick the two lines in, first south then nonsouth
abline(blkleg.model, lty=2)

abline(a=-.77698-3.01005, b=.82403, lty=1)

#now we subset and shade southern points
states.south=subset(states, south=="South")
points(x=states.south$blkleg, y=states.south$blkpct10, pch=16)

#And two legends for our lines and points
legend(x=1, y=15,pch=c(1,16), cex=.8, bty="n", 
       legend=c("Non Southern States", "Southern States"))
legend(x=1, y=20,lty=c(1, 2), cex=.8, bty="n", 
       legend=c("Non Southern States", "Southern States"))


###################################Question 5##############################

#make model and find out who is reference category
labor.model=lm(free_labor~regionun, data=world)
summary(labor.model)
levels(world$regionun)

view(world$free_labor)
#creating the CI plot
prediction.frame=expand.grid(regionun=levels(world$regionun))
predictions=predict.lm(labor.model, newdata=prediction.frame, interval="confidence", level=.95)
predictions
plot(x=1:6, y=predictions[,"fit"], ylim = c(0,100), axes=F, xlab="Region", ylab="Labor Freedom", pch=16, main="Labor Freedom by Region")
segments(x0=1:6, y0=predictions[,"lwr"], x1=1:6,
         y1=predictions[,"upr"])
axis(side=1, at=1:6, labels=prediction.frame[,1], cex.axis=.7)
axis(side=2, las=1)

