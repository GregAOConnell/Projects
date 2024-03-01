###########LogitHW

library(poliscidata)
#question 1

#design dataset with new vars
nes$dem_edugroup.n=as.numeric(nes$dem_edugroup)
nes$dem_age_group_r.n=as.numeric(nes$dem_age_group_r) 
nesD=svydesign(id=~1, data=nes, weights=~wt)


#model
levels(nes$dem_edugroup)
modelA <- svyglm(voted2012~dem_edugroup.n+dem_age_group_r.n, design=nesD, family="quasibinomial")
summary(modelA)
#odds ratios
orci(modelA)
logregR2(modelA)

############question 4
#make numeric and add to nesD
nes$dhsinvolv_message.n=as.numeric(nes$dhsinvolv_message=="1. Have done this in past 4 years")
nes$polknow3.n=as.numeric(nes$polknow3=="High know")
nesD=svydesign(id=~1, data=nes, weights=~wt)

#now we make the model
modelB <- svyglm(dhsinvolv_message.n~polknow3.n, design=nesD, family="quasibinomial")
summary(modelB)
orci(modelB)


## plotting time, but first we need to organize some vars
polknow.level=seq(1,3, by=1)


predicted.logged.odds= -1.38581+0.35367*polknow.level
predicted.probabilities=inverse.logit(predicted.logged.odds)
plot(x="", y="", xlim=c(1,3), ylim=c(0,1),
     xlab="Level of Poltiical Knowledge",
     ylab="Probability of posting politics on social media",
     main="Probability of posting political messages on social media based on levels of political knowledge")
lines(x=polknow.level, y=predicted.probabilities)

#lets find the 3 points
#polknow1
inverse.logit(-1.38581+0.35367)
#polknow2
inverse.logit(-1.38581+0.35367*2)
#polknow3
inverse.logit(-1.38581+0.35367*3)


?svyglm
#############################Question 5###################
#start with design data
levels(world$democ_regime)
world$democ_regime.n=as.numeric(world$democ_regime=="Yes")
worldD=svydesign(id=~1, data=world)

#now we make a logit model
modelC <- svyglm(democ_regime.n~frac_eth+gdp08, design=worldD, family="quasibinomial")
summary(modelC)
orci(modelC)

#calculating 5b without gdp
frac.levels <- c(0,1)
predicted.logged.odds= 1.157761-1.848008*frac.levels
predicted.probabilities=inverse.logit(predicted.logged.odds)

#one unit change in frac's impact
predicted.probabilities
#frac=0-frac=1 is the impact of a one unit change
0.7609256-0.3339781



#lets find the min max gdp and control for ethnic fracturing


max_gdp <- 14200.0
min_gdp <- .8
gdp.range=seq(0,15000, by=1)

#predicted probs
predicted.logged.odds_noeth <- 1.157761+0.000240*gdp.range
predicted.probabilities_noeth=inverse.logit(predicted.logged.odds_noeth)
predicted.probabilities_noeth

#one unit change in GDP values
 0.7610042-0.7609606
 #no real discerable change in Y

#6 now we plot
 
#first make a line equation with high eth frac and one with low
predicted.logged.odds_higheth <-  1.157761-1.848008+0.000240*gdp.range
predicted.probabilities_higheth=inverse.logit(predicted.logged.odds_higheth)


plot(x="", y="", xlim=c(0,15000), ylim=c(0,1),
     xlab="GDP in 2008 measured in millions of $",
     ylab="Probability of being a democratic regime",
     main="Probability of a democratic regime \n based on GDP and Ethnic Fracturing")
lines(x=gdp.range, y=predicted.probabilities_higheth, lty=1)
lines(x=gdp.range, y=predicted.probabilities_noeth, lty=2)

#And two legends for our lines and points

legend(x="bottomleft", y=50, lty=c(1,2), cex=.8, bty="n", 
       legend=c("High Ethnic Fracturing", "Low Ethnic Fracturing"))


#finding the switchover point with ALEGBRA (rad)


#0= 1.157761-1.848008+(.000240*gdp.range)
#final answer for switchover is 2876.029
-(1.157761-1.848008)/.00024





#gdp mean
describe(world$gdp08)
gdp.mean <- 439.9

#prediction Frame for holding the mean constant
prediction.frame=expand.grid(frac_eth=frac.levels, gdp08=gdp.mean)

prediction.frame
#plots our points
predictions=predict.glm(modelC, newdata=prediction.frame, 
                       type="response", se.fit=T, interval="confidence",level=.95)
predictions
plot(c(1,2), y=predictions$fit, ylim=c(0,1), axes=T,
     xlab="Levels of Ethnic Fracturing",
     ylab="Probability of Democratic Regime", main="Holding GDP at the Mean, the effect of \n Ethnic Fracturing on Probability of Democratic Reimges", pch=16
)

#lets make our confidence interval lines too
segments(x0=1:2 , y0=predictions$fit-1.96*predictions$se.fit ,
         x1=1:2 , y1=predictions$fit+1.96*predictions$se.fit)
axis(side=1, at=1:2, labels=c("Low Levels of Ethnic Fracturing", "High Levels of Ethnic Fracturing"))
