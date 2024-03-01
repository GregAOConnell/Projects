#############Logit Land############

library(poliscidata)


##syntax is svyglm(DV~IV+IV.. design=design.dataset, family="quasibinomial")
describe(nes$obama_vote, weights=nes$wt)
freq(nes$obama_vote, nes$wt)
obama.logit=svyglm(obama_vote~ft_dem, design=nesD, family="quasibinomial")
summary(obama.logit)



##with mutiple IVS
describe(nes$white, weights=nes$wt)
describe(nes$owngun_owngun, weights=nes$wt)
obama.logit2=svyglm(obama_vote~ft_dem+white+(owngun_owngun=="1. Yes"), design=nesD,
                    family="quasibinomial")
summary(obama.logit2)


##Odds ratio
orci(obama.logit)

ft_dem.values=seq(0,100, by=1)
predicted.logged.odds= -4.665066+.093648*ft_dem.values

predicted.probabilities=inverse.logit(predicted.logged.odds)
predicted.probabilities


##and plot
plot(x="", y="", xlim=c(0,100), ylim=c(0,1),
     xlab="Feelings towards Obama",
     ylab="Probability of voting for Obama")
lines(x=ft_dem.values, y=predicted.probabilities)

levels(nes$polknow3)
##plug and chug with dummy variables
#white gun owner
log.odds.white.gunown=-3.2267+.089366*ft_dem.values-1.229075-.0569815
#white nongun owner
log.odds.white.nongun=-3.2267+.089366*ft_dem.values-1.229075

#nonwhite nongunowner
log.odds.nonwhite.nongun=-3.2267+.089366*ft_dem.values

#nonwhite gunowner
log.odds.nonwhite.gunown=-3.2267+.089366*ft_dem.values-.0569815

#replot with all 4 lines

plot(x="", y="", xlim=c(0,100), ylim=c(0,1),
     xlab="Feelings towards Obama",
     ylab="Probability of voting for Obama")
# all 4 lines
lines(x=ft_dem.values, y=inverse.logit(log.odds.white.gunown), lty=1)

lines(x=ft_dem.values, y=inverse.logit(log.odds.white.nongun), lty=2)

lines(x=ft_dem.values, y=inverse.logit(log.odds.nonwhite.gunown), lty=3)

lines(x=ft_dem.values, y=inverse.logit(log.odds.nonwhite.nongun), lty=4)

ft_dem.mean=wtd.mean(nes$ft_dem, nes$wt)
prediction.frame=expand.grid(ft_dem=ft_dem.mean, white=levels(nes$white),
                             owngun_owngun=levels(nes$owngun_owngun))

prediction.frame
predictions=predict.glm(obama.logit2, newdata=prediction.frame, type="response", se.fit=T, interval="confidence", level=.95)
plot(1:4, y=predictions$fit, ylim=c(0,1), axes=F,
     xlab="Voter Types with mean Dem Part Sentiment",
     ylab="Probability of Obama Vote", main="Mean Dem Voters and Obama Vote", pch=16
)
segments(x0=1:4 , y0=predictions$fit-1.96*predictions$se.fit ,
           x1=1:4 , y1=predictions$fit+1.96*predictions$se.fit)
axis(side=1, at=1:4, labels=c("Nonwhite gun owner", "White gun owner", "nonwhite nongun", "nonwhite gun"),)

logregR2(obama.logit)
