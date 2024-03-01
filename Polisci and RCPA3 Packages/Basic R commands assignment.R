#320 HW R basic commands

library(poliscidata)
library(tidyverse)
#1
names(states)
#2
view(world$gini10)
#3
view(states$abortlaw10)
freq(states$abortlaw10)
freq?


#4
describe(world$women13)
sortC(world, country, women13)
wtd.hist(world$women13, xlab="Percentage of Women in Legislature", 
         main="Percentage of Female Legislators in 90 democracies", col="lightblue")
#5
describe(gss$science_quiz)
freq(gss$science_quiz, gss$wtss)
describe(gss$wordsum)
freq(gss$wordsum, gss$wtss)
?gss

#6
freq(gss$polviews, gss$wtss)
levels(gss$polviews)
#a
gss$polview.n=as.numeric(gss$polviews, gss$wtss)
freq(gss$polview.n, gss$wtss)
cutpoints=c(4,5)
gss$polview.3=cut2(gss$polview.n, cutpoints)
freqC(gss$polview.3, gss$wtss)
#b
levels(gss$polview.3)=c("Liberal", "Moderate", "Conserative")
gss$polview.3=as.ordered(gss$polview.3)
freqC(gss$polview.3, gss$wtss)

#7
class(gss$grass)
levels(gss$grass)
gss$grass.yes=as.numeric(gss$grass== "LEGAL")

freqC(gss$grass.yes, gss$wtss)
levels(gss$grass.yes)=c("NOT LEGAL", "LEGAL")
levels(gss$grass.yes)

#8
compmeans(nes$fedspend_scale, nes$pid_x, nes$wt)
?nes

#9 
?plotmeansC
plotmeansC(nes, ~fedspend_scale, ~pid_x, fedspend_scale~pid_x, w=~wt,
           main="Pro-government Spending Scale, by Party ID", col="purple", xlab="Party ID",
           ylab="Pro-spending scale score")

#10
xtabC(~gay_marry+libcon3, nesD)
nesD=svydesign(id=~1, data=nes, weights=~wt)

#11

worldD=svydesign(id=~1, data=world)
xtabC(~democ_regime +frac_eth3 +gdp_cap2, worldD)
levels(world$democ_regime)
world$democ.yes=as.numeric(world$democ_regime=="Yes")*100
worldD=svydesign(id=~1, data=world)
iplotC(~democ.yes, ~frac_eth3+gdp_cap2, worldD, democ.yes~gdp_cap2+frac_eth3,
       xlab= "Fractionalization", ylab="Percentage of Democracies", main="Percentage of Democracies by \n Fractionalization and GDP")
legend("topleft", legend=c("High GDP", "Low GDP"), lty=c(2,1), lwd=2, title= "GDP",
       inset=.1, bty="n")

#12
imeansC(~ftgr_tea, ~dhs_threat3+polknow3, nesD)

#13
wtd.t.test(gss$egalit_scale, 0, weight=gss$wtss)

#13a
CI95(19.345, .304)
#14
wtd.ttestC(~int_info_scale, ~age2, gssD)
#15
levels(gss$partyid_3)
gss$dem=as.numeric(gss$partyid_3=="Dem")
prop.testC(gss$dem, gss$sex, gss$wtss)
#16
gssD.lowed=subset(gssD, educ_2=="0-12 yrs")
gssD.highed=subset(gssD, educ_2=="13+ yrs")
gssD=svy

xtabC(~partyid_3 +egalit_scale3, gssD.lowed)
xtabC(~partyid_3 +egalit_scale3, gssD.highed)

#17
variablegroup=cbind(states$demhr11, states$demstate13, states$union10)
wtd.cor(variablegroup)

#checking work for 17 (I did it before we learned the easy way to do it in class)
wtd.cor(states$demhr11, states$demstate13)
wtd.cor(states$demhr11, states$union10)
wtd.cor(states$demstate13, states$demhr11)
wtd.cor(states$union10, states$demstate13)
?wtd.cor
