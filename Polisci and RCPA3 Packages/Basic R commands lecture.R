install.packages("poliscidata")
library(poliscidata)
library(ggplot2)
library(tidyverse)
#shows all variables
names(gss)
names(world)
names(nes)
names(states)

#displays data frame
View(gss)

#######################freq gives descriptive stats, include weight for nes and gss##################
freq(nes$pid_x, nes$wt)
#gss and nes are opinion level data, there is sample bias in the survey for people who agree to respond
#for those reasons we weight our samples, to solve for discrepancies in our samples and make it representative

#freqC changes x axis value labels
freqC(gss$zodiac, gss$wtss)
#weighted mean and median
wtd.median()
wtd.mean()

###############################describe cant take a weight variable#########################
describe(gss$age)

##########################histogram##################################
hist(states$hispanic10, xlab = "Percent of State Hispanic", col="red", main="Histogram of Hispanic Population in 2010")



#########################sortC descending list##########################

sortC(states, state, trnout00)

#sortC ascending list 00' turnout
sortC(states, state, trnout00, F)


#################Classes of variables checks variable type##########################
#factor is categorical/dummy
#ordered factor is ordinal
#numeric is interval
class(gss$grass)
class(nes$obama_therm)
class(states$trnout00)

##############as.numeric transforms it into a numeric variable############################

class(gss$vetyears)
freq(gss$vetyears)

gss$vet=gss$vetyears!= "NONE"

gss$vet.n=as.numeric(gss$vet)
freq(gss$vet.n, gss$wtss)
##what if you want to keep categories as ordinal
gss$vet.ordered=as.numeric(gss$vetyears)

freq(gss$vet.ordered, gss$wtss)
#lets make an ordinakl into a dummy
freq(nes$presapp_econ_x, nes$wt)
freqC(nes$presapp_econ_x, nes$wt)


nes$presapp.econ.1=nes$presapp_econ_x== "1. Approve strongly"
nes$presapp.econ.2=nes$presapp_econ_x== "2. Approve not strongly"

#lets see they are dummies now
freqC(nes$presapp.econ.1, nes$wt)
freqC(nes$presapp.econ.2, nes$wt)
#then combine, it means 0s are everything else and 1s are either approve strongly or approve not strongly
nes$presapp.econ= nes$presapp.econ.1+ nes$presapp.econ.2
freqC(nes$presapp.econ, nes$wt)

#numeric to ordered
describe(gss$authoritarianism)
class(gss$authoritarianism)
gss$auth1=as.ordered(gss$authoritarianism)
freq(gss$auth1, gss$wtss)
class(gss$auth1)

#numeric to factors, still looks same but R labels it a factor

gss$auth2=as.factor(gss$authoritarianism)
freq(gss$auth2)

#############################################levels()##############################
levels(gss$grass)
gss$grass.yesno=gss$grass

levels(gss$grass.yesno)= c("Yes", "No")
levels(gss$grass.yesno)

#interval to ordinal with buckets and age
cutpoints= c(31,41,51,61,90)
gss$age5=cut2(gss$age, cutpoints)
describe(gss$age5)
class(gss$age5)
gss$age5=as.ordered(gss$age5)
levels(gss$age5)=c("18-30", "31-40", "41-50", "51-60", "61 and over")
freqC(gss$age5, gss$wtss)


#this time no object, just tell r how many buckets

gss$popage=cut2(gss$age, g=5)
freqC(gss$popage, gss$wtss)


#combine different ordinal labels into the same bucket eg strongapp to app and strong disapp to disapp

levels(nes$congapp_job_x)
nes$congapprove= nes$congapp_job_x
levels(nes$congapprove)=c("Approve", "Approve", "Disapprove", "Disapprove")
freqC(nes$congapp_job_x, nes$wt)
freqC(nes$congapprove, nes$wt)
############################### Crosstab o'clock

#make sure you have variables coded correctly before cross tabs, transform first.

#xtp(data, DV, IV [optional weight, plot.options, chisq=T/F])
xtp(states, obama_win12, obama_win08, ylab="'08 Obama win", xlab = "'12 Obama win")

levels(states$obama_win08)=c("No","Yes")


#can also do stat tests on xtabs like chi squared
xtp(nes, envjob_3, dem_educ3, wt)
xtp(nes, envjob_3, dem_educ3, wt, chisq= T)




#plotmeansC(data, ~DV, ~IV, DV~IV, [optional:w=~weight, plot.options])
#creates linegrphs, works on all IVs but DVs must be numeric form
plotmeansC(nes, ~ft_hclinton, ~pid_x, ft_hclinton~pid_x, w=~wt, xlab="Party ID", 
           ylab="Clinton Feeling Thermometer", main="H Clinton ratings \n by Party ID")

#compmeans(DV, IV, [optional: weight, plot options])
compmeans(nes$ft_hclinton, nes$south, nes$wt)


#stripcharts(Dv~IV, data=, [optional:plot options])
stripchart(literacy~democ_regime, data=world)
stripchart(literacy~democ_regime, data=world, method="jitter")
stripchart(literacy~democ_regime, data=world, method="stack")
?ggplot2


##Design datasets, you cant see it but its there, its like a zip file
###########################UPDATING GSS DOESNT UPDATE DESIGN, YOU MUST RERUN LINE OF CODE##############################
gssD=svydesign(id=~1, data=gss, weights=~wtss)



#####xtabC(~DV, +IV, +CV, design.dataset)
####adds a control vars to your xtab, a controlled comparison

#this table means given a states gun restrictions, sorted by region, how mant restrictions on abortion do they have
#ex100% of states in the northeast with moderate restrictions on guns have between 0-5 abortion restrictions
xtabC(~abortlaw3 +region+ gun_rank3, statesD)
#run three xtabs by each level
#NaN is not a number
#doesn't work well with continous variables
xtabC(~abortlaw3+romney2012+ gun_rank3, statesD)


##################iplotC(~DV, ~IV+CV, design.dataset, DV~CV+IV, [option:plot.options])
#makes multiplemlines on a plot of a controlled comparison
#notice how IV and CV switch positions mid way in command
levels(gss$voted08)
class(gss$voted08)
gss$vote08.d=as.numeric((gss$voted08=="Voted"))

freq(gss$vote08.d, gss$wtss)
#remember to do the "==value" for the one you want to be 1


##now because we want to graph percentages ( we can plan head to make y axis to be 0-100)
gss$vote08.d=100*gss$vote08.d
freq(gss$vote08.d, gss$wtss)
#update design dataset
gssD=svydesign(id=~1, data=gss, weights=~wtss)

levels(gss$age2)
levels(gss$degree)
?xtabC
xtabC(~vote08.d+ degree + age2, gssD)

iplotC(~vote08.d, ~degree+age2, gssD, vote08.d~age2+degree,
       xlab="Education", ylab="Percent who voted in '08",
       main=list("Percentage 08 voter \n by education and age", cex=1))
legend("bottom",
       legend=c("<=30", ">=31"),
       lty=c(1,2),
             lwd=2,
             title="Age",
             inset=0,1,
             bty="n")
#legend(location, keged, legend.options)

##imeansC(~DV, ~IV+CV, design.dataset)
#this gives us a controlled comparison of the means
#no picture, no pvalue (ttest diff of means)
describe(nes$ftgr_feminists)
imeansC(~ftgr_feminists, ~gender+married, nesD)
#can plit here for mutliple line charts
iplotC(~ftgr_feminists, ~gender+married, nesD, ftgr_feminists~married+gender,
       xlab="Gender", ylab="Rating of Feminists", main=list("Feminists Ratings by Gender and Marital Status"))
legend("topleft", legend=c("No", "Yes"), lty=c(1,2), lwd=2,
       title="Married?", inset=.1,bty="n")

iplotC(~modsex_scale, ~link_wom_scale+dem_raceeth2, nesD,
       modsex_scale~dem_raceeth2+link_wom_scale,
       xlab="Linked Fate Scale", ylab="Modern Sexism Scale",
       main=list("Modern Sexism Linked Fate with Race"))

imeansC(~modsex_scale, ~link_wom_scale+dem_raceeth2, nesD)

legend("topleft", legend=c("Black", "White"), lty=c(2,1), lwd=2, title= "Race",
       inset=.1, bty="n")

#######################wtd.t.test(x, test value, [option weight=])
describe(nes$ftgr_liberals)
wtd.t.test(nes$ftgr_liberals, 0, weight=nes$wt)
wtd.t.test(nes$ftgr_liberals, 50, weight=nes$wt)
wtd.t.test(nes$ftgr_liberals, 47, weight=nes$wt)

##############################CI95(mean, se.mean)

CI95(46.917, .345)

#################wtd.ttestC(~DV, ~IV, design.dataset)
wtd.ttestC(~ft_rep, ~gender, nesD)

###############diff of proptions
#prop.testC(DV, IV, [optionalw=weight variable])
levels(nes$pid_3)
nes$dem=as.numeric(nes$pid_3=="Dem")
prop.testC(nes$dem, nes$gender, w=nes$wt)



#######subset(data, subset.definition)
nes.women=subset(nes, gender=="Female")
nes.men=subset(nes, gender=="Male")








########wtd.cor(x1,x2,x3[optional weight=])
gss$grass.n=as.numeric(gss$grass)
gss$degree.n=as.numeric(gss$degree)
freq(gss$grass.n)
wtd.cor(gss$degree.n, gss$grass.n, weight=gss$wtss)

#as education increases support for legalization increases



#######cbind(vector elements)
variableSet= cbind(gss$degree.n, gss$grass.n, gss$age_5)
wtd.cor(variableSet, weight=gss$wtss)


###cool visual
variableNames=c("Liberals", "Feminists", "Big Business", "Military", "Atheists")
pairs(variableSet, labels=variableNames, cex=.8, cex.labels=1.5)


##scatterplot
scatterplot(world$unions, world$gdp_10_thou)
