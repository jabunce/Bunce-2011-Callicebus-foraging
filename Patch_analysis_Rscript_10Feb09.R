###############Contents
#This script fits multilevel Poisson regressions for YOR patch encounters and
#all patch encounters

######YOR Patch Analysis
#Step1: Empty model with random effect for group
#Step2: Simple model with sex as a predictor
#Step3: Basal model with Vision predictor
#Step3.5: Full model with predictors and interactions
#Step4: Reduce full model, eliminate Sex:InfDep interaction
#Step5: Reduce full model, eliminate Sex:Season interaction
#Step6: Reduce full model, eliminate InfDep:Season interaction
#Step7: Reduce full model, eliminate Vision:Season interaction
#Step8: Reduce full model, eliminate Season predictor
#Step9: Model comparison
#Step10: Two best models with fixed offset
#Step11: Overdispersion analysis for Model.YOR.reduced3.off
#Step12: Residual analysis for Model.YOR.reduced3.off

######All Patch Analysis
#Step1: Empty model with random effect for group
#Step2: Simple model with sex as a predictor
#Step3: Basal model with Vision predictor
#Step3.5: Full model with predictors and interactions
#Step4: Reduce full model, eliminate InfDep:Season interaction
#Step5: Reduce full model, eliminate Vision:InfDep interaction
#Step6: Reduce full model, eliminate Vision:Season interaction
#Step7: Model comparison
#Step8: fix the exposure time offset in best model
#Step9: Overdispersion analysis for Model.All.reduced2
#Step10: quasi-poisson model for overdispersion
#Step11: Residual analysis for Model.All.reduced2

###########################


#Read the data from the csv data file into R:
Patches.raw <- read.csv(file="./Data/PatchData_10feb09.csv", header=TRUE)

#Check the variable names and dimensions in the data frame Patches.raw
names(Patches.raw)
dim(Patches.raw)

#Attach the data frame
attach(Patches.raw)

#ln of the exposure time = the "offset" in Poisson model (pg111-112)
#	= ln(fraction of a 12h day observed traveling)
#ln(exposure time) can also be included as a predictor in the Poisson
#	regression, allowing its coefficient to be estimated from the
#	data, rather than being fixed at 1, as it is when used as an
#	offset (pg112)

log.TimeTraveled <- log(TimeTravelday)

#Center log.TimeTraveled on its mean, i.e. the mean amount of time per day,
#in units of 12h days, that the animals were observed to be traveling. 

c.log.TimeTraveled <- log.TimeTraveled - mean(log.TimeTraveled)

#mean observed travel time per day
exp(mean(log.TimeTraveled))*12*60


###########################################################################
#########################YOR patch analysis################################


##############Step1: Empty model with random effect for group

Model.YOR.empty <- glmer(YORTotal ~ c.log.TimeTraveled + (1 | GroupName),
	family = poisson)

summary(Model.YOR.empty)

coef(Model.YOR.empty)
table(YORTotal, GroupName)


###############Step2: Simple model with sex as a predictor (0=female, 1=male)

Model.YOR.sex <- glmer(YORTotal ~ c.log.TimeTraveled + InvSexCode +
		(1 | GroupName), family=poisson)

summary(Model.YOR.sex)

table(YORTotal, InvSexCode)


###############Step3: Basal model with Vision predictor (1=tri, 0=di)

#Vision is actually an interaction between vision type and sex, due to the
#fact that males can only be dichromats whereas females can be di or 
#trichromats

Model.YOR.base <- glmer(YORTotal ~ c.log.TimeTraveled + InvSexCode +
		VisionCode + (1 | GroupName), family=poisson)

summary(Model.YOR.base)


##############Step3.5: Full model with predictors and interactions for InfDep and Season

#InvInfDepCode: 0=heavy nursing/carrying
#		    1=light/no nursing/no carrying

#SeasonCode: 0=dry, 1=wet 

Model.YOR.full <- glmer(YORTotal ~ c.log.TimeTraveled +
			InvSexCode + VisionCode+ InvInfDepCode + SeasonCode+
			InvSexCode:InvInfDepCode +
			InvSexCode:SeasonCode + VisionCode:InvInfDepCode +
			VisionCode:SeasonCode + InvInfDepCode:SeasonCode +
			(1 | GroupName), family=poisson)

summary(Model.YOR.full)


##############Step4: Reduce full model, eliminate Sex:InfDep interaction

#Negative coef of this interaction means changing from lactating to
#non-lactating gives the female even more of an encounter advantage. Expect
#males to converge with females when not carrying infant, so sign of the
#interaction is unexpected. Highest p-value. Eliminating reduces AIC.

Model.YOR.reduced1 <- glmer(YORTotal ~ c.log.TimeTraveled +
			InvSexCode + VisionCode+ InvInfDepCode + SeasonCode +
			InvSexCode:SeasonCode + VisionCode:InvInfDepCode +
			VisionCode:SeasonCode + InvInfDepCode:SeasonCode +
			(1 | GroupName), family=poisson)

summary(Model.YOR.reduced1)


##############Step5: Reduce full model, eliminate Sex:Season interaction

#Positive coef of this interaction means changing from dry to wet season
#decreases female encounter rate advantage over male. This makes sense, but
#the effect is tiny. Second highest p-value. P-value of Season is higher but
#Season has several interactions with larger effects. AIC is reduced.

Model.YOR.reduced2 <- glmer(YORTotal ~ c.log.TimeTraveled +
			InvSexCode + VisionCode+ InvInfDepCode + SeasonCode +
			VisionCode:InvInfDepCode +
			VisionCode:SeasonCode + InvInfDepCode:SeasonCode +
			(1 | GroupName), family=poisson)

summary(Model.YOR.reduced2)


##############Step6: Reduce full model, eliminate InfDep:Season interaction

#Positive coef of this interaction means changing from dry to wet season
#increases the advantage of non-lactating over lactating animal. Diets
#expected to converge in the wet season, not diverge. Largest p-value and
#smallest |coef| other than Season. AIC is reduced.

Model.YOR.reduced3 <- glmer(YORTotal ~ c.log.TimeTraveled +
			InvSexCode + VisionCode+ InvInfDepCode + SeasonCode +
			VisionCode:InvInfDepCode +
			VisionCode:SeasonCode + 
			(1 | GroupName), family=poisson)

summary(Model.YOR.reduced3)


##############Step7: Reduce full model, eliminate Vision:Season interaction

#Negative coef of this interaction means changing from dry to wet decreases
#the advantage of tri over dichromacy. This makes sense. Try eliminating
#due to non-significance. AIC rises a little.

Model.YOR.reduced4 <- glmer(YORTotal ~ c.log.TimeTraveled +
			InvSexCode + VisionCode+ InvInfDepCode + SeasonCode +
			VisionCode:InvInfDepCode +
			(1 | GroupName), family=poisson)

summary(Model.YOR.reduced4)


##############Step8: Reduce full model, eliminate Season predictor

#Negative coef of Season now means that patch encounter advantage in the dry
#rather than the wet season. This is unexpected. Highest p-value. AIC reduced.

Model.YOR.reduced5 <- glmer(YORTotal ~ c.log.TimeTraveled +
			InvSexCode + VisionCode+ InvInfDepCode +
			VisionCode:InvInfDepCode + 
			(1 | GroupName), family=poisson)

summary(Model.YOR.reduced5)


##############Step9: Model comparison

#Model.YOR.reduced5 has the lowest AIC, but has higher residual deviance
#than Model.YOR.reduced3.

anova(Model.YOR.full, Model.YOR.reduced1, Model.YOR.reduced2,
	Model.YOR.reduced3, Model.YOR.reduced4, Model.YOR.reduced5)

anova(Model.YOR.reduced3, Model.YOR.reduced5)

#Anova indicates Model.YOR.reduced5 slightly better than Model.YOR.reduced3.
#However, because all coefs are interpretable in Model.YOR.reduced3 and
#the residual deviance is more than 2 less than that of Model.YOR.reduced5
#even though there are only 2 more predictors in Model.YOR.reduced3 (G&H 100),
#we will accept Model.YOR.reduced3. 


##############Step10: Two best models with fixed offset

#instead of estimating the coef of exposure time from the data, fix it at 1
#as in G&H 111-112. This assumes that a unit increase in the number of patch
#encounters corresponds to a unit increase in observed time traveling. In
#previous models, coefs were estimated at approx. 0.78 

Model.YOR.reduced3.off <- glmer(YORTotal ~
			InvSexCode + VisionCode + InvInfDepCode + SeasonCode +
			VisionCode:InvInfDepCode +
			VisionCode:SeasonCode + 
			(1 | GroupName), family=poisson, offset=log.TimeTraveled)

summary(Model.YOR.reduced3.off)

Model.YOR.reduced5.off <- glmer(YORTotal ~
			InvSexCode + VisionCode+ InvInfDepCode +
			VisionCode:InvInfDepCode + 
			(1 | GroupName), family=poisson, offset=log.TimeTraveled)

summary(Model.YOR.reduced5.off)

#model comparison
anova(Model.YOR.reduced3.off, Model.YOR.reduced5.off)

#Deviance of offset models increases slightly relative to original models, but
#AIC is reduced. Offset Reduced5 is still slightly better than Offset Reduced3,
#but keep Offset Reduced3 due to interpretable predictors. One potential 
#problem of Reduced3 is that the correlation between Vision and InfDep is 0.63
#which means that the effects of Vision and InfDep may be hard to distinguish.


##############Step11: Overdispersion analysis for Model.YOR.reduced3.off

#Hinde and Demetrio 1998
#Overdispersion = residual deviance/degrees of freedom, should be ~1
#Deviance = 183
#d.f. = N - [(# fixed effects + intercept + offset) + (1 for rand effects)]
#d.f. = 214 - (7 + 1 + 1) = 205
#Overdispersion = 183/205 = 0.893, slight underdispersion
#Note: random effect for group probably contirbutes a bit more than 1 and less
#than 5 to the actual degrees of freedom, based on the amount of pooling
#(G&H 525), so the overdispersion is between 0.893 and 0.910 


# #Bates' method (doesn't work with glmer)
# z <- Model.YOR.reduced3.off@deviance["pwrss"] #penalized weighted resid sum of sqrs
# n <- Model.YOR.reduced3.off@dims["n"] #number of observations
# OverDispPara <- z/n
# OverDispPara
# #Overdispersion = 0.963, slight underdispersion


###############Step12: Residual analysis for Model.YOR.reduced3.off

#fitted values from the model on the natural scale
fit.vals <- fitted(Model.YOR.reduced3.off)
fit.vals

#standardized redisuals: (YORTotal-fit.vals)/sqrt(fit.vals)
resid.vals <- resid(Model.YOR.reduced3.off)
resid.vals

#residual plot
plot(fit.vals, resid.vals)
curve(0*x, add=TRUE)

#######Bin the residuals following G&H 559
#join the fitted and residual vectors
fit.resid <- cbind(fit.vals, resid.vals)
fit.resid

#export to excel and bin in 3 ways:
#1) Divide fitted values into bins of 0.1, take mean of resids in each bin
#2) Bin every 5 fitted values and take means of fitted and resids in each bin
#3) Bin every 10 fitted values and take means of fitted and resids in each bin

detach(Patches.raw)

#Read the binned resids from the csv data file into R:
Binned.resids <- read.csv(
	file=
	"./Data/PatchData_11feb09_binned_resids.csv", 
	header=TRUE)

names(Binned.resids)
dim(Binned.resids)
attach(Binned.resids)

#0.1 bins
plot(bin.fit1.0.1bins, bin.resid1) 
curve(0*x, add=TRUE)
#Note: population/bin decreases and thus variance increases w/ increasing x

#Every5 bins
plot(bin.fit2.every5, bin.resid2) 
curve(0*x, add=TRUE)

#Every10 bins
plot(bin.fit3.every10, bin.resid3) 
curve(0*x, add=TRUE)

####Also use residuals to monitor overdispersion, following G&H 114
#Standardized residuals should have mean=0 and stdev=1
#Plots indicate stdev<1, evidence of underdispersion

detach(Binned.resids)




#############################################################################
###############################All Patch Analysis############################

attach(Patches.raw)

##############Step1: Empty model with random effect for group

Model.All.empty <- glmer(PatchTotal ~ c.log.TimeTraveled + (1 | GroupName),
	family = poisson)

summary(Model.All.empty)

coef(Model.All.empty)
table(PatchTotal, GroupName)


###############Step2: Simple model with sex as a predictor (0=female, 1=male)

Model.All.sex <- glmer(PatchTotal ~ c.log.TimeTraveled + InvSexCode +
		(1 | GroupName), family=poisson)

summary(Model.All.sex)

table(PatchTotal, InvSexCode)


###############Step3: Basal model with Vision predictor (1=tri, 0=di)

#Vision is actually an interaction between vision type and sex, due to the
#fact that males can only be dichromats whereas females can be di or 
#trichromats

Model.All.base <- glmer(PatchTotal ~ c.log.TimeTraveled + InvSexCode +
		VisionCode + (1 | GroupName), family=poisson)

summary(Model.All.base)


##############Step3.5: Full model with predictors and interactions for InfDep and Season

#InvInfDepCode: 0=heavy nursing/carrying
#		    1=light/no nursing/no carrying

#SeasonCode: 0=dry, 1=wet 

Model.All.full <- glmer(PatchTotal ~ c.log.TimeTraveled +
			InvSexCode + VisionCode+ InvInfDepCode + SeasonCode+
			InvSexCode:InvInfDepCode +
			InvSexCode:SeasonCode + VisionCode:InvInfDepCode +
			VisionCode:SeasonCode + InvInfDepCode:SeasonCode +
			(1 | GroupName), family=poisson)

summary(Model.All.full)


##############Step4: Reduce full model, eliminate InfDep:Season interaction

#Sign of this interaction means as change from dry to wet season, advantage of
#nonInfDep over InfDep animals is reduced. This makes sense but the effect is
#small and not significant. AIC is reduced.

Model.All.reduced1 <- glmer(PatchTotal ~ c.log.TimeTraveled +
			InvSexCode + VisionCode+ InvInfDepCode + SeasonCode+
			InvSexCode:InvInfDepCode +
			InvSexCode:SeasonCode + VisionCode:InvInfDepCode +
			VisionCode:SeasonCode +
			(1 | GroupName), family=poisson)

summary(Model.All.reduced1)


##############Step5: Reduce full model, eliminate Vision:InfDep interaction

#Sign of this interaction means as change from InfDep to nonInfDep, advantage
#of trichromacy is reduced. This makes sense but the effect is
#small and not significant. AIC is reduced.

Model.All.reduced2 <- glmer(PatchTotal ~ c.log.TimeTraveled +
			InvSexCode + VisionCode+ InvInfDepCode + SeasonCode+
			InvSexCode:InvInfDepCode +
			InvSexCode:SeasonCode +
			VisionCode:SeasonCode +
			(1 | GroupName), family=poisson)

summary(Model.All.reduced2)


##############Step6: Reduce full model, eliminate Vision:Season interaction

#Sign of this interaction means as change from dry to wet season, advantage
#of trichromacy is increased. This makes sense and the effect is almost
#significant. AIC is increased.

Model.All.reduced3 <- glmer(PatchTotal ~ c.log.TimeTraveled +
			InvSexCode + VisionCode+ InvInfDepCode + SeasonCode+
			InvSexCode:InvInfDepCode +
			InvSexCode:SeasonCode +
			(1 | GroupName), family=poisson)

summary(Model.All.reduced3)

#Favor Model.All.reduced2


##############Step7: Model comparison

anova(Model.All.full, Model.All.reduced1, Model.All.reduced2,
	Model.All.reduced3)

#Model.All.reduced2 has lowest AIC and is almost significant by anova


##############Step8: fix the exposure time offset in best model

Model.All.reduced2.off <- glmer(PatchTotal ~
			InvSexCode + VisionCode+ InvInfDepCode + SeasonCode+
			InvSexCode:InvInfDepCode +
			InvSexCode:SeasonCode +
			VisionCode:SeasonCode +
			(1 | GroupName), family=poisson, offset=log.TimeTraveled)

summary(Model.All.reduced2.off)

summary(Model.All.reduced2)

#AIC is slightly lower when exposure time coef is fixed at 1, but
#keep the original Reduced2 model because the estimate for the exposure time
#coef is 0.9 which is pretty good. The variance in the random effect is
#reduced quite a bit when exposure time coef is fixed at 1.


##############Step9: Overdispersion analysis for Model.All.reduced2

#Hinde and Demetrio 1998
#Overdispersion = residual deviance/degrees of freedom, should be ~1
#Deviance = 304.2
#d.f. = N - [(# fixed effects + intercept) + (1 for rand effects)]
#d.f. = 214 - (9 + 1) = 204
#Overdispersion = 304.2/204 = 1.49, overdispersion
#Note: random effect for group probably contirbutes a bit more than 1 and less
#than 5 to the actual degrees of freedom, based on the amount of pooling
#(G&H 525), so the overdispersion is between 1.49 and 1.52 


# #Bates' method (doesn't work with glmer)
# z <- Model.All.reduced2@deviance["pwrss"] #penalized weighted resid sum of sqrs
# n <- Model.All.reduced2@dims["n"] #number of observations
# OverDispPara <- z/n
# OverDispPara
# #Overdispersion = 1.29, overdispersion

# #overdispersion means that the variance estimates for the coef are too low.


# ##############Step10: quasi-poisson model for overdispersion (doesn't work with glmer)

# Model.All.reduced2.quasi <- glmer(PatchTotal ~ c.log.TimeTraveled +
# 			InvSexCode + VisionCode+ InvInfDepCode + SeasonCode+
# 			InvSexCode:InvInfDepCode +
# 			InvSexCode:SeasonCode +
# 			VisionCode:SeasonCode +
# 			(1 | GroupName), family=quasipoisson)

# summary(Model.All.reduced2.quasi)

# summary(Model.All.reduced2)

# #Std. Error estimates for coefs increase slightly. AIC increases.


# ###############Step11: Residual analysis for Model.All.reduced2

# #fitted values from the model on the natural scale
# fit.vals <- fitted(Model.All.reduced2)
# fit.vals

# #standardized redisuals: (YORTotal-fit.vals)/sqrt(fit.vals)
# resid.vals <- resid(Model.All.reduced2)
# resid.vals

# #residual plot
# plot(fit.vals, resid.vals)
# curve(0*x, add=TRUE)

# #residuals look relatively pattern-less, though stdev is slightly > 1
# #95% CI ~ +/-2






##############################################################################
############################Raw Group Means###################################

#############################################Female YOR mean rates


#Group1

Grp1.nurse <- subset(Patches.raw, GroupCode==1 & SexCode==1 & InfDepCode==1)
Grp1.nurse
Rate <- Grp1.nurse$YORTotal/Grp1.nurse$TimeTravelday
Rate
Grp1.nurse.mean.rate <- mean(Rate)
Grp1.nurse.mean.rate
Grp1.YOR.nurse.sum <- sum(Grp1.nurse$YORTotal)
Grp1.YOR.nurse.sum
Grp1.YOR.nurse.days <- length(Grp1.nurse$YORTotal)
Grp1.YOR.nurse.days

Grp1.nonnurse <- subset(Patches.raw, GroupCode==1 & SexCode==1 & InfDepCode==0)
Grp1.nonnurse
Rate <- Grp1.nonnurse$YORTotal/Grp1.nonnurse$TimeTravelday
Rate
Grp1.nonnurse.mean.rate <- mean(Rate)
Grp1.nonnurse.mean.rate
Grp1.YOR.nonnurse.sum <- sum(Grp1.nonnurse$YORTotal)
Grp1.YOR.nonnurse.sum
Grp1.YOR.nonnurse.days <- length(Grp1.nonnurse$YORTotal)
Grp1.YOR.nonnurse.days


Grp1.nurse.mean.rate
Grp1.nonnurse.mean.rate
Grp1.nurse.mean.rate-Grp1.nonnurse.mean.rate


#Group2

Grp2.nurse <- subset(Patches.raw, GroupCode==2 & SexCode==1 & InfDepCode==1)
Grp2.nurse
Rate <- Grp2.nurse$YORTotal/Grp2.nurse$TimeTravelday
Rate
Grp2.nurse.mean.rate <- mean(Rate)
Grp2.nurse.mean.rate
Grp2.YOR.nurse.sum <- sum(Grp2.nurse$YORTotal)
Grp2.YOR.nurse.sum
Grp2.YOR.nurse.days <- length(Grp2.nurse$YORTotal)
Grp2.YOR.nurse.days

Grp2.nonnurse <- subset(Patches.raw, GroupCode==2 & SexCode==1 & InfDepCode==0)
Grp2.nonnurse
Rate <- Grp2.nonnurse$YORTotal/Grp2.nonnurse$TimeTravelday
Rate
Grp2.nonnurse.mean.rate <- mean(Rate)
Grp2.nonnurse.mean.rate
Grp2.YOR.nonnurse.sum <- sum(Grp2.nonnurse$YORTotal)
Grp2.YOR.nonnurse.sum
Grp2.YOR.nonnurse.days <- length(Grp2.nonnurse$YORTotal)
Grp2.YOR.nonnurse.days

Grp2.nurse.mean.rate
Grp2.nonnurse.mean.rate
Grp2.nurse.mean.rate-Grp2.nonnurse.mean.rate


#Group3

Grp3.nurse <- subset(Patches.raw, GroupCode==3 & SexCode==1 & InfDepCode==1)
Grp3.nurse
Rate <- Grp3.nurse$YORTotal/Grp3.nurse$TimeTravelday
Rate
Grp3.nurse.mean.rate <- mean(Rate)
Grp3.nurse.mean.rate
Grp3.YOR.nurse.sum <- sum(Grp3.nurse$YORTotal)
Grp3.YOR.nurse.sum
Grp3.YOR.nurse.days <- length(Grp3.nurse$YORTotal)
Grp3.YOR.nurse.days

Grp3.nonnurse <- subset(Patches.raw, GroupCode==3 & SexCode==1 & InfDepCode==0)
Grp3.nonnurse
Rate <- Grp3.nonnurse$YORTotal/Grp3.nonnurse$TimeTravelday
Rate
Grp3.nonnurse.mean.rate <- mean(Rate)
Grp3.nonnurse.mean.rate
Grp3.YOR.nonnurse.sum <- sum(Grp3.nonnurse$YORTotal)
Grp3.YOR.nonnurse.sum
Grp3.YOR.nonnurse.days <- length(Grp3.nonnurse$YORTotal)
Grp3.YOR.nonnurse.days

Grp3.nurse.mean.rate
Grp3.nonnurse.mean.rate
Grp3.nurse.mean.rate-Grp3.nonnurse.mean.rate


#GroupS

GrpS.nurse <- subset(Patches.raw, GroupCode==4 & SexCode==1 & InfDepCode==1)
GrpS.nurse
Rate <- GrpS.nurse$YORTotal/GrpS.nurse$TimeTravelday
Rate
GrpS.nurse.mean.rate <- mean(Rate)
GrpS.nurse.mean.rate
GrpS.YOR.nurse.sum <- sum(GrpS.nurse$YORTotal)
GrpS.YOR.nurse.sum
GrpS.YOR.nurse.days <- length(GrpS.nurse$YORTotal)
GrpS.YOR.nurse.days

GrpS.nonnurse <- subset(Patches.raw, GroupCode==4 & SexCode==1 & InfDepCode==0)
GrpS.nonnurse
Rate <- GrpS.nonnurse$YORTotal/GrpS.nonnurse$TimeTravelday
Rate
GrpS.nonnurse.mean.rate <- mean(Rate)
GrpS.nonnurse.mean.rate
GrpS.YOR.nonnurse.sum <- sum(GrpS.nonnurse$YORTotal)
GrpS.YOR.nonnurse.sum
GrpS.YOR.nonnurse.days <- length(GrpS.nonnurse$YORTotal)
GrpS.YOR.nonnurse.days

GrpS.nurse.mean.rate
GrpS.nonnurse.mean.rate
GrpS.nurse.mean.rate-GrpS.nonnurse.mean.rate



#GroupW

GrpW.nurse <- subset(Patches.raw, GroupCode==5 & SexCode==1 & InfDepCode==1)
GrpW.nurse
Rate <- GrpW.nurse$YORTotal/GrpW.nurse$TimeTravelday
Rate
GrpW.nurse.mean.rate <- mean(Rate)
GrpW.nurse.mean.rate
GrpW.YOR.nurse.sum <- sum(GrpW.nurse$YORTotal)
GrpW.YOR.nurse.sum
GrpW.YOR.nurse.days <- length(GrpW.nurse$YORTotal)
GrpW.YOR.nurse.days


GrpW.nonnurse <- subset(Patches.raw, GroupCode==5 & SexCode==1 & InfDepCode==0)
GrpW.nonnurse
Rate <- GrpW.nonnurse$YORTotal/GrpW.nonnurse$TimeTravelday
Rate
GrpW.nonnurse.mean.rate <- mean(Rate)
GrpW.nonnurse.mean.rate
GrpW.YOR.nonnurse.sum <- sum(GrpW.nonnurse$YORTotal)
GrpW.YOR.nonnurse.sum
GrpW.YOR.nonnurse.days <- length(GrpW.nonnurse$YORTotal)
GrpW.YOR.nonnurse.days

GrpW.nurse.mean.rate
GrpW.nonnurse.mean.rate
GrpW.nurse.mean.rate-GrpW.nonnurse.mean.rate


#InfDep-NonInfDep YOR rate summary
Grp1.nurse.mean.rate-Grp1.nonnurse.mean.rate
Grp2.nurse.mean.rate-Grp2.nonnurse.mean.rate
Grp3.nurse.mean.rate-Grp3.nonnurse.mean.rate
GrpS.nurse.mean.rate-GrpS.nonnurse.mean.rate
GrpW.nurse.mean.rate-GrpW.nonnurse.mean.rate

sum(subset(Patches.raw, SexCode==1)$YORTotal)
length(subset(Patches.raw, SexCode==1)$YORTotal)
Grp1.YOR.nonnurse.sum
Grp2.YOR.nonnurse.sum
Grp3.YOR.nonnurse.sum
GrpS.YOR.nonnurse.sum
GrpW.YOR.nonnurse.sum
Grp1.YOR.nurse.sum
Grp2.YOR.nurse.sum
Grp3.YOR.nurse.sum
GrpS.YOR.nurse.sum
GrpW.YOR.nurse.sum


#################################################Female Total mean rates


#Group1

Grp1.nurse <- subset(Patches.raw, GroupCode==1 & SexCode==1 & InfDepCode==1)
Grp1.nurse
Rate <- Grp1.nurse$PatchTotal/Grp1.nurse$TimeTravelday
Rate
Grp1.nurse.mean.rate <- mean(Rate)
Grp1.nurse.mean.rate
Grp1.nurse.sum <- sum(Grp1.nurse$PatchTotal)
Grp1.nurse.sum
Grp1.nurse.days <- length(Grp1.nurse$PatchTotal)
Grp1.nurse.days


Grp1.nonnurse <- subset(Patches.raw, GroupCode==1 & SexCode==1 & InfDepCode==0)
Grp1.nonnurse
Rate <- Grp1.nonnurse$PatchTotal/Grp1.nonnurse$TimeTravelday
Rate
Grp1.nonnurse.mean.rate <- mean(Rate)
Grp1.nonnurse.mean.rate
Grp1.nonnurse.sum <- sum(Grp1.nonnurse$PatchTotal)
Grp1.nonnurse.sum
Grp1.nonnurse.days <- length(Grp1.nonnurse$PatchTotal)
Grp1.nonnurse.days


Grp1.nurse.mean.rate
Grp1.nonnurse.mean.rate
Grp1.nurse.mean.rate-Grp1.nonnurse.mean.rate


#Group2

Grp2.nurse <- subset(Patches.raw, GroupCode==2 & SexCode==1 & InfDepCode==1)
Grp2.nurse
Rate <- Grp2.nurse$PatchTotal/Grp2.nurse$TimeTravelday
Rate
Grp2.nurse.mean.rate <- mean(Rate)
Grp2.nurse.mean.rate
Grp2.nurse.sum <- sum(Grp2.nurse$PatchTotal)
Grp2.nurse.sum
Grp2.nurse.days <- length(Grp2.nurse$PatchTotal)
Grp2.nurse.days


Grp2.nonnurse <- subset(Patches.raw, GroupCode==2 & SexCode==1 & InfDepCode==0)
Grp2.nonnurse
Rate <- Grp2.nonnurse$PatchTotal/Grp2.nonnurse$TimeTravelday
Rate
Grp2.nonnurse.mean.rate <- mean(Rate)
Grp2.nonnurse.mean.rate
Grp2.nonnurse.sum <- sum(Grp2.nonnurse$PatchTotal)
Grp2.nonnurse.sum
Grp2.nonnurse.days <- length(Grp2.nonnurse$PatchTotal)
Grp2.nonnurse.days


Grp2.nurse.mean.rate
Grp2.nonnurse.mean.rate
Grp2.nurse.mean.rate-Grp2.nonnurse.mean.rate


#Group3

Grp3.nurse <- subset(Patches.raw, GroupCode==3 & SexCode==1 & InfDepCode==1)
Grp3.nurse
Rate <- Grp3.nurse$PatchTotal/Grp3.nurse$TimeTravelday
Rate
Grp3.nurse.mean.rate <- mean(Rate)
Grp3.nurse.mean.rate
Grp3.nurse.sum <- sum(Grp3.nurse$PatchTotal)
Grp3.nurse.sum
Grp3.nurse.days <- length(Grp3.nurse$PatchTotal)
Grp3.nurse.days

Grp3.nonnurse <- subset(Patches.raw, GroupCode==3 & SexCode==1 & InfDepCode==0)
Grp3.nonnurse
Rate <- Grp3.nonnurse$PatchTotal/Grp3.nonnurse$TimeTravelday
Rate
Grp3.nonnurse.mean.rate <- mean(Rate)
Grp3.nonnurse.mean.rate
Grp3.nonnurse.sum <- sum(Grp3.nonnurse$PatchTotal)
Grp3.nonnurse.sum
Grp3.nonnurse.days <- length(Grp3.nonnurse$PatchTotal)
Grp3.nonnurse.days

Grp3.nurse.mean.rate
Grp3.nonnurse.mean.rate
Grp3.nurse.mean.rate-Grp3.nonnurse.mean.rate


#GroupS

GrpS.nurse <- subset(Patches.raw, GroupCode==4 & SexCode==1 & InfDepCode==1)
GrpS.nurse
Rate <- GrpS.nurse$PatchTotal/GrpS.nurse$TimeTravelday
Rate
GrpS.nurse.mean.rate <- mean(Rate)
GrpS.nurse.mean.rate
GrpS.nurse.sum <- sum(GrpS.nurse$PatchTotal)
GrpS.nurse.sum
GrpS.nurse.days <- length(GrpS.nurse$PatchTotal)
GrpS.nurse.days

GrpS.nonnurse <- subset(Patches.raw, GroupCode==4 & SexCode==1 & InfDepCode==0)
GrpS.nonnurse
Rate <- GrpS.nonnurse$PatchTotal/GrpS.nonnurse$TimeTravelday
Rate
GrpS.nonnurse.mean.rate <- mean(Rate)
GrpS.nonnurse.mean.rate
GrpS.nonnurse.sum <- sum(GrpS.nonnurse$PatchTotal)
GrpS.nonnurse.sum
GrpS.nonnurse.days <- length(GrpS.nonnurse$PatchTotal)
GrpS.nonnurse.days

GrpS.nurse.mean.rate
GrpS.nonnurse.mean.rate
GrpS.nurse.mean.rate-GrpS.nonnurse.mean.rate



#GroupW

GrpW.nurse <- subset(Patches.raw, GroupCode==5 & SexCode==1 & InfDepCode==1)
GrpW.nurse
Rate <- GrpW.nurse$PatchTotal/GrpW.nurse$TimeTravelday
Rate
GrpW.nurse.mean.rate <- mean(Rate)
GrpW.nurse.mean.rate
GrpW.nurse.sum <- sum(GrpW.nurse$PatchTotal)
GrpW.nurse.sum
GrpW.nurse.days <- length(GrpW.nurse$PatchTotal)
GrpW.nurse.days


GrpW.nonnurse <- subset(Patches.raw, GroupCode==5 & SexCode==1 & InfDepCode==0)
GrpW.nonnurse
Rate <- GrpW.nonnurse$PatchTotal/GrpW.nonnurse$TimeTravelday
Rate
GrpW.nonnurse.mean.rate <- mean(Rate)
GrpW.nonnurse.mean.rate
GrpW.nonnurse.sum <- sum(GrpW.nonnurse$PatchTotal)
GrpW.nonnurse.sum
GrpW.nonnurse.days <- length(GrpW.nonnurse$PatchTotal)
GrpW.nonnurse.days

GrpW.nurse.mean.rate
GrpW.nonnurse.mean.rate
GrpW.nurse.mean.rate-GrpW.nonnurse.mean.rate


#InfDep-NonInfDep PatchTotal rate summary
Grp1.nurse.mean.rate-Grp1.nonnurse.mean.rate
Grp2.nurse.mean.rate-Grp2.nonnurse.mean.rate
Grp3.nurse.mean.rate-Grp3.nonnurse.mean.rate
GrpS.nurse.mean.rate-GrpS.nonnurse.mean.rate
GrpW.nurse.mean.rate-GrpW.nonnurse.mean.rate

sum(subset(Patches.raw, SexCode==1)$PatchTotal)
length(subset(Patches.raw, SexCode==1)$PatchTotal)
Grp1.nonnurse.sum
Grp2.nonnurse.sum
Grp3.nonnurse.sum
GrpS.nonnurse.sum
GrpW.nonnurse.sum
Grp1.nurse.sum
Grp2.nurse.sum
Grp3.nurse.sum
GrpS.nurse.sum
GrpW.nurse.sum


#########################################Male YOR mean rates

#Group1

Grp1.nurse <- subset(Patches.raw, GroupCode==1 & SexCode==0 & InfDepCode==1)
Grp1.nurse
Rate <- Grp1.nurse$YORTotal/Grp1.nurse$TimeTravelday
Rate
Grp1.nurse.mean.rate <- mean(Rate)
Grp1.nurse.mean.rate

Grp1.nonnurse <- subset(Patches.raw, GroupCode==1 & SexCode==0 & InfDepCode==0)
Grp1.nonnurse
Rate <- Grp1.nonnurse$YORTotal/Grp1.nonnurse$TimeTravelday
Rate
Grp1.nonnurse.mean.rate <- mean(Rate)
Grp1.nonnurse.mean.rate

Grp1.nurse.mean.rate
Grp1.nonnurse.mean.rate
Grp1.nurse.mean.rate-Grp1.nonnurse.mean.rate


#Group2

Grp2.nurse <- subset(Patches.raw, GroupCode==2 & SexCode==0 & InfDepCode==1)
Grp2.nurse
Rate <- Grp2.nurse$YORTotal/Grp2.nurse$TimeTravelday
Rate
Grp2.nurse.mean.rate <- mean(Rate)
Grp2.nurse.mean.rate

Grp2.nonnurse <- subset(Patches.raw, GroupCode==2 & SexCode==0 & InfDepCode==0)
Grp2.nonnurse
Rate <- Grp2.nonnurse$YORTotal/Grp2.nonnurse$TimeTravelday
Rate
Grp2.nonnurse.mean.rate <- mean(Rate)
Grp2.nonnurse.mean.rate

Grp2.nurse.mean.rate
Grp2.nonnurse.mean.rate
Grp2.nurse.mean.rate-Grp2.nonnurse.mean.rate


#Group3

Grp3.nurse <- subset(Patches.raw, GroupCode==3 & SexCode==0 & InfDepCode==1)
Grp3.nurse
Rate <- Grp3.nurse$YORTotal/Grp3.nurse$TimeTravelday
Rate
Grp3.nurse.mean.rate <- mean(Rate)
Grp3.nurse.mean.rate

Grp3.nonnurse <- subset(Patches.raw, GroupCode==3 & SexCode==0 & InfDepCode==0)
Grp3.nonnurse
Rate <- Grp3.nonnurse$YORTotal/Grp3.nonnurse$TimeTravelday
Rate
Grp3.nonnurse.mean.rate <- mean(Rate)
Grp3.nonnurse.mean.rate

Grp3.nurse.mean.rate
Grp3.nonnurse.mean.rate
Grp3.nurse.mean.rate-Grp3.nonnurse.mean.rate


#GroupS

GrpS.nurse <- subset(Patches.raw, GroupCode==4 & SexCode==0 & InfDepCode==1)
GrpS.nurse
Rate <- GrpS.nurse$YORTotal/GrpS.nurse$TimeTravelday
Rate
GrpS.nurse.mean.rate <- mean(Rate)
GrpS.nurse.mean.rate

GrpS.nonnurse <- subset(Patches.raw, GroupCode==4 & SexCode==0 & InfDepCode==0)
GrpS.nonnurse
Rate <- GrpS.nonnurse$YORTotal/GrpS.nonnurse$TimeTravelday
Rate
GrpS.nonnurse.mean.rate <- mean(Rate)
GrpS.nonnurse.mean.rate

GrpS.nurse.mean.rate
GrpS.nonnurse.mean.rate
GrpS.nurse.mean.rate-GrpS.nonnurse.mean.rate



#GroupW

GrpW.nurse <- subset(Patches.raw, GroupCode==5 & SexCode==0 & InfDepCode==1)
GrpW.nurse
Rate <- GrpW.nurse$YORTotal/GrpW.nurse$TimeTravelday
Rate
GrpW.nurse.mean.rate <- mean(Rate)
GrpW.nurse.mean.rate

GrpW.nonnurse <- subset(Patches.raw, GroupCode==5 & SexCode==0 & InfDepCode==0)
GrpW.nonnurse
Rate <- GrpW.nonnurse$YORTotal/GrpW.nonnurse$TimeTravelday
Rate
GrpW.nonnurse.mean.rate <- mean(Rate)
GrpW.nonnurse.mean.rate

GrpW.nurse.mean.rate
GrpW.nonnurse.mean.rate
GrpW.nurse.mean.rate-GrpW.nonnurse.mean.rate


#InfDep-NonInfDep YOR rate summary
Grp1.nurse.mean.rate-Grp1.nonnurse.mean.rate
Grp2.nurse.mean.rate-Grp2.nonnurse.mean.rate
Grp3.nurse.mean.rate-Grp3.nonnurse.mean.rate
GrpS.nurse.mean.rate-GrpS.nonnurse.mean.rate
GrpW.nurse.mean.rate-GrpW.nonnurse.mean.rate


#################################################Male Total mean rates


#Group1

Grp1.nurse <- subset(Patches.raw, GroupCode==1 & SexCode==0 & InfDepCode==1)
Grp1.nurse
Rate <- Grp1.nurse$PatchTotal/Grp1.nurse$TimeTravelday
Rate
Grp1.nurse.mean.rate <- mean(Rate)
Grp1.nurse.mean.rate

Grp1.nonnurse <- subset(Patches.raw, GroupCode==1 & SexCode==0 & InfDepCode==0)
Grp1.nonnurse
Rate <- Grp1.nonnurse$PatchTotal/Grp1.nonnurse$TimeTravelday
Rate
Grp1.nonnurse.mean.rate <- mean(Rate)
Grp1.nonnurse.mean.rate

Grp1.nurse.mean.rate
Grp1.nonnurse.mean.rate
Grp1.nurse.mean.rate-Grp1.nonnurse.mean.rate


#Group2

Grp2.nurse <- subset(Patches.raw, GroupCode==2 & SexCode==0 & InfDepCode==1)
Grp2.nurse
Rate <- Grp2.nurse$PatchTotal/Grp2.nurse$TimeTravelday
Rate
Grp2.nurse.mean.rate <- mean(Rate)
Grp2.nurse.mean.rate

Grp2.nonnurse <- subset(Patches.raw, GroupCode==2 & SexCode==0 & InfDepCode==0)
Grp2.nonnurse
Rate <- Grp2.nonnurse$PatchTotal/Grp2.nonnurse$TimeTravelday
Rate
Grp2.nonnurse.mean.rate <- mean(Rate)
Grp2.nonnurse.mean.rate

Grp2.nurse.mean.rate
Grp2.nonnurse.mean.rate
Grp2.nurse.mean.rate-Grp2.nonnurse.mean.rate


#Group3

Grp3.nurse <- subset(Patches.raw, GroupCode==3 & SexCode==0 & InfDepCode==1)
Grp3.nurse
Rate <- Grp3.nurse$PatchTotal/Grp3.nurse$TimeTravelday
Rate
Grp3.nurse.mean.rate <- mean(Rate)
Grp3.nurse.mean.rate

Grp3.nonnurse <- subset(Patches.raw, GroupCode==3 & SexCode==0 & InfDepCode==0)
Grp3.nonnurse
Rate <- Grp3.nonnurse$PatchTotal/Grp3.nonnurse$TimeTravelday
Rate
Grp3.nonnurse.mean.rate <- mean(Rate)
Grp3.nonnurse.mean.rate

Grp3.nurse.mean.rate
Grp3.nonnurse.mean.rate
Grp3.nurse.mean.rate-Grp3.nonnurse.mean.rate


#GroupS

GrpS.nurse <- subset(Patches.raw, GroupCode==4 & SexCode==0 & InfDepCode==1)
GrpS.nurse
Rate <- GrpS.nurse$PatchTotal/GrpS.nurse$TimeTravelday
Rate
GrpS.nurse.mean.rate <- mean(Rate)
GrpS.nurse.mean.rate

GrpS.nonnurse <- subset(Patches.raw, GroupCode==4 & SexCode==0 & InfDepCode==0)
GrpS.nonnurse
Rate <- GrpS.nonnurse$PatchTotal/GrpS.nonnurse$TimeTravelday
Rate
GrpS.nonnurse.mean.rate <- mean(Rate)
GrpS.nonnurse.mean.rate

GrpS.nurse.mean.rate
GrpS.nonnurse.mean.rate
GrpS.nurse.mean.rate-GrpS.nonnurse.mean.rate



#GroupW

GrpW.nurse <- subset(Patches.raw, GroupCode==5 & SexCode==0 & InfDepCode==1)
GrpW.nurse
Rate <- GrpW.nurse$PatchTotal/GrpW.nurse$TimeTravelday
Rate
GrpW.nurse.mean.rate <- mean(Rate)
GrpW.nurse.mean.rate

GrpW.nonnurse <- subset(Patches.raw, GroupCode==5 & SexCode==0 & InfDepCode==0)
GrpW.nonnurse
Rate <- GrpW.nonnurse$PatchTotal/GrpW.nonnurse$TimeTravelday
Rate
GrpW.nonnurse.mean.rate <- mean(Rate)
GrpW.nonnurse.mean.rate

GrpW.nurse.mean.rate
GrpW.nonnurse.mean.rate
GrpW.nurse.mean.rate-GrpW.nonnurse.mean.rate


#InfDep-NonInfDep PatchTotal rate summary
Grp1.nurse.mean.rate-Grp1.nonnurse.mean.rate
Grp2.nurse.mean.rate-Grp2.nonnurse.mean.rate
Grp3.nurse.mean.rate-Grp3.nonnurse.mean.rate
GrpS.nurse.mean.rate-GrpS.nonnurse.mean.rate
GrpW.nurse.mean.rate-GrpW.nonnurse.mean.rate

