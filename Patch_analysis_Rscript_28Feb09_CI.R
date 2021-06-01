#This calculates 95% CIs for population average estimates from
#Poisson regressions.

#Contents:
#Step1: bring in the previously determined best-fit models
#Step2: example of CI calulation steps with notes
#Step3: CIs for Model.YOR.reduced3.off
#Step4: CIs for Model.YOR.reduced5.off


# #Read the data from the csv data file into R:
# Patches.raw <- read.csv(file="./Data/PatchData_10feb09.csv", header=TRUE)

# #Check the variable names and dimensions in the data frame Patches.raw
# names(Patches.raw)
# dim(Patches.raw)

# #Attach the data frame
# attach(Patches.raw)

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


#Step1####################################################################
##########Import fitted models############################################

###Model.YOR.reduced3 series

#with estimated exposure time
Model.YOR.reduced3 <- glmer(YORTotal ~ c.log.TimeTraveled +
			InvSexCode + VisionCode+ InvInfDepCode + SeasonCode +
			VisionCode:InvInfDepCode +
			VisionCode:SeasonCode + 
			(1 | GroupName), family=poisson)

summary(Model.YOR.reduced3)

#with fixed offset
Model.YOR.reduced3.off <- glmer(YORTotal ~
			InvSexCode + VisionCode + InvInfDepCode + SeasonCode +
			VisionCode:InvInfDepCode +
			VisionCode:SeasonCode + 
			(1 | GroupName), family=poisson, offset=log.TimeTraveled)

summary(Model.YOR.reduced3.off)



###Model.YOR.reduced5 series

#with estimated exposure time
Model.YOR.reduced5 <- glmer(YORTotal ~ c.log.TimeTraveled +
			InvSexCode + VisionCode+ InvInfDepCode +
			VisionCode:InvInfDepCode + 
			(1 | GroupName), family=poisson)

summary(Model.YOR.reduced5)


#with fixed offset
Model.YOR.reduced5.off <- glmer(YORTotal ~
			InvSexCode + VisionCode+ InvInfDepCode +
			VisionCode:InvInfDepCode + 
			(1 | GroupName), family=poisson, offset=log.TimeTraveled)

summary(Model.YOR.reduced5.off)



#Step 2###################################################################
#####################Example CIs##########################################


####Example CI calculation for Model.YOR.reduced3.off

#condition
binary.vector <- c(1, 0, 1, 1, 1, 1, 1) #fem, indep, tri, wet


#model population average estimate for the condition
eta <-  t(binary.vector) %*% fixef(Model.YOR.reduced3.off)

#t(binary.vector) is a row vector
#fixef(Model.YOR.reduced3.off) is a column vector
#matrix multiplication sums element by element product of 1st row by 1st column


#variance in the estimate
var.eta <- t(binary.vector) %*% vcov(Model.YOR.reduced3.off) %*% binary.vector

#Variance-covariance matrix contains the variance of the coef estimates from
#the model (which is the squared SE) down the diagonal and the rest of the
#cells filled with the covariances of paired coefs.
#The first multiplication step yields a row vector, with each element
#corresponding to a coef from the model and representing the sum of the coef's
#variance and that coef's covariances with all other coefs.
#The second matrix multiplication step sums the variance+covariances for the
#condition represented by binary.vector, which is a column vector 


se.eta <- sqrt(var.eta) #standard error = variance^0.5

# 95% CI
eta.upper <- eta + 1.96 * se.eta
eta.lower <- eta - 1.96 * se.eta

# On the same scale as the data
exp(eta)
exp(eta.upper)
exp(eta.lower)


#Step 3####################################################################
######################CIs for Model.YOR.reduced3.off#######################

#conditions:
#				    Fem=0 di=0  Dep=0 dry=0
#			     Int   Sex   Vis   Inf   Sea  Vis:Inf  Vis:Sea    
fem.dep.di.dry <- c(	1,    0,    0,    0,    0,    0,       0)
fem.ind.di.dry <- c(	1,    0,    0,    1,    0,    0,       0)
fem.dep.tri.dry <- c(	1,    0,    1,    0,    0,    0,       0)
fem.ind.tri.dry <- c(	1,    0,    1,    1,    0,    1,       0)
fem.dep.di.wet <- c(	1,    0,    0,    0,    1,    0,       0)
fem.ind.di.wet <- c(	1,    0,    0,    1,    1,    0,       0)
fem.dep.tri.wet <- c(	1,    0,    1,    0,    1,    0,       1)
fem.ind.tri.wet <- c(	1,    0,    1,    1,    1,    1,       1)
mal.dep.di.dry <- c(	1,    1,    0,    0,    0,    0,       0)
mal.ind.di.dry <- c(	1,    1,    0,    1,    0,    0,       0)
mal.dep.di.wet <- c(	1,    1,    0,    0,    1,    0,       0)
mal.ind.di.wet <- c(	1,    1,    0,    1,    1,    0,       0)

#model estimates:
eta.fem.dep.di.dry <-  t(fem.dep.di.dry) %*% fixef(Model.YOR.reduced3.off)
eta.fem.ind.di.dry <-  t(fem.ind.di.dry) %*% fixef(Model.YOR.reduced3.off)
eta.fem.dep.tri.dry <-  t(fem.dep.tri.dry) %*% fixef(Model.YOR.reduced3.off)
eta.fem.ind.tri.dry <-  t(fem.ind.tri.dry) %*% fixef(Model.YOR.reduced3.off)
eta.fem.dep.di.wet <-  t(fem.dep.di.wet) %*% fixef(Model.YOR.reduced3.off)
eta.fem.ind.di.wet <-  t(fem.ind.di.wet) %*% fixef(Model.YOR.reduced3.off)
eta.fem.dep.tri.wet <-  t(fem.dep.tri.wet) %*% fixef(Model.YOR.reduced3.off)
eta.fem.ind.tri.wet <-  t(fem.ind.tri.wet) %*% fixef(Model.YOR.reduced3.off)
eta.mal.dep.di.dry <-  t(mal.dep.di.dry) %*% fixef(Model.YOR.reduced3.off)
eta.mal.ind.di.dry <-  t(mal.ind.di.dry) %*% fixef(Model.YOR.reduced3.off)
eta.mal.dep.di.wet <-  t(mal.dep.di.wet) %*% fixef(Model.YOR.reduced3.off)
eta.mal.ind.di.wet <-  t(mal.ind.di.wet) %*% fixef(Model.YOR.reduced3.off)

#variance of estimates
var.fem.dep.di.dry <- t(fem.dep.di.dry) %*% vcov(Model.YOR.reduced3.off) %*%
					fem.dep.di.dry
var.fem.ind.di.dry <- t(fem.ind.di.dry) %*% vcov(Model.YOR.reduced3.off) %*%
					fem.ind.di.dry
var.fem.dep.tri.dry <- t(fem.dep.tri.dry) %*% vcov(Model.YOR.reduced3.off) %*%
					fem.dep.tri.dry
var.fem.ind.tri.dry <- t(fem.ind.tri.dry) %*% vcov(Model.YOR.reduced3.off) %*%
					fem.ind.tri.dry
var.fem.dep.di.wet <- t(fem.dep.di.wet) %*% vcov(Model.YOR.reduced3.off) %*%
					fem.dep.di.wet
var.fem.ind.di.wet <- t(fem.ind.di.wet) %*% vcov(Model.YOR.reduced3.off) %*%
					fem.ind.di.wet
var.fem.dep.tri.wet <- t(fem.dep.tri.wet) %*% vcov(Model.YOR.reduced3.off) %*%
					fem.dep.tri.wet
var.fem.ind.tri.wet <- t(fem.ind.tri.wet) %*% vcov(Model.YOR.reduced3.off) %*%
					fem.ind.tri.wet
var.mal.dep.di.dry <- t(mal.dep.di.dry) %*% vcov(Model.YOR.reduced3.off) %*%
					mal.dep.di.dry
var.mal.ind.di.dry <- t(mal.ind.di.dry) %*% vcov(Model.YOR.reduced3.off) %*%
					mal.ind.di.dry
var.mal.dep.di.wet <- t(mal.dep.di.wet) %*% vcov(Model.YOR.reduced3.off) %*%
					mal.dep.di.wet
var.mal.ind.di.wet <- t(mal.ind.di.wet) %*% vcov(Model.YOR.reduced3.off) %*%
					mal.ind.di.wet

#CIs
upper.fem.dep.di.dry <- eta.fem.dep.di.dry + 1.96 * sqrt(var.fem.dep.di.dry)
lower.fem.dep.di.dry <- eta.fem.dep.di.dry - 1.96 * sqrt(var.fem.dep.di.dry)
upper.fem.ind.di.dry <- eta.fem.ind.di.dry + 1.96 * sqrt(var.fem.ind.di.dry)
lower.fem.ind.di.dry <- eta.fem.ind.di.dry - 1.96 * sqrt(var.fem.ind.di.dry)
upper.fem.dep.tri.dry <- eta.fem.dep.tri.dry + 1.96 * sqrt(var.fem.dep.tri.dry)
lower.fem.dep.tri.dry <- eta.fem.dep.tri.dry - 1.96 * sqrt(var.fem.dep.tri.dry)
upper.fem.ind.tri.dry <- eta.fem.ind.tri.dry + 1.96 * sqrt(var.fem.ind.tri.dry)
lower.fem.ind.tri.dry <- eta.fem.ind.tri.dry - 1.96 * sqrt(var.fem.ind.tri.dry)
upper.fem.dep.di.wet <- eta.fem.dep.di.wet + 1.96 * sqrt(var.fem.dep.di.wet)
lower.fem.dep.di.wet <- eta.fem.dep.di.wet - 1.96 * sqrt(var.fem.dep.di.wet)
upper.fem.ind.di.wet <- eta.fem.ind.di.wet + 1.96 * sqrt(var.fem.ind.di.wet)
lower.fem.ind.di.wet <- eta.fem.ind.di.wet - 1.96 * sqrt(var.fem.ind.di.wet)
upper.fem.dep.tri.wet <- eta.fem.dep.tri.wet + 1.96 * sqrt(var.fem.dep.tri.wet)
lower.fem.dep.tri.wet <- eta.fem.dep.tri.wet - 1.96 * sqrt(var.fem.dep.tri.wet)
upper.fem.ind.tri.wet <- eta.fem.ind.tri.wet + 1.96 * sqrt(var.fem.ind.tri.wet)
lower.fem.ind.tri.wet <- eta.fem.ind.tri.wet - 1.96 * sqrt(var.fem.ind.tri.wet)
upper.mal.dep.di.dry <- eta.mal.dep.di.dry + 1.96 * sqrt(var.mal.dep.di.dry)
lower.mal.dep.di.dry <- eta.mal.dep.di.dry - 1.96 * sqrt(var.mal.dep.di.dry)
upper.mal.ind.di.dry <- eta.mal.ind.di.dry + 1.96 * sqrt(var.mal.ind.di.dry)
lower.mal.ind.di.dry <- eta.mal.ind.di.dry - 1.96 * sqrt(var.mal.ind.di.dry)
upper.mal.dep.di.wet <- eta.mal.dep.di.wet + 1.96 * sqrt(var.mal.dep.di.wet)
lower.mal.dep.di.wet <- eta.mal.dep.di.wet - 1.96 * sqrt(var.mal.dep.di.wet)
upper.mal.ind.di.wet <- eta.mal.ind.di.wet + 1.96 * sqrt(var.mal.ind.di.wet)
lower.mal.ind.di.wet <- eta.mal.ind.di.wet - 1.96 * sqrt(var.mal.ind.di.wet)


#matrix of estimates and CIs [drop() turns a 1x1 matrix to a scalar]

est.mat <- matrix(c(exp(eta.fem.dep.di.dry), drop(exp(upper.fem.dep.di.dry)),
				drop(exp(lower.fem.dep.di.dry)),
exp(eta.fem.ind.di.dry), drop(exp(upper.fem.ind.di.dry)), drop(exp(lower.fem.ind.di.dry)),
exp(eta.fem.dep.tri.dry), drop(exp(upper.fem.dep.tri.dry)), drop(exp(lower.fem.dep.tri.dry)),
exp(eta.fem.ind.tri.dry), drop(exp(upper.fem.ind.tri.dry)), drop(exp(lower.fem.ind.tri.dry)),
exp(eta.fem.dep.di.wet), drop(exp(upper.fem.dep.di.wet)), drop(exp(lower.fem.dep.di.wet)),
exp(eta.fem.ind.di.wet), drop(exp(upper.fem.ind.di.wet)), drop(exp(lower.fem.ind.di.wet)),
exp(eta.fem.dep.tri.wet), drop(exp(upper.fem.dep.tri.wet)), drop(exp(lower.fem.dep.tri.wet)),
exp(eta.fem.ind.tri.wet), drop(exp(upper.fem.ind.tri.wet)), drop(exp(lower.fem.ind.tri.wet)),
exp(eta.mal.dep.di.dry), drop(exp(upper.mal.dep.di.dry)), drop(exp(lower.mal.dep.di.dry)),
exp(eta.mal.ind.di.dry), drop(exp(upper.mal.ind.di.dry)), drop(exp(lower.mal.ind.di.dry)),
exp(eta.mal.dep.di.wet), drop(exp(upper.mal.dep.di.wet)), drop(exp(lower.mal.dep.di.wet)),
exp(eta.mal.ind.di.wet), drop(exp(upper.mal.ind.di.wet)), drop(exp(lower.mal.ind.di.wet))),
				nrow=3,
				dimnames = list(value = c("estimate", "upper", "lower"),
					condition = c("fem.dep.di.dry",
							"fem.ind.di.dry",
							"fem.dep.tri.dry",
							"fem.ind.tri.dry",
							"fem.dep.di.wet",
							"fem.ind.di.wet",
							"fem.dep.tri.wet",
							"fem.ind.tri.wet",
							"mal.dep.di.dry",
							"mal.ind.di.dry",
							"mal.dep.di.wet",
							"mal.ind.di.wet")))

#patch encounters per hour
CI.matrix <- t(est.mat/12)

#add labels as factors to the matrix
position.labs <- c(1.15,4.15,	1.85,4.85,	1.15,4.15,	1.85,4.85,	2.65,5.65,	2.65,5.65)
season.labs <- as.factor(c("Dry","Dry","Dry","Dry","Wet","Wet","Wet","Wet","Dry","Dry","Wet","Wet"))
sex.labs <- as.factor(c("Female","Female","Female","Female","Female","Female","Female","Female",
	"Male","Male","Male","Male"))
infdep.labs <- as.factor(c("Dependent","Independent","Dependent","Independent",
			"Dependent","Independent","Dependent","Independent",
			"Dependent","Independent","Dependent","Independent")) 
CI.matrix.labs <- cbind(position.labs, sex.labs, infdep.labs, season.labs, CI.matrix)


#plot for wet season
barplot(cbind(CI.matrix.labs[c(5,7,11),5], CI.matrix.labs[c(6,8,12),5]), beside=TRUE,
	ylim=c(0, 2.5), xlim=c(0.75, 8), width=c(0.75), col=c(gray(0.8), gray(1), gray(0.6)),
	names.arg=c("Di F", "Tri F", "M", "Di F", "Tri F", "M"),
	ylab="Patch Encounters Per Hour Travel"
	)

errbar(CI.matrix.labs[c(5,7,11,6,8,12),1], CI.matrix.labs[c(5,7,11,6,8,12),5],
	CI.matrix.labs[c(5,7,11,6,8,12),6], CI.matrix.labs[c(5,7,11,6,8,12),7],
	add=TRUE)

mtext("Dependent", at=1.8, side=1, line=2.5)
mtext("Independent", at=4.8, side=1, line=2.5)


#plot for dry season
barplot(cbind(CI.matrix.labs[c(1,3,9),5], CI.matrix.labs[c(2,4,10),5]), beside=TRUE,
	ylim=c(0, 2.5), xlim=c(0.75, 8), width=c(0.75), col=c(gray(0.8), gray(1), gray(0.6)),
	names.arg=c("Di F", "Tri F", "M", "Di F", "Tri F", "M"),
	ylab="Patch Encounters Per Hour Travel"
	)

errbar(CI.matrix.labs[c(1,3,9,2,4,10),1], CI.matrix.labs[c(1,3,9,2,4,10),5],
	CI.matrix.labs[c(1,3,9,2,4,10),6], CI.matrix.labs[c(1,3,9,2,4,10),7],
	add=TRUE)

mtext("Dependent", at=1.8, side=1, line=2.5)
mtext("Independent", at=4.8, side=1, line=2.5)


#Step 4#########################################################################
####################CIs for Model.YOR.reduced5.off##############################

#conditions:
# ".dry" has no meaning	    Fem=0  di=0 Dep=0
# 			     Int   Sex   Vis   Inf   Vis:Inf      
fem.dep.di.dry <- c(	1,    0,    0,    0,     0)      
fem.ind.di.dry <- c(	1,    0,    0,    1,     0)      
fem.dep.tri.dry <- c(	1,    0,    1,    0,     0)     
fem.ind.tri.dry <- c(	1,    0,    1,    1,     1)      
mal.dep.di.dry <- c(	1,    1,    0,    0,     0)      
mal.ind.di.dry <- c(	1,    1,    0,    1,     0)      

#model estimates:
eta.fem.dep.di.dry <-  t(fem.dep.di.dry) %*% fixef(Model.YOR.reduced5.off)
eta.fem.ind.di.dry <-  t(fem.ind.di.dry) %*% fixef(Model.YOR.reduced5.off)
eta.fem.dep.tri.dry <-  t(fem.dep.tri.dry) %*% fixef(Model.YOR.reduced5.off)
eta.fem.ind.tri.dry <-  t(fem.ind.tri.dry) %*% fixef(Model.YOR.reduced5.off)
eta.mal.dep.di.dry <-  t(mal.dep.di.dry) %*% fixef(Model.YOR.reduced5.off)
eta.mal.ind.di.dry <-  t(mal.ind.di.dry) %*% fixef(Model.YOR.reduced5.off)

#variance of estimates
var.fem.dep.di.dry <- t(fem.dep.di.dry) %*% vcov(Model.YOR.reduced5.off) %*%
					fem.dep.di.dry
var.fem.ind.di.dry <- t(fem.ind.di.dry) %*% vcov(Model.YOR.reduced5.off) %*%
					fem.ind.di.dry
var.fem.dep.tri.dry <- t(fem.dep.tri.dry) %*% vcov(Model.YOR.reduced5.off) %*%
					fem.dep.tri.dry
var.fem.ind.tri.dry <- t(fem.ind.tri.dry) %*% vcov(Model.YOR.reduced5.off) %*%
					fem.ind.tri.dry
var.mal.dep.di.dry <- t(mal.dep.di.dry) %*% vcov(Model.YOR.reduced5.off) %*%
					mal.dep.di.dry
var.mal.ind.di.dry <- t(mal.ind.di.dry) %*% vcov(Model.YOR.reduced5.off) %*%
					mal.ind.di.dry

#CIs
upper.fem.dep.di.dry <- eta.fem.dep.di.dry + 1.96 * sqrt(var.fem.dep.di.dry)
lower.fem.dep.di.dry <- eta.fem.dep.di.dry - 1.96 * sqrt(var.fem.dep.di.dry)
upper.fem.ind.di.dry <- eta.fem.ind.di.dry + 1.96 * sqrt(var.fem.ind.di.dry)
lower.fem.ind.di.dry <- eta.fem.ind.di.dry - 1.96 * sqrt(var.fem.ind.di.dry)
upper.fem.dep.tri.dry <- eta.fem.dep.tri.dry + 1.96 * sqrt(var.fem.dep.tri.dry)
lower.fem.dep.tri.dry <- eta.fem.dep.tri.dry - 1.96 * sqrt(var.fem.dep.tri.dry)
upper.fem.ind.tri.dry <- eta.fem.ind.tri.dry + 1.96 * sqrt(var.fem.ind.tri.dry)
lower.fem.ind.tri.dry <- eta.fem.ind.tri.dry - 1.96 * sqrt(var.fem.ind.tri.dry)
upper.mal.dep.di.dry <- eta.mal.dep.di.dry + 1.96 * sqrt(var.mal.dep.di.dry)
lower.mal.dep.di.dry <- eta.mal.dep.di.dry - 1.96 * sqrt(var.mal.dep.di.dry)
upper.mal.ind.di.dry <- eta.mal.ind.di.dry + 1.96 * sqrt(var.mal.ind.di.dry)
lower.mal.ind.di.dry <- eta.mal.ind.di.dry - 1.96 * sqrt(var.mal.ind.di.dry)


#matrix of estimates and CIs [drop() turns a 1x1 matrix to a scalar]

est.mat <- matrix(c(exp(eta.fem.dep.di.dry), drop(exp(upper.fem.dep.di.dry)),
				drop(exp(lower.fem.dep.di.dry)),
exp(eta.fem.ind.di.dry), drop(exp(upper.fem.ind.di.dry)), drop(exp(lower.fem.ind.di.dry)),
exp(eta.fem.dep.tri.dry), drop(exp(upper.fem.dep.tri.dry)), drop(exp(lower.fem.dep.tri.dry)),
exp(eta.fem.ind.tri.dry), drop(exp(upper.fem.ind.tri.dry)), drop(exp(lower.fem.ind.tri.dry)),
exp(eta.mal.dep.di.dry), drop(exp(upper.mal.dep.di.dry)), drop(exp(lower.mal.dep.di.dry)),
exp(eta.mal.ind.di.dry), drop(exp(upper.mal.ind.di.dry)), drop(exp(lower.mal.ind.di.dry))),
				nrow=3,
				dimnames = list(value = c("estimate", "upper", "lower"),
					condition = c("fem.dep.di.dry",
							"fem.ind.di.dry",
							"fem.dep.tri.dry",
							"fem.ind.tri.dry",
							"mal.dep.di.dry",
							"mal.ind.di.dry")))

#patch encounters per hour
CI.matrix <- t(est.mat/12)

#add labels as factors to the matrix
position.labs <- c(1.85,4.85,	2.65,5.65, 1.15,4.15)
sex.labs <- as.factor(c("Female","Female","Female","Female","Male","Male"))
infdep.labs <- as.factor(c("Dependent","Independent","Dependent","Independent",
			"Dependent","Independent")) 
CI.matrix.labs <- cbind(position.labs, sex.labs, infdep.labs, CI.matrix)

#plot
barplot(cbind(CI.matrix.labs[c(5,1,3),4], CI.matrix.labs[c(6,2,4),4]), beside=TRUE,
	ylim=c(0, 2), xlim=c(0.75, 8), width=c(0.75), col=c(gray(0.8), gray(1), gray(0.6)),
	names.arg=c("M", "Di F", "Tri F", "M", "Di F", "Tri F"),
	ylab="Patch Encounters Per Hour Travel"
	)

errbar(CI.matrix.labs[c(5,1,3,6,2,4),1], CI.matrix.labs[c(5,1,3,6,2,4),4],
	CI.matrix.labs[c(5,1,3,6,2,4),5], CI.matrix.labs[c(5,1,3,6,2,4),6],
	add=TRUE)

mtext("Dependent", at=1.8, side=1, line=2.5)
mtext("Independent", at=4.8, side=1, line=2.5)	


#Step 5#########################################################################
##############CIs for Model.YOR.reduced5 (estimated intercept)#################

#conditions:
# ".dry" has no meaning	        Fem=0  di=0 Dep=0
# 			     Int  Exp  Sex   Vis   Inf   Vis:Inf      
fem.dep.di.dry <- c(	1,   0,   0,    0,    0,     0)      
fem.ind.di.dry <- c(	1,   0,   0,    0,    1,     0)      
fem.dep.tri.dry <- c(	1,   0,   0,    1,    0,     0)     
fem.ind.tri.dry <- c(	1,   0,   0,    1,    1,     1)      
mal.dep.di.dry <- c(	1,   0,   1,    0,    0,     0)      
mal.ind.di.dry <- c(	1,   0,   1,    0,    1,     0)      

#model estimates:
eta.fem.dep.di.dry <-  t(fem.dep.di.dry) %*% fixef(Model.YOR.reduced5)
eta.fem.ind.di.dry <-  t(fem.ind.di.dry) %*% fixef(Model.YOR.reduced5)
eta.fem.dep.tri.dry <-  t(fem.dep.tri.dry) %*% fixef(Model.YOR.reduced5)
eta.fem.ind.tri.dry <-  t(fem.ind.tri.dry) %*% fixef(Model.YOR.reduced5)
eta.mal.dep.di.dry <-  t(mal.dep.di.dry) %*% fixef(Model.YOR.reduced5)
eta.mal.ind.di.dry <-  t(mal.ind.di.dry) %*% fixef(Model.YOR.reduced5)

#variance of estimates
var.fem.dep.di.dry <- t(fem.dep.di.dry) %*% vcov(Model.YOR.reduced5) %*%
					fem.dep.di.dry
var.fem.ind.di.dry <- t(fem.ind.di.dry) %*% vcov(Model.YOR.reduced5) %*%
					fem.ind.di.dry
var.fem.dep.tri.dry <- t(fem.dep.tri.dry) %*% vcov(Model.YOR.reduced5) %*%
					fem.dep.tri.dry
var.fem.ind.tri.dry <- t(fem.ind.tri.dry) %*% vcov(Model.YOR.reduced5) %*%
					fem.ind.tri.dry
var.mal.dep.di.dry <- t(mal.dep.di.dry) %*% vcov(Model.YOR.reduced5) %*%
					mal.dep.di.dry
var.mal.ind.di.dry <- t(mal.ind.di.dry) %*% vcov(Model.YOR.reduced5) %*%
					mal.ind.di.dry

#CIs
upper.fem.dep.di.dry <- eta.fem.dep.di.dry + 1.96 * sqrt(var.fem.dep.di.dry)
lower.fem.dep.di.dry <- eta.fem.dep.di.dry - 1.96 * sqrt(var.fem.dep.di.dry)
upper.fem.ind.di.dry <- eta.fem.ind.di.dry + 1.96 * sqrt(var.fem.ind.di.dry)
lower.fem.ind.di.dry <- eta.fem.ind.di.dry - 1.96 * sqrt(var.fem.ind.di.dry)
upper.fem.dep.tri.dry <- eta.fem.dep.tri.dry + 1.96 * sqrt(var.fem.dep.tri.dry)
lower.fem.dep.tri.dry <- eta.fem.dep.tri.dry - 1.96 * sqrt(var.fem.dep.tri.dry)
upper.fem.ind.tri.dry <- eta.fem.ind.tri.dry + 1.96 * sqrt(var.fem.ind.tri.dry)
lower.fem.ind.tri.dry <- eta.fem.ind.tri.dry - 1.96 * sqrt(var.fem.ind.tri.dry)
upper.mal.dep.di.dry <- eta.mal.dep.di.dry + 1.96 * sqrt(var.mal.dep.di.dry)
lower.mal.dep.di.dry <- eta.mal.dep.di.dry - 1.96 * sqrt(var.mal.dep.di.dry)
upper.mal.ind.di.dry <- eta.mal.ind.di.dry + 1.96 * sqrt(var.mal.ind.di.dry)
lower.mal.ind.di.dry <- eta.mal.ind.di.dry - 1.96 * sqrt(var.mal.ind.di.dry)


#matrix of estimates and CIs [drop() turns a 1x1 matrix to a scalar]

est.mat <- matrix(c(exp(eta.fem.dep.di.dry), drop(exp(upper.fem.dep.di.dry)),
				drop(exp(lower.fem.dep.di.dry)),
exp(eta.fem.ind.di.dry), drop(exp(upper.fem.ind.di.dry)), drop(exp(lower.fem.ind.di.dry)),
exp(eta.fem.dep.tri.dry), drop(exp(upper.fem.dep.tri.dry)), drop(exp(lower.fem.dep.tri.dry)),
exp(eta.fem.ind.tri.dry), drop(exp(upper.fem.ind.tri.dry)), drop(exp(lower.fem.ind.tri.dry)),
exp(eta.mal.dep.di.dry), drop(exp(upper.mal.dep.di.dry)), drop(exp(lower.mal.dep.di.dry)),
exp(eta.mal.ind.di.dry), drop(exp(upper.mal.ind.di.dry)), drop(exp(lower.mal.ind.di.dry))),
				nrow=3,
				dimnames = list(value = c("estimate", "upper", "lower"),
					condition = c("fem.dep.di.dry",
							"fem.ind.di.dry",
							"fem.dep.tri.dry",
							"fem.ind.tri.dry",
							"mal.dep.di.dry",
							"mal.ind.di.dry")))

#patch encounters per mean travel time = 53min
CI.matrix <- t(est.mat)

#add labels as factors to the matrix
position.labs <- c(1.15,4.15,	1.85,4.85,	2.65,5.65)
sex.labs <- as.factor(c("Female","Female","Female","Female","Male","Male"))
infdep.labs <- as.factor(c("Dependent","Independent","Dependent","Independent",
			"Dependent","Independent")) 
CI.matrix.labs <- cbind(position.labs, sex.labs, infdep.labs, CI.matrix)

#plot
barplot(cbind(CI.matrix.labs[c(1,3,5),4], CI.matrix.labs[c(2,4,6),4]), beside=TRUE,
	ylim=c(0, 2), xlim=c(0.75, 8), width=c(0.75), col=c(gray(0.8), gray(1), gray(0.6)),
	names.arg=c("Di F", "Tri F", "M", "Di F", "Tri F", "M"),
	ylab="Patch Encounters Per Hour Travel"
	)

errbar(CI.matrix.labs[c(1,3,5,2,4,6),1], CI.matrix.labs[c(1,3,5,2,4,6),4],
	CI.matrix.labs[c(1,3,5,2,4,6),5], CI.matrix.labs[c(1,3,5,2,4,6),6],
	add=TRUE)

mtext("Dependent", at=1.8, side=1, line=2.5)
mtext("Independent", at=4.8, side=1, line=2.5)			