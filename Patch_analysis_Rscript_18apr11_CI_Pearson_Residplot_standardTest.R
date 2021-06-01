#Contents:
#Step1: bring in the previously determined best-fit model
#Step2: CIs for Model.YOR.reduced5.off
#Step3: Pearson statistic Model.YOR.reduced5.off
#Step4: Residual analysis Model.YOR.reduced5.off
#Step5: Conventional tests of contrasts: Schenker and Gentleman


# #Read the data from the csv data file into R:
# Patches.raw <- read.csv(file="./Data/PatchData_10feb09.csv", header=TRUE)

# #Check the variable names and dimensions in the data frame Patches.raw
# names(Patches.raw)
# dim(Patches.raw)

# #Attach the data frame
# attach(Patches.raw)

#ln of the exposure time = the "offset" in Poisson model (pg111-112)
#	= ln(hours per day observed traveling)

log.TimeTraveled <- log(TimeTravelh)
 
#Step1####################################################################
##########Import fitted model

Model.YOR.reduced5.off <- glmer(YORTotal ~ 									#originally fit with lmer. Now in newer version of lme4 it must be fit with glmer. Results still roughly correspond with Table 1
			InvSexCode + VisionCode + InvInfDepCode +
			VisionCode:InvInfDepCode + 
			(1 | GroupName), family=poisson, offset=log.TimeTraveled)

summary(Model.YOR.reduced5.off)

deviance(Model.YOR.reduced5.off)
logLik(Model.YOR.reduced5.off)


#Step 2#########################################################################
##############CIs for Model.YOR.reduced5.off

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

CI.matrix <- t(est.mat)

#add labels as factors to the matrix
position.labs <- c(1.85,4.85,	2.65,5.65, 1.15,4.15)
sex.labs <- as.factor(c("Female","Female","Female","Female","Male","Male"))
infdep.labs <- as.factor(c("Dependent","Independent","Dependent","Independent",
			"Dependent","Independent")) 
CI.matrix.labs <- cbind(position.labs, sex.labs, infdep.labs, CI.matrix)

#plot
pdf(file="./Plots/Figure2.pdf",
		height=10, width=8)

par(mar=c(10, 8, 8, 2), fin=c(6, 7)) #set page and plot margins


#layout(rbind(c(2,1)), widths=(c(1,20)))

midpts <- barplot(cbind(CI.matrix.labs[c(5,1,3),4], CI.matrix.labs[c(6,2,4),4]), beside=TRUE,
	axes=F,
	ylim=c(0, 1.8), xlim=c(0.75, 6.5),
	width=c(0.8),
	angle=c(45, 0, 135),
	density=c(20, 0, 30),
	col=c(gray(0), gray(1), gray(0))
	)

#midpts is 3x2 matrix containing midpoint x-values of bars
midpts_list <- c(midpts[1:3], midpts[4:6])

axis(side=2, at=seq(0, 1.8, 0.6), lwd=2, las=1, cex.axis=1.1)
mtext("Patch Encounters Per Hour Travel", side=2, line=3, cex=1.2, at=0.9)

errbar(midpts_list, CI.matrix.labs[c(5,1,3,6,2,4),4],
	CI.matrix.labs[c(5,1,3,6,2,4),5], CI.matrix.labs[c(5,1,3,6,2,4),6],
	add=TRUE, pch=3, lwd=2, cap=0.03, col="black"
	)

halfwidth <- diff(midpts[1:2])/2
rect_left_x <- midpts_list - halfwidth
rect_right_x <- midpts_list + halfwidth
rect_tops <- CI.matrix.labs[c(5,1,3,6,2,4),4]

rect(rect_left_x, 0, rect_right_x,rect_tops, lwd=2, border="black")
#rect(0, 0.001, 6.7, 1.8, lwd=2, border="black") #plot border
box(which = "plot", lty = "solid", lwd=2) 

mtext(c("M", "Di-F", "Tri-F", "M", "Di-F", "Tri-F"),at=midpts_list,
		line=0.5, cex=1.1, side=1) 
mtext("Dependent", at=2, side=1, line=2, cex=1.2)
mtext("Independent", at=5.3, side=1, line=2, cex=1.2)	

graphics.off()


#Step 3######################################################################
############Pearson statistic calculation for Model.YOR.reduced5.off

Model.YOR.reduced5.off <- glmer(YORTotal ~
			InvSexCode + VisionCode+ InvInfDepCode +
			VisionCode:InvInfDepCode + 
			(1 | GroupName), family=poisson, offset=log.TimeTraveled)

summary(Model.YOR.reduced5.off)

std.resids <- (YORTotal - fitted(Model.YOR.reduced5.off))/sqrt(fitted(Model.YOR.reduced5.off))
Pearson.stat <- sum(std.resids^2)
Pearson.stat


#Step 4######################################################################
############Residual plots for Model.YOR.reduced5.off

#fitted values from the model on the natural scale
fit.vals <- fitted(Model.YOR.reduced5.off)
fit.vals

#residuals: YORTotal-fit.vals
resid.vals <- resid(Model.YOR.reduced5.off)
resid.vals

#residual plot
plot(fit.vals, resid.vals)
curve(0*x, add=TRUE)

#######Bin the residuals following G&H 559
binnedplot(fit.vals, resid.vals, nclass=50) #50 bins


#Step 5######################################################################
#############Conventional tests of contrasts: Schenker and Gentleman

summary(Model.YOR.reduced5.off)

#conditions:
#                     Fem=0  di=0 Dep=0
# 		     Int   Sex   Vis   Inf   Vis:Inf      
fem.dep.di <- c(	1,    0,    0,    0,     0)      
fem.ind.di <- c(	1,    0,    0,    1,     0)      
fem.dep.tri <- c(	1,    0,    1,    0,     0)     
fem.ind.tri <- c(	1,    0,    1,    1,     1)
mal.dep.di <- c(	1,    1,    0,    0,     0)      
mal.ind.di <- c(	1,    1,    0,    1,     0)         

#model estimates:
eta.fem.dep.di <-  t(fem.dep.di) %*% fixef(Model.YOR.reduced5.off)
eta.fem.ind.di <-  t(fem.ind.di) %*% fixef(Model.YOR.reduced5.off)
eta.fem.dep.tri <- t(fem.dep.tri) %*% fixef(Model.YOR.reduced5.off)
eta.fem.ind.tri <- t(fem.ind.tri) %*% fixef(Model.YOR.reduced5.off)
eta.mal.dep.di <-  t(mal.dep.di) %*% fixef(Model.YOR.reduced5.off)
eta.mal.ind.di <-  t(mal.ind.di) %*% fixef(Model.YOR.reduced5.off)

###Bonferroni correction for making 5 comparisons (last two of the six are same):
qtl <- qnorm(p=0.05/5, lower.tail=FALSE) 



#####Difference: Tri Fem w/ Dep Infant - Di Fem w/ Dep Infant
delta1 <- eta.fem.dep.tri - eta.fem.dep.di
#delta1 also equals coef for Vision (via algebra)
fixef(Model.YOR.reduced5.off)[3]

#Therefore variance of delta is variance of coef for Vision
#In vcov(Model.YOR.reduced5.off), indices refer to:
#1.Int, 2.Sex, 3.Vis, 4.Inf, 5.Vis:Inf
#variance for coef of Vis = variance of delta1 is:
var_delta1 <- vcov(Model.YOR.reduced5.off)[3,3]

#####MG alternative method for calculation of the variance of delta:
the.contrast1 <- fem.dep.tri - fem.dep.di
the.variance1 <- t(the.contrast1) %*% vcov(Model.YOR.reduced5.off) %*% 
	the.contrast1  
##### The matrix product above gives the.variance as a 1 x 1 matrix,
##### so to use it as a number below we have to cast it to numeric: 
the.variance1 <- as.numeric(the.variance1)

#Check whether 95% CI around delta contain 0, in which case difference non-signif
up1 <- delta1 + qtl * sqrt(the.variance1)
lw1 <- delta1 - qtl * sqrt(the.variance1)



#####Difference: Tri Fem w/ Indep Infant - Di Fem w/ Indep Infant
delta2 <- eta.fem.ind.tri - eta.fem.ind.di
#delta2 also equals coef for Vision + coef for Vis:Inf (via algebra)
fixef(Model.YOR.reduced5.off)[3] + fixef(Model.YOR.reduced5.off)[5]

#####MG alternative method for calculation of the variance of delta:
the.contrast2 <- fem.ind.tri - fem.ind.di
the.variance2 <- t(the.contrast2) %*% vcov(Model.YOR.reduced5.off) %*% 
	the.contrast2  
the.variance2 <- as.numeric(the.variance2)

#####MG Note that my calculation and yours differ here. The reason is
#####that in the algebraic expression you need to count the covariances
#####twice: 
var_delta2 <- vcov(Model.YOR.reduced5.off)[3,3] + 
		  vcov(Model.YOR.reduced5.off)[5,5] +
		  2 * vcov(Model.YOR.reduced5.off)[3,5]

#####MG Now we agree. 

#Check whether 95% CI around delta contain 0, in which case difference non-signif
up2 <- delta2 + qtl * sqrt(the.variance2)
lw2 <- delta2 - qtl * sqrt(the.variance2)



#####Difference: Tri Fem w/ Dep Infant - Di Mal w/ Dep Infant
delta3 <- eta.fem.dep.tri - eta.mal.dep.di
#delta3 also equals coef for Vision - Sex (via algebra)
fixef(Model.YOR.reduced5.off)[3] - fixef(Model.YOR.reduced5.off)[2]

the.contrast3 <- fem.dep.tri - mal.dep.di
the.variance3 <- t(the.contrast3) %*% vcov(Model.YOR.reduced5.off) %*% 
	the.contrast3  
the.variance3 <- as.numeric(the.variance3)

up3 <- delta3 + qtl * sqrt(the.variance3)
lw3 <- delta3 - qtl * sqrt(the.variance3)



#####Difference: Tri Fem w/ Indep Infant - Di Mal w/ Indep Infant
delta4 <- eta.fem.ind.tri - eta.mal.ind.di
#delta4 also equals coef for Vision - Sex + Vis:Inf (via algebra)
fixef(Model.YOR.reduced5.off)[3] - fixef(Model.YOR.reduced5.off)[2] +
	fixef(Model.YOR.reduced5.off)[5]

the.contrast4 <- fem.ind.tri - mal.ind.di
the.variance4 <- t(the.contrast4) %*% vcov(Model.YOR.reduced5.off) %*% 
	the.contrast4  
the.variance4 <- as.numeric(the.variance4)

up4 <- delta4 + qtl * sqrt(the.variance4)
lw4 <- delta4 - qtl * sqrt(the.variance4)



#####Difference: Di Fem w/ Dep Infant - Di Mal w/ Dep Infant
delta5 <- eta.fem.dep.di - eta.mal.dep.di
#delta5 also equals coef for -1*Sex (via algebra)
-1*fixef(Model.YOR.reduced5.off)[2]

the.contrast5 <- fem.dep.di - mal.dep.di
the.variance5 <- t(the.contrast5) %*% vcov(Model.YOR.reduced5.off) %*% 
	the.contrast5  
the.variance5 <- as.numeric(the.variance5)

up5 <- delta5 + qtl * sqrt(the.variance5)
lw5 <- delta5 - qtl * sqrt(the.variance5)



#####Difference: Di Fem w/ Indep Infant - Di Mal w/ Indep Infant
delta6 <- eta.fem.ind.di - eta.mal.ind.di
#delta6 also equals coef for -1*Sex (via algebra)
-1*fixef(Model.YOR.reduced5.off)[2]

the.contrast6 <- fem.ind.di - mal.ind.di
the.variance6 <- t(the.contrast6) %*% vcov(Model.YOR.reduced5.off) %*% 
	the.contrast6  
the.variance6 <- as.numeric(the.variance6)

up6 <- delta6 + qtl * sqrt(the.variance6)
lw6 <- delta6 - qtl * sqrt(the.variance6)



############Make table of comparisons. Even with the new model fitting, this still roughly corresponds with Table 2
comparisons <- c("F_Tri_Dep - F_Di_Dep", "F_Tri_Ind - F_Di_Ind", "F_Tri_Dep - M_Dep",
	"F_Tri_Ind - M_Ind", "F_Di_Dep - M_Dep", "F_Di_Ind - M_Ind")
uppers <- c(up1, up2, up3, up4, up5, up6)
lowers <- c(lw1, lw2, lw3, lw4, lw5, lw6)

comp_table <- cbind(comparisons, lowers, uppers)
comp_table

