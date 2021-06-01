
#Read the data from the csv data file into R:
InterObserv.raw <- read.csv(file="./Data/Joh_Ine_all_UnObsBehavOmit.csv", header=TRUE)

#Check the variable names and dimensions in the data frame InterObserv.raw
names(InterObserv.raw)
dim(InterObserv.raw)

#Attach the data frame
attach(InterObserv.raw)


###################Overall Durations
All.Dur <- cbind(Ine_Duration_Sec, Joh_Duration_Sec)
All.Dur

icc(All.Dur, "twoway", "agreement")

###################Behavior Durations

###Trial 1

Behavior1 <- subset(InterObserv.raw, Target_Class == "Behav" &
		Trial_Number==1)
Behavior1

Behav.Dur1 <- cbind(Behavior1$Ine_Duration_Sec, Behavior1$Joh_Duration_Sec)
Behav.Dur1

ICC.1 <- icc(Behav.Dur1, "twoway", "agreement")
ICC.1

###Trial 2

Behavior2 <- subset(InterObserv.raw, Target_Class == "Behav" &
		Trial_Number==2)
Behavior2

Behav.Dur2 <- cbind(Behavior2$Ine_Duration_Sec, Behavior2$Joh_Duration_Sec)
Behav.Dur2

ICC.2 <- icc(Behav.Dur2, "twoway", "agreement")
ICC.2


###Trial 3

Behavior3 <- subset(InterObserv.raw, Target_Class == "Behav" &
		Trial_Number==3)
Behavior3

Behav.Dur3 <- cbind(Behavior3$Ine_Duration_Sec, Behavior3$Joh_Duration_Sec)
Behav.Dur3

ICC.3 <- icc(Behav.Dur3, "twoway", "agreement")
ICC.3


###Trial 4

Behavior4 <- subset(InterObserv.raw, Target_Class == "Behav" &
		Trial_Number==4)
Behavior4

Behav.Dur4 <- cbind(Behavior4$Ine_Duration_Sec, Behavior4$Joh_Duration_Sec)
Behav.Dur4

ICC.4 <- icc(Behav.Dur4, "twoway", "agreement")
ICC.4

###Trial 5

Behavior5 <- subset(InterObserv.raw, Target_Class == "Behav" &
		Trial_Number==5)
Behavior5

Behav.Dur5 <- cbind(Behavior5$Ine_Duration_Sec, Behavior5$Joh_Duration_Sec)
Behav.Dur5

ICC.5 <- icc(Behav.Dur5, "twoway", "agreement")
ICC.5


###Trial 6

Behavior6 <- subset(InterObserv.raw, Target_Class == "Behav" &
		Trial_Number==6)
Behavior6

Behav.Dur6 <- cbind(Behavior6$Ine_Duration_Sec, Behavior6$Joh_Duration_Sec)
Behav.Dur6

ICC.6 <- icc(Behav.Dur6, "twoway", "agreement")
ICC.6


###Trial 7

Behavior7 <- subset(InterObserv.raw, Target_Class == "Behav" &
		Trial_Number==7)
Behavior7

Behav.Dur7 <- cbind(Behavior7$Ine_Duration_Sec, Behavior7$Joh_Duration_Sec)
Behav.Dur7

ICC.7 <- icc(Behav.Dur7, "twoway", "agreement")
ICC.7


###Trial 8

Behavior8 <- subset(InterObserv.raw, Target_Class == "Behav" &
		Trial_Number==8)
Behavior8

Behav.Dur8 <- cbind(Behavior8$Ine_Duration_Sec, Behavior8$Joh_Duration_Sec)
Behav.Dur8

ICC.8 <- icc(Behav.Dur8, "twoway", "agreement")
ICC.8

#####Summary

summ <- c("ICC coefficient", ICC.1$value, ICC.2$value,
		ICC.3$value, ICC.4$valuet, ICC.5$value,
		ICC.6$value, ICC.7$value, ICC.8$value, 
		mean(c(ICC.1$value, ICC.2$value,
		ICC.3$value, ICC.5$value,
		ICC.6$value, ICC.7$value, ICC.8$value)))
summ


detach(InterObserv.raw)