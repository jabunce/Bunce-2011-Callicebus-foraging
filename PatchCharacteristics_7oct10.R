
#Read the data from the csv data file into R:
Patches.raw <- read.csv(file="./Data/PatchCharacteristics_19oct09.csv", header=TRUE)


#Check the variable names and dimensions in the data frame Patches.raw
names(Patches.raw)
dim(Patches.raw)


###################Dotplot of Memorable vs. Alone Once patches

AlOnce.data <- subset(Patches.raw, Labels %in% 
		c("TogetherORnotOnce", "Al_Once_Fem", "Al_Once_Mal")) 

ClowCI <- AlOnce.data$Clower
CUppCI <- AlOnce.data$Cupper
TlowCI <- AlOnce.data$Tlower
TUppCI <- AlOnce.data$Tupper

AlOnce.CI <- data.frame(AlOnce.data, ClowCI, CUppCI, TlowCI, TUppCI)


#arrange increasing order of means
AlOnce.CI$Labels2 <- reorder(AlOnce.CI$Labels2,
	AlOnce.CI$CountPercMean)

memplot <- dotplot(Labels2 ~ CountPercMean, data = AlOnce.CI,
	aspect = "fill",
	xlim = c(-0.05, 1.05),
	xlab = "Proportion",
	scales = list(tck=c(1, 0), alternating=c(1), lwd=2, fontface="plain",
				cex=1),
	panel = function (x, y) {
		
		panel.xyplot(AlOnce.CI$TimePercMean, y, pch = 16, cex=1.4, 
			col = grey(0.6))
		panel.segments(AlOnce.CI$TlowCI, as.numeric(y),
		AlOnce.CI$TUppCI, as.numeric(y), lty = 1, lwd = 5, col = grey(0.6))

		panel.xyplot(x, y, pch = 16, cex=1, col = "black")
		panel.segments(AlOnce.CI$ClowCI, as.numeric(y),
		AlOnce.CI$CUppCI, as.numeric(y), lty = 1, lwd=2.5, col = "black")

		grid.text("c", x=unit(0, "npc")+unit(3.2, "mm"),
				y=unit(1, "npc")-unit(3.5, "mm"),
				gp=gpar(cex=1.5, fontface="bold"))

		grid.rect(gp=gpar(lwd=2))
	})

#######################Dotplot of All YOR

AllYOR.data <- subset(Patches.raw, Labels %in% 
		c("YORall", "Other")) 

ClowCI <- AllYOR.data$Clower
CUppCI <- AllYOR.data$Cupper
TlowCI <- AllYOR.data$Tlower
TUppCI <- AllYOR.data$Tupper

AllYOR.CI <- data.frame(AllYOR.data, ClowCI, CUppCI, TlowCI, TUppCI)


#arrange increasing order of means
AllYOR.CI$Labels2 <- reorder(AllYOR.CI$Labels2,
	AllYOR.CI$CountPercMean)

YORplot <- dotplot(Labels2 ~ CountPercMean, data = AllYOR.CI,
	aspect = "fill",
	xlim = c(-0.05, 1.05),
	xlab = "",
	scales = list(tck=c(1, 1), alternating=c(0), lwd=2, fontface="plain",
			cex=1), 
	panel = function (x, y) {
		
		panel.xyplot(AllYOR.CI$TimePercMean, y, pch = 16, cex=1.4,
			col = grey(0.6))
		panel.segments(AllYOR.CI$TlowCI, as.numeric(y),
		AllYOR.CI$TUppCI, as.numeric(y), lty = 1, lwd=5, col = grey(0.6))

		panel.xyplot(x, y, pch = 16, cex=1, col = "black")
		panel.segments(AllYOR.CI$ClowCI, as.numeric(y),
		AllYOR.CI$CUppCI, as.numeric(y), lty = 1, lwd=2.5, col = "black")

		grid.text("b", x=unit(0, "npc")+unit(3.2, "mm"),
				y=unit(1, "npc")-unit(3.5, "mm"),
				gp=gpar(cex=1.5, fontface="bold"))

		grid.rect(gp=gpar(lwd=2))
	})


#######################Dotplot of All FoodType

AllType.data <- subset(Patches.raw, Labels %in% 
		c("flower", "fruit", "insect", "leaf/stem", "other", "unknown")) 

ClowCI <- AllType.data$Clower
CUppCI <- AllType.data$Cupper
TlowCI <- AllType.data$Tlower
TUppCI <- AllType.data$Tupper

AllType.CI <- data.frame(AllType.data, ClowCI, CUppCI, TlowCI, TUppCI)


#arrange increasing order of means
AllType.CI$Labels2 <- reorder(AllType.CI$Labels2,
	AllType.CI$CountPercMean)

foodplot <- dotplot(Labels2 ~ CountPercMean, data = AllType.CI,
	aspect = "fill",
	xlim = c(-0.05, 1.05),
	xlab = "",
	scales = list(tck=c(0, 1), alternating=c(2), lwd=2, fontface="plain",
			cex=1),
	panel = function (x, y) {
		
		panel.xyplot(AllType.CI$TimePercMean, y, pch = 16, cex=1.4,
			col = grey(0.6))
		panel.segments(AllType.CI$TlowCI, as.numeric(y),
		AllType.CI$TUppCI, as.numeric(y), lty = 1, lwd=5, col = grey(0.6))

		panel.xyplot(x, y, pch = 16, cex=1, col = "black")
		panel.segments(AllType.CI$ClowCI, as.numeric(y),
		AllType.CI$CUppCI, as.numeric(y), lty = 1, lwd=2.5, col = "black")

		grid.text("a", x=unit(0, "npc")+unit(3.5, "mm"),
				y=unit(1, "npc")-unit(3.5, "mm"),
				gp=gpar(cex=1.5, fontface="bold"))

		grid.rect(gp=gpar(lwd=2))		
	},
	key = list(text = list(c("Prop Daily Patch Encounters",
		"Prop Daily Foraging Time"), cex = .8),
		points = list(pch = c(16, 16), col = c("black", grey(0.6)),
		cex = c(1, 1.4)),
		x=0.98, y=0.05, corner=c(1,0), border=F, padding.text=2.2)
	)
	


##########################################################################
######Arrange 3 plots in pdf

pdf(file="./Plots/Figure1.pdf", 
	height=10, width=8)

print(foodplot, panel.width=list(7, "cm"), panel.height=list(4, "cm"), 
		position=c(0,0,1,1.262), more=TRUE)
grid.rect(x=0.446, y=0.55, width=0.27, height=0.039, gp=gpar(lwd=1.3,
		col="black"), hjust=0, vjust=0) #key border
print(YORplot, panel.width=list(7.012, "cm"), panel.height=list(1.55, "cm"),
		position=c(0,0,1.005,1), more=TRUE)
print(memplot, panel.width=list(7.03, "cm"), panel.height=list(2.2, "cm"), 
		position=c(0,0,0.9168,0.809))

graphics.off()


