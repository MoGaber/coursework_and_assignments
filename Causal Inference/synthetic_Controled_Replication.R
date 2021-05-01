################################################
# R Code for Synthetic Controls Analysis
# "Do Good Borders Make Good Rebels?"
# Journal of Politics Replication File
# Yu-Ming Liou and Megan Stewart
# Department of Government
# Georgetown University
# 
# Code Last Modified 6/15/2016 (Yu-Ming Liou)
#
################################################

################################################
# Description
#
# This code replicates analyses necessary to
# generate Figures 4-5
#
################################################

################################################
# Data Cleaning And Loading
################################################


setwd("C:/Users/meganstewart/Documents/YML, Sanctuary Documents/Stewart and Liou Replication Files")

library(foreign)
synth.data <- read.dta("PKK osv synth.dta")

treated <- 531
# Note that UNITA (608) cannot be used as a control,
# because it is missing data for 1995. 
# Therefore, we exclude it here.

controls <- c(372,385,445,452,501,502,571,579,617)

synth.data$insurgentnum<-as.numeric(synth.data$insurgentnum)

#Not including nonmilsupport as a predictor since only FARC gets it.
#Not including ethnic as a special predictor b/c all cases coded as non-ethnic conflicts

library(Synth)
dataprep.out <-
  dataprep(
    foo = synth.data,
    predictors = c("popdensity","gdppc_log","rebestsize",
                   "rebstrength_ord","milsupport"),
    predictors.op = "mean",
    dependent = "oneside_best",
    unit.variable = "insurgentnum",
    time.variable = "year",
    special.predictors =list(
#     list("ethnic",1989, "mean"),
      list("terrdum",1989, "mean")),
    treatment.identifier = treated,
    controls.identifier = controls,
    time.predictors.prior = c(1989:1990),
    time.optimize.ssr = c(1989:1991),
    unit.names.variable = "side_b",
    time.plot = 1989:1995
  )
synth.out <- synth(dataprep.out, method = "BFGS")

################################################
# Data Output
################################################

tables <- synth.tab(synth.res = synth.out, dataprep.res = dataprep.out)

print(tables$tab.v)
print(tables$tab.w)

path.plot(synth.res = synth.out, dataprep.res = dataprep.out,  
          Ylab = "One-Sided Violence", Xlab = "Year", 
          Main = "Synthetic vs. Observed, One-Sided Violence (PKK)", 
          Legend = c("Treated", "Synthetic"), 
          Legend.position = c("topright"), tr.intake = 1991)

# Specify the ylim to plot placebo gaps plots on the same scale.
gaps.plot(synth.res = synth.out, dataprep.res = dataprep.out, 
          Ylab = "One-Sided Violence", Xlab = "Year", 
          Main = "Gaps Plot: Observed-Synthetic One-Sided Violence (PKK)",
          tr.intake=1991)

################################################
# Placebo Test
################################################

universe <- c(controls,treated)
placebo.broad <- universe

for(i in 1:length(placebo.broad)){
nam <- paste("dataprep.out.placebo",i,sep=".")
assign(nam,1:i)
}

for(i in 1:length(placebo.broad)){
	
	placebo.controls <- subset(universe,universe!=placebo.broad[i])
	
	temp.out<-
		dataprep(
			foo = synth.data,	
		predictors = c("popdensity","rgdp96pc","rebestsize",
		               "rebstrength_ord","milsupport"),
		predictors.op = "mean",
		dependent = "oneside_best",
		unit.variable = "insurgentnum",
		time.variable = "year",
		special.predictors =list(
#		  list("ethnic",1989, "mean"),
		  list("terrdum",1989, "mean")),
		treatment.identifier = placebo.broad[i],
		controls.identifier = unique(placebo.controls),
		time.predictors.prior = c(1989:1990),
		time.optimize.ssr = c(1989:1991),
		unit.names.variable = "side_b",
		time.plot = 1989:1995
		)
		
		nam <- paste("dataprep.out.placebo",i,sep=".")
		assign(nam,temp.out)	
	}

#	There will be 10 synthetic control units. (The last one will never be used because it is always the treated synth.)

	synth.out.1 <- synth(dataprep.out.placebo.1, method = "BFGS")

	synth.out.2 <- synth(dataprep.out.placebo.2, method = "BFGS")

	synth.out.3 <- synth(dataprep.out.placebo.3, method = "BFGS")

	synth.out.4 <- synth(dataprep.out.placebo.4, method = "BFGS")

	synth.out.5 <- synth(dataprep.out.placebo.5, method = "BFGS")
	
	synth.out.6 <- synth(dataprep.out.placebo.6, method = "BFGS")
	
	synth.out.7 <- synth(dataprep.out.placebo.7, method = "BFGS")

  synth.out.8 <- synth(dataprep.out.placebo.8, method = "BFGS")

  synth.out.9 <- synth(dataprep.out.placebo.9, method = "BFGS")

	gap.1 <- dataprep.out.placebo.1$Y1plot - (dataprep.out.placebo.1$Y0plot %*% synth.out.1$solution.w)
	
	gap.2 <- dataprep.out.placebo.2$Y1plot - (dataprep.out.placebo.2$Y0plot %*%  synth.out.2$solution.w)
	
	gap.3 <- dataprep.out.placebo.3$Y1plot - (dataprep.out.placebo.3$Y0plot %*% synth.out.3$solution.w)

	gap.4 <- dataprep.out.placebo.4$Y1plot - (dataprep.out.placebo.4$Y0plot %*% synth.out.4$solution.w)

	gap.5 <- dataprep.out.placebo.5$Y1plot - (dataprep.out.placebo.5$Y0plot %*% synth.out.5$solution.w)

	gap.6 <- dataprep.out.placebo.6$Y1plot - (dataprep.out.placebo.6$Y0plot %*% synth.out.6$solution.w)

	gap.7 <- dataprep.out.placebo.7$Y1plot - (dataprep.out.placebo.7$Y0plot %*% synth.out.7$solution.w)

  gap.8 <- dataprep.out.placebo.8$Y1plot - (dataprep.out.placebo.8$Y0plot %*% synth.out.8$solution.w)

  gap.9 <- dataprep.out.placebo.9$Y1plot - (dataprep.out.placebo.9$Y0plot %*% synth.out.9$solution.w)

	# PKK
	gap.prime <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
	
	###Placebo Plotting Code

		gap.plot <- cbind(gap.1,gap.2,gap.5,gap.6,gap.7,gap.8,gap.9,gap.prime)
		years<-cbind(1989:1995) 

		plot(years,gap.1,type="l",lwd=2,col="gray",ylim=range(gap.plot),main = "PKK One-Sided Violence Placebo Test", ylab="One-Sided Violence")
		lines(years,gap.1,type="l",lwd=2,col="gray")
		lines(years,gap.2,type="l",lwd=2,col="gray")
		lines(years,gap.3,type="l",lwd=2,col="gray")
		lines(years,gap.4,type="l",lwd=2,col="gray")
		lines(years,gap.5,type="l",lwd=2,col="gray")
		lines(years,gap.6,type="l",lwd=2, col="gray")
		lines(years,gap.7,type="l",lwd=2,col="gray")
    lines(years,gap.8,type="l",lwd=2,col="gray")
    lines(years,gap.9,type="l",lwd=2,col="gray")
		lines(years,gap.prime,type="l",lwd=2, col="black")
		abline(h=0)
		abline(v = 1991, col = "black", lty = "dotted", lwd = 2)