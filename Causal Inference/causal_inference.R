

# Setting Data for Question 2:
library('Matching')
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
# extract relevant columns
foo <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]
# remove 2 rows with missing data (there are better ways to handle missing data)
foo <- foo[c(-19, -47), ]
# check that all missing data is gone...
which(is.na(foo) == TRUE)


#-----------Question 2 -----------------------------------------------------------------------------------

######### --------- Replicating the figure with wardur on the x axis:

#The paper Original Model:
glm.original = glm(pbs2s3~ wartype+logcost+wardur+factnum+factnum2+trnsfcap+untype4+treaty+develop+exp+decade , data = foo, family = binomial)




outcome_function = function(units, coefs ){
  
  value = coefs[1] + units[1]*coefs[2] +
    units[2]*coefs[3] +
    units[3]*coefs[4] + 
    units[4]*coefs[5] +
    units[5]*coefs[6] +
    units[6]*coefs[7] +
    units[7]*coefs[8] +
    units[8]*coefs[9] +
    units[9]*coefs[10] +
    units[10]*coefs[11] +
    units[11]*coefs[12]

  
  
  return(exp(value) / (1 + exp(value)))
}



#store the treatment outocme:
storage.list.tr.original = c()
#store the control outcome:
storage.list.cr.original = c()
#store values from 0 to 315 for war duration
war_duration = c()

#for loop to iterate over different values of war duration and get the correspondind treatment effect
for (dur in 0:315){
  war_duration = c(war_duration, dur)
  unit.tr = c(mean(foo$wartype), mean(foo$logcost), dur,mean(foo$factnum),mean(foo$factnum2), mean(foo$trnsfcap), 1, mean(foo$treaty), mean(foo$develop), mean(foo$exp), mean(foo$decade))
  unit.cr = c(mean(foo$wartype), mean(foo$logcost), dur,mean(foo$factnum),mean(foo$factnum2), mean(foo$trnsfcap), 0, mean(foo$treaty), mean(foo$develop), mean(foo$exp), mean(foo$decade))
  effect.tr = outcome_function(unit.tr, glm.original$coefficients)
  effect.cr = outcome_function(unit.cr, glm.original$coefficients)
  storage.list.tr.original = c(storage.list.tr.original, effect.tr)
  storage.list.cr.original = c(storage.list.cr.original,effect.cr)
}


#calculating the treatment effect:
treatment.effect.orignial = storage.list.tr.original-storage.list.cr.original




#--------------------------------------------------------------------
#The original paper modified model:
glm.modified = glm(pbs2s3~ wartype+logcost+wardur+factnum+factnum2+trnsfcap+untype4+treaty+develop+exp+decade+ I(wardur*untype4) , data = foo, family = binomial)

#function to calculate the outcome:
outcome_function.modified = function(units, coefs ){
  
  value = coefs[1] + units[1]*coefs[2] +
    units[2]*coefs[3] +
    units[3]*coefs[4] + 
    units[4]*coefs[5] +
    units[5]*coefs[6] +
    units[6]*coefs[7] +
    units[7]*coefs[8] +
    units[8]*coefs[9] +
    units[9]*coefs[10] +
    units[10]*coefs[11] +
    units[11]*coefs[12]+
    units[7]*units[3]*coefs[13]
  
  
  
  return(exp(value) / (1 + exp(value)))
}


#store the outcome for treatment
storage.list.tr.modified = c()

#store the oucome for control
storage.list.cr.modified = c()

#store values from 0 to 315 for war duration
war_duration = c()


#iterate over different values for war duration:
for (dur in 0:315){
  war_duration = c(war_duration, dur)
  unit.tr = c(mean(foo$wartype), mean(foo$logcost), dur,mean(foo$factnum),mean(foo$factnum2), mean(foo$trnsfcap), 1, mean(foo$treaty), mean(foo$develop), mean(foo$exp), mean(foo$decade))
  unit.cr = c(mean(foo$wartype), mean(foo$logcost), dur,mean(foo$factnum),mean(foo$factnum2), mean(foo$trnsfcap), 0, mean(foo$treaty), mean(foo$develop), mean(foo$exp), mean(foo$decade))
  effect.tr = outcome_function.modified(unit.tr, glm.modified$coefficients)
  effect.cr = outcome_function.modified(unit.cr, glm.modified$coefficients)
  storage.list.tr.modified = c(storage.list.tr.modified, effect.tr)
  storage.list.cr.modified = c(storage.list.cr.modified,effect.cr)
}




#calculate the treatment effect:
treatment.effect.modified = storage.list.tr.modified-storage.list.cr.modified



#-------------------------------------------------------------------------------
# the new model with required interaction (logcost*untype4)
glm.new = glm(pbs2s3~ wartype+logcost+wardur+factnum+factnum2+trnsfcap+untype4+treaty+develop+exp+decade+ I(logcost*untype4) , data = foo, family = binomial)


#function to calculate the outcome values
outcome_function.new = function(units, coefs ){
  
  value = coefs[1] + units[1]*coefs[2] +
    units[2]*coefs[3] +
    units[3]*coefs[4] + 
    units[4]*coefs[5] +
    units[5]*coefs[6] +
    units[6]*coefs[7] +
    units[7]*coefs[8] +
    units[8]*coefs[9] +
    units[9]*coefs[10] +
    units[10]*coefs[11] +
    units[11]*coefs[12]+
    units[2]*units[7]*coefs[13]
  
  
  
  return(exp(value) / (1 + exp(value)))
}


#storing the outcome for the treatment group:
storage.list.tr.new = c()
#storing the ouctome for the control group:
storage.list.cr.new = c()
#storing the duration values from 0 to 315
war_duration = c()

for (dur in 0:315){
  war_duration = c(war_duration, dur)
  unit.tr = c(mean(foo$wartype), mean(foo$logcost), dur,mean(foo$factnum),mean(foo$factnum2), mean(foo$trnsfcap), 1, mean(foo$treaty), mean(foo$develop), mean(foo$exp), mean(foo$decade))
  unit.cr = c(mean(foo$wartype), mean(foo$logcost), dur,mean(foo$factnum),mean(foo$factnum2), mean(foo$trnsfcap), 0, mean(foo$treaty), mean(foo$develop), mean(foo$exp), mean(foo$decade))
  effect.tr = outcome_function.new(unit.tr, glm.new$coefficients)
  effect.cr = outcome_function.new(unit.cr, glm.new$coefficients)
  storage.list.tr.new = c(storage.list.tr.new, effect.tr)
  storage.list.cr.new = c(storage.list.cr.new,effect.cr)
}




#calculating the treatment effect:
treatment.effect.new = storage.list.tr.new-storage.list.cr.new





#------------plotting:

plot(x = war_duration, y =treatment.effect.modified,ylim = c(0:1), type='n', main = "Causal Effect of Multidimensional UN Peacekeeping Operations", xlab = "Duration of wars in months", 
     ylab = "Marginal effects of UN peacekeeping operations")

#original model plot
lines(x = war_duration, y =treatment.effect.orignial, col='black', lty=2 )
#modified model plot
lines(x = war_duration, y =treatment.effect.modified, col='black' )
# new modified model
lines(x = war_duration, y =treatment.effect.new, col= 'red' )


legend("topright", legend=c("The paper original model","the paper modified model (wardur *untype4)", "The new model (logcost*untype4)"),
       col=c("black",  "black","red" ), lty=c(2,1,1))

##################################################################################################
##################################################################################################


############## Replicating the model with logcost on the x-axis------------------------------------------------------------


#-------------The paper Original Model:
glm.original = glm(pbs2s3~ wartype+logcost+wardur+factnum+factnum2+trnsfcap+untype4+treaty+develop+exp+decade , data = foo, family = binomial)



outcome_function = function(units, coefs ){
  
  value = coefs[1] + units[1]*coefs[2] +
    units[2]*coefs[3] +
    units[3]*coefs[4] + 
    units[4]*coefs[5] +
    units[5]*coefs[6] +
    units[6]*coefs[7] +
    units[7]*coefs[8] +
    units[8]*coefs[9] +
    units[9]*coefs[10] +
    units[10]*coefs[11] +
    units[11]*coefs[12]
  
  
  
  return(exp(value) / (1 + exp(value)))
}



#store the treatment outocme:
storage.list.tr.original = c()
#store the control outcome:
storage.list.cr.original = c()
#store values from 0 to 315 for war duration
logcost = c()


#for loop to iterate over different values of war duration and get the correspondind treatment effect
for (cost in min(foo$logcost):max(foo$logcost)){
  logcost = c(logcost, cost)
  unit.tr = c(mean(foo$wartype), cost, mean(foo$wardur),mean(foo$factnum),mean(foo$factnum2), mean(foo$trnsfcap), 1, mean(foo$treaty), mean(foo$develop), mean(foo$exp), mean(foo$decade))
  unit.cr = c(mean(foo$wartype), cost, mean(foo$wardur),mean(foo$factnum),mean(foo$factnum2), mean(foo$trnsfcap), 0, mean(foo$treaty), mean(foo$develop), mean(foo$exp), mean(foo$decade))
  effect.tr = outcome_function(unit.tr, glm.original$coefficients)
  effect.cr = outcome_function(unit.cr, glm.original$coefficients)
  storage.list.tr.original = c(storage.list.tr.original, effect.tr)
  storage.list.cr.original = c(storage.list.cr.original,effect.cr)
}


#calculating the treatment effect:
treatment.effect.orignial = storage.list.tr.original-storage.list.cr.original




#---------------The original paper' modified model:
glm.modified = glm(pbs2s3~ wartype+logcost+wardur+factnum+factnum2+trnsfcap+untype4+treaty+develop+exp+decade+ I(wardur*untype4) , data = foo, family = binomial)

#function to calculate the outcome:
outcome_function.modified = function(units, coefs ){
  
  value = coefs[1] + units[1]*coefs[2] +
    units[2]*coefs[3] +
    units[3]*coefs[4] + 
    units[4]*coefs[5] +
    units[5]*coefs[6] +
    units[6]*coefs[7] +
    units[7]*coefs[8] +
    units[8]*coefs[9] +
    units[9]*coefs[10] +
    units[10]*coefs[11] +
    units[11]*coefs[12]+
    units[7]*units[3]*coefs[13]
  
  
  
  return(exp(value) / (1 + exp(value)))
}


#store the outcome for treatment
storage.list.tr.modified = c()

#store the oucome for control
storage.list.cr.modified = c()

#store values from 0 to 315 for war duration
logcost = c()


#iterate over different values for war duration:
for (cost in min(foo$logcost):max(foo$logcost)){
  logcost = c(logcost, cost)
  unit.tr = c(mean(foo$wartype), cost, mean(foo$wardur),mean(foo$factnum),mean(foo$factnum2), mean(foo$trnsfcap), 1, mean(foo$treaty), mean(foo$develop), mean(foo$exp), mean(foo$decade))
  unit.cr = c(mean(foo$wartype), cost, mean(foo$wardur),mean(foo$factnum),mean(foo$factnum2), mean(foo$trnsfcap), 0, mean(foo$treaty), mean(foo$develop), mean(foo$exp), mean(foo$decade))
  effect.tr = outcome_function.modified(unit.tr, glm.modified$coefficients)
  effect.cr = outcome_function.modified(unit.cr, glm.modified$coefficients)
  storage.list.tr.modified = c(storage.list.tr.modified, effect.tr)
  storage.list.cr.modified = c(storage.list.cr.modified,effect.cr)
}




#calculate the treatment effect:
treatment.effect.modified = storage.list.tr.modified-storage.list.cr.modified



#------------------------new model----------------------------------------------
# the new model with required interaction (logcost*untype4)
glm.new = glm(pbs2s3~ wartype+logcost+wardur+factnum+factnum2+trnsfcap+untype4+treaty+develop+exp+decade+ I(logcost*untype4) , data = foo, family = binomial)


#function to calculate the outcome values
outcome_function.new = function(units, coefs ){
  
  value = coefs[1] + units[1]*coefs[2] +
    units[2]*coefs[3] +
    units[3]*coefs[4] + 
    units[4]*coefs[5] +
    units[5]*coefs[6] +
    units[6]*coefs[7] +
    units[7]*coefs[8] +
    units[8]*coefs[9] +
    units[9]*coefs[10] +
    units[10]*coefs[11] +
    units[11]*coefs[12]+
    units[2]*units[7]*coefs[13]
  
  
  
  return(exp(value) / (1 + exp(value)))
}


#storing the outcome for the treatment group:
storage.list.tr.new = c()
#storing the ouctome for the control group:
storage.list.cr.new = c()
#storing the duration values from 0 to 315
logcost = c()


for (cost in min(foo$logcost):max(foo$logcost)){
  logcost = c(logcost, cost)
  unit.tr = c(mean(foo$wartype), cost, mean(foo$wardur),mean(foo$factnum),mean(foo$factnum2), mean(foo$trnsfcap), 1, mean(foo$treaty), mean(foo$develop), mean(foo$exp), mean(foo$decade))
  unit.cr = c(mean(foo$wartype), cost, mean(foo$wardur),mean(foo$factnum),mean(foo$factnum2), mean(foo$trnsfcap), 0, mean(foo$treaty), mean(foo$develop), mean(foo$exp), mean(foo$decade))
  effect.tr = outcome_function.new(unit.tr, glm.new$coefficients)
  effect.cr = outcome_function.new(unit.cr, glm.new$coefficients)
  storage.list.tr.new = c(storage.list.tr.new, effect.tr)
  storage.list.cr.new = c(storage.list.cr.new,effect.cr)
}




#calculating the treatment effect:
treatment.effect.new = storage.list.tr.new-storage.list.cr.new

#------------plotting:

plot(x = logcost, y =treatment.effect.modified,ylim = c(0:1), type='n', main = "Causal Effect of Multidimensional UN Peacekeeping Operations", xlab = "logcost", 
     ylab = "Marginal effects of UN peacekeeping operations")

#original model plot
lines(x = logcost, y =treatment.effect.orignial, col='black', lty=2 )
#modified model plot
lines(x = logcost, y =treatment.effect.modified, col='black' )
# new modified model
lines(x = logcost, y =treatment.effect.new, col= 'red' )


legend("topright", legend=c("The paper original model","the paper modified model (wardur *untype4)", "The new model (logcost*untype4)"),
       col=c("black",  "black","red" ), lty=c(2,1,1))

########################---------------------------------------------------------------------------------------------



######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################


#Setting the data for question 4:

Q4_data <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
#deleting the rows that have NAs:
Q4_data <- Q4_data[c(-19, -47,-4, -16, -84, -93, -98), ]
which(is.na(Q4_data$pbs2l) == TRUE)



#---treatment:
Tr <- rep(0, length(Q4_data$uncint))
Tr[which(Q4_data$uncint != 'None' )] <- 1
Q4_data$Tr = Tr

# & Q4_data$uncint != 'Observer'


#---- outcome after two years----
len2years <- rep(0, length(Q4_data$pbs2l))
len2years[which(Q4_data$pbs2l == 'Success')] <- 1
len2years[which(Q4_data$pbs2l == 'Failure')] <- 0
#--------------

#----- outcome after five years---
len5years <- rep(0, length(Q4_data$pbs5l))
len5years[which(Q4_data$pbs5l == 'Success')] <- 1
len5years[which(Q4_data$pbs5l == 'Failure')] <- 0


#----------------------Question 4--------------------------------------------------


#--------Logistic Model-----------


logistic.model_2years = glm(len2years ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + Tr + treaty + 
                                  develop + exp + decade, data=Q4_data, family="binomial")

logistic.model_5years = glm(len5years ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + Tr + treaty + 
                              develop + exp + decade, data=Q4_data, family="binomial")



logistic.mb = MatchBalance(Tr~wartype + logcost + wardur + factnum + factnum2 + trnsfcap + Tr + treaty + 
                               develop + exp + decade, data=Q4_data,nboots=4000)

#treatment effect from the logistic model for 2 years
control.2years <- predict(logistic.model_2years,newdata=subset(Q4_data,Tr==0))
treat.2years <- predict(logistic.model_2years,newdata=subset(Q4_data,Tr==1))
tmt.2years = mean(treat.2years) - mean(control.2years)
tmt.2years

#treatment effect from the logistic model for 2 years
control.5years <- predict(logistic.model_5years,newdata=subset(Q4_data,Tr==0))
treat.5years <- predict(logistic.model_5years,newdata=subset(Q4_data,Tr==1))
tmt.5years = mean(treat.5years) - mean(control.5years)
tmt.5years

###

#---------------Propensity matching--------------------------


propensity_model = glm( Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty +
                          develop + exp + decade+wardur*logcost+wardur*factnum+treaty*exp,  data=Q4_data, family="binomial")


X <- propensity_model$fitted.values



matchout.prop <- Match(Tr = Tr, X=X)

mb.out <- MatchBalance(Tr ~  wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty +
                         develop + exp + decade+wardur*logcost+wardur*factnum+treaty*exp, data=Q4_data, match.out = matchout.prop, nboots=10000)



#getting the treatment effect after we have done the matching:
summary(Match(Y=len2years,  Tr = Tr, X=X, BiasAdjust = FALSE ))
summary(Match(Y=len2years,  Tr = Tr, X=X, BiasAdjust = TRUE ))

summary(Match(Y=len5years,  Tr = Tr, X=X, BiasAdjust = FALSE ))
summary(Match(Y=len5years,  Tr = Tr, X=X, BiasAdjust = TRUE ))



#----------------------Genetic Matching-----

X = cbind(Q4_data$wartype , Q4_data$logcost , Q4_data$wardur , Q4_data$factnum , Q4_data$factnum2 , Q4_data$trnsfcap , Q4_data$treaty , 
          Q4_data$develop , Q4_data$exp , Q4_data$decade, Q4_data$logcost*Q4_data$wardur, Q4_data$factnum*Q4_data$logcost, Q4_data$treaty*Q4_data$exp)


genout <- GenMatch(X = X, Tr = Tr, pop.size = 500,max.generations=500, wait.generations=15 )


matchout.gen <- Match(X = X, Tr = Tr, Weight.matrix=genout)

mb.out3 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + 
                          develop + exp + decade+ logcost*wardur+ factnum*logcost+ treaty*exp,data=Q4_data, match.out = matchout.gen, nboots=50000)


#getting the treatment effect after we have done the matching:
summary(Match(Y =len2years ,X = X, Tr = Tr, Weight.matrix=genout, BiasAdjust = FALSE))
summary(Match(Y =len2years ,X = X, Tr = Tr, Weight.matrix=genout, BiasAdjust = TRUE))

summary(Match(Y =len5years ,X = X, Tr = Tr, Weight.matrix=genout, BiasAdjust = FALSE))
summary(Match(Y =len5years ,X = X, Tr = Tr, Weight.matrix=genout, BiasAdjust = TRUE))



