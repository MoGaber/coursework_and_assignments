#
#
#
#
###############------------ QUESTION 1-------------------------------------------------------------------------------------------


Sample = runif(999, 1, 15)

Y = c()
X = c()
for(x in Sample){ 
  X = c(X, x)
  error = runif(1, -9, 9)
  y = 10*x +error
  Y = c(Y, y)
  
}
fake.dataset = data.frame(X,Y)


original.model = lm(Y~X, fake.dataset)
plot(fake.dataset$X, fake.dataset$Y, xlab = "x", ylab = 'y', main='The Regression line for the original 999 data points')
lines(fake.dataset$X,predict(original.model), lwd = 1,col = "blue")
summary(original.model)


fake.dataset.outlier = rbind(fake.dataset, c(3000, -3000)) #adding the outlier
outlier.model = lm(Y~X, fake.dataset.outlier)
plot(fake.dataset.outlier$X, fake.dataset.outlier$Y, xlab = "x", ylab = 'y', main='The Regression line for the 1000 data points')
lines(fake.dataset.outlier$X,predict(outlier.model), lwd = 1,col = "red")

summary(outlier.model)










############-------------Question 2----------------------------------------------------------------------------------------


library(arm) 
library(Matching)
data(lalonde)


control.group = lalonde[which(lalonde$treat==0),] #data to use the control group only


lm.model = lm(re78~age+ educ+ re74+re75+ I(educ*re74)+ I(educ*re75)+ I(age*re74)+ I(age*re75)+ I(age*age)+  I(re74*re75),data = control.group)

sim.model <- sim(lm.model, 10000) # doing simulation to get 10,000 coefficients


# indexes for predictors will be in this order --> c(age , educ , re74 , re75 )
summary(lm.model)

 
get.expeced.value = function(coefs, units){ # function to do  the prediction (expected) for each unit in the data
  value = coefs[1] + units[1]*coefs[2] +
    units[2]*coefs[3] +
    units[3]*coefs[4] + 
    units[4]*coefs[5] +
    units[2]*units[3]*coefs[6]+
    units[2]*units[4]*coefs[7]+
    units[1]*units[3]*coefs[8]+
    units[1]*units[4]*coefs[9]+
    units[1]*units[1]*coefs[10]+
    units[3]*units[4]*coefs[11]
  
  return (value)
}


expected.matrix.median <- matrix(NA, nrow = 10000, ncol = 39) 

for (age in c(17:55)){
  for (i in 1:10000)
  {
    units=c(age, median(control.group$educ) , median(control.group$re74), median(control.group$re75))
    expected.matrix.median[i, age-16] = get.expeced.value(sim.model@coef[i,], units)
    
  }
}

intervals.expected.median <- apply(expected.matrix.median, 2, quantile, probs = c(0.025, 0.975))



expected.matrix.quanitle <- matrix(NA, nrow = 10000, ncol = 39)

for (age in c(17:55)){
  for (i in 1:10000)
  {
    units=c(age, quantile(control.group$educ)[4] , quantile(control.group$re74)[4], quantile(control.group$re75)[4])
    expected.matrix.quanitle[i, age-16] = get.expeced.value(sim.model@coef[i,], units)
    
  }
}

intervals.expected.quantile <- apply(expected.matrix.quanitle, 2, quantile, probs = c(0.025, 0.975))




get.predicted.value = function(coefs,Sigma.value, units) {  
  value_2 = coefs[1] + units[1]*coefs[2] +
    units[2]*coefs[3] +
    units[3]*coefs[4] + 
    units[4]*coefs[5] +
    units[2]*units[3]*coefs[6]+
    units[2]*units[4]*coefs[7]+
    units[1]*units[3]*coefs[8]+
    units[1]*units[4]*coefs[9]+
    units[1]*units[1]*coefs[10]+
    units[3]*units[4]*coefs[11]+
    rnorm(1, 0, Sigma.value)
  
  return (value_2)
}



predicted.matrix.median <- matrix(NA, nrow = 10000, ncol = 39)

for (age in c(17:55)){
  for (i in 1:10000)
  {
    units=c(age, median(control.group$educ) , median(control.group$re74), median(control.group$re75))
    predicted.matrix.median[i, age-16] = get.predicted.value(sim.model@coef[i,], sim.model@sigma[i],units)
    
  }
}

intervals.predicted.median <- apply(predicted.matrix.median, 2, quantile, probs = c(0.025, 0.975))






predicted.matrix.quantile <- matrix(NA, nrow = 10000, ncol = 39)

for (age in c(17:55)){
  for (i in 1:10000)
  {
    units=c(age, quantile(control.group$educ)[4] , quantile(control.group$re74)[4], quantile(control.group$re75)[4])
    predicted.matrix.quantile[i, age-16] = get.predicted.value(sim.model@coef[i,], sim.model@sigma[i],units)
    
  }
}

intervals.predicted.quantile <- apply(predicted.matrix.quantile, 2, quantile, probs = c(0.025, 0.975))


# in order to generate a nice looking table, I will do the following:

age = c() #first,adding all the ages in a vector
for (i in 17:55){
  age = c(age, i)
  
}


convert = function(interval){  #Now adding the two bounds of each confedince intervals into one vector so that I can 
  vector = c()                  # put it in one row in the table
  for (i in 1:39){
    a =interval[1, i] # lower bound
    b = interval[2, i] # uppere bound
    add = toString(c(a, b))
    vector = c(vector, add)
    
  }  
  return (vector)
    
  
}



Interval.ExpectedValue.median = convert(intervals.expected.median) # applying the function convert to each interval
Interval.ExpectedValue.quantile = convert(intervals.expected.quantile)
Interval.PredictedValue.median = convert(intervals.predicted.median)
Interval.PredictedValue.quantile = convert(intervals.predicted.quantile)



#now adding the age and the other vectors that containt the intervals in a data frame
table = data.frame(age, Interval.ExpectedValue.median, Interval.ExpectedValue.quantile, Interval.PredictedValue.median,  Interval.PredictedValue.quantile)

#using the DT library to generate a nice looking table
library(DT)
datatable(table)



write.csv(table,'intervals_table.csv') # Also writing the table into CSV format 

# Now plotting time!!

plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(0,15000), 
     main = "Expected Values for re78 VS Age", xlab = "Age",
     ylab = "Expected re78")
                                    
                                    


legend("topright", legend=c("Predictors at median", "Predictors at 75% quantile"),
       col=c("red", "blue"), lty=c(1,1))


for (age in 17:55) {
  segments(
    x0 = age,
    y0 = intervals.expected.median[1, age - 16],
    x1 = age,
    y1 = intervals.expected.median[2, age - 16],
    lwd = 2, col='red', lty = 2)
}


for (age in 17:55) {
  segments(
    x0 = age,
    y0 = intervals.expected.quantile[1, age - 16],
    x1 = age,
    y1 = intervals.expected.quantile[2, age - 16],
    lwd = 2)
}



plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(-10000,23000), 
     main = "Predicted Values for re78 VS Age", xlab = "Age", 
     ylab = "Predicted re78")


legend("topright", legend=c("Predictors at median", "Predictors at 75% quantile"),
       col=c("red", "blue"), lty=c(1,1))

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = intervals.predicted.median[1, age - 16],
    x1 = age,
    y1 = intervals.predicted.median[2, age - 16],
    lwd = 2, col='red',lty=2)
}

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = intervals.predicted.quantile[1, age - 16],
    x1 = age,
    y1 = intervals.predicted.quantile[2, age - 16],
    lwd = 2)
}

text(35, 20000, "Other Predictors at Median")
text(35, 110000, "Other Predictors at 75% Quantile")






##########------------------Question 3----------------------------------------------------------------------------------------


data(PlantGrowth)
newdata = PlantGrowth[-which(PlantGrowth$group== 'trt2'),] #getting rid of trt2

indicator=ifelse(newdata$group =='trt1',1,0) #turning trt1 to 1 and ctr to zero
newdata = data.frame(newdata, indicator)


newdata
model = lm(weight ~ indicator, newdata)

summary(model)
confint(model, model$coef[2], 0.95) #analytical conf interval

model



results <- rep(0, 10000)
for (i in 1:10000){
  
  sample1 <- sample(c(1:length(newdata$group)), length(newdata$group), replace = T)
  
  results[i] <- summary(lm(weight~indicator, data = newdata[sample1,]))$coef[2]
  }

conf_interv = quantile(results, probs = c(0.025, 0.975)) #bootstrap conf interval

hist(results, xlab='Bootstrap-sample results', main='Histogram of Bootstrap-sample results')

conf_interv




######----------------------Question 4--------------------------------------------------------------------------------




R.Squared = function(Y.observed, Y.predicted){
  SSE = 0
  SST = 0
  for (i in 1:20){
    SSE = SSE + (Y.observed[i] -Y.predicted[i])^2
    SST = SST + (Y.observed[i] - mean(Y.observed))^2

  }
  return (1 - (SSE/SST))
}


Y.observed = newdata$weight
Y.predicted = predict(model)

table2 = data.frame(Y.observed, Y.predicted)


datatable(table2)

R.Squared(Y.observed, Y.predicted)

summary(model)









######---------------Question 5---------------------------------------------------------------------------------------


library(foreign)
nsw.data = read.dta('D:/Courses/CS112/nsw.dta')

Logit.model = glm(treat~ age +education +black +hispanic +married +nodegree +re75, data = nsw.data, family = binomial )


treatment.Prob = predict(Logit.model, type ='response') 

control.prob = 1- predict(Logit.model, type ='response')

hist(treatment.Prob, col = 'red', main='Histogram of the treatment group's estimated probabilities', xlab = 'Probability')
hist(control.prob, col='blue', main='Histogram of the control group's estimated probabilities', xlab = 'Probability')


