
foo <- read.csv("https://tinyurl.com/yb4phxx8") # read in the data

date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25)

for(i in date.columns)  {
  
  which_values_are_missing <- which(as.character(foo[, i]) == "")
  

  foo[which_values_are_missing, i] <- NA
  
 
  foo[, i] <- as.Date(as.character(foo[, i]))

  }

which.have.NAs <- which(is.na(foo$CirculationDate == TRUE)) #removing NAs from CirculationDate

new_foo <- foo[-which.have.NAs, ]

which.below.data <- which(new_foo$CirculationDate <=as.Date('2008-01-01') ) 

new_foo <- new_foo[-which.below.data, ]

which.have.NAs <- which(is.na(new_foo$OriginalCompletionDate == TRUE)) #removing NAs from OriginalCompletionDate

foo_1 <- new_foo[-which.have.NAs, ]

planned_duration <- as.numeric(difftime(foo_1$OriginalCompletionDate, foo_1$ApprovalDate), units="days")
 #project duration at approval

mean(planned_duration/30)

median(planned_duration/30)

quantile(planned_duration/30)

Actual_duration <- as.numeric(difftime(foo_1$RevisedCompletionDate, foo_1$ApprovalDate), units="days")

difference = Actual_duration - planned_duration #difference between the acutal duration and the planned duration

mean(difference/30)

median(difference/30)

quantile(difference/30)

which.have.NAs <- which(is.na(new_foo$Rating == TRUE)) #removing NAs from Rating only

foo_2 <- new_foo[-which.have.NAs, ]

rate_0 = which(foo_2$Rating ==0 ) 

rate_1 = which(foo_2$Rating ==1 ) 

rate_2 = which(foo_2$Rating ==2 ) 

rate_3 = which(foo_2$Rating ==3 ) 

P_0 = (length(rate_0) / nrow(foo_2))*100


P_1 = (length(rate_1) / nrow(foo_2))*100

P_2 = (length(rate_2) / nrow(foo_2))*100

P_3 = (length(rate_3) / nrow(foo_2))*100

x <- c(P_0,P_1,P_2,P_3)
labels <- c(paste("Rating 0= ",round(P_0), '%'), paste('Rating 1=',round(P_1), '%'), paste('Rating 2=',round(P_2), '%'), paste('Rating 3=',round(P_3), '%'))

a  <- c(1,2,3,4)

pie(x,labels, main="Percentages of the Ratings of the projects") 


which.PPTA <- which(foo_2$Type == 'PPTA') #Excluding all PPTAs projects

foo_3 <- foo_2[-which.PPTA, ]

rate_0 = which(foo_3$Rating ==0 ) 

rate_1 = which(foo_3$Rating ==1 ) 

rate_2 = which(foo_3$Rating ==2 ) 

rate_3 = which(foo_3$Rating ==3 ) 

P_0 = (length(rate_0) / nrow(foo_3))*100

P_1 = (length(rate_1) / nrow(foo_3))*100

P_2 = (length(rate_2) / nrow(foo_3))*100

P_3 = (length(rate_3) / nrow(foo_3))*100

x <- c(P_0,P_1,P_2,P_3)
labels <- c(paste("Rating 0= ",round(P_0), '%'), paste('Rating 1=',round(P_1), '%'), paste('Rating 2=',round(P_2), '%'), paste('Rating 3=',round(P_3), '%'))

a  <- c(1,2,3,4)

pie(x,labels) 


which.have.NAs <- which(is.na(new_foo$Rating == TRUE)) #removing NAs from Rating
foo_4 <- new_foo[-which.have.NAs, ]

Top_25 = head(foo_4[order(foo_4$RevisedAmount,decreasing=T),],.25*nrow(foo_4)) #Top 25% by RevisedAmount

Bottom_25 =  head(foo_4[order(foo_4$RevisedAmount,decreasing=F),],.25*nrow(foo_4)) #Bottom 25% by RevisedAmount

mean(Top_25$Rating)

mean(Bottom_25$Rating)

which.PAK <- which(Top_25$Country == "PAK" ) #the top 25% by RevisedAmount from PAkistan
Top_25_PAK <- Top_25[which.PAK,]

Bottom_25_PAK <- Bottom_25[which.PAK,]#the top 25% by RevisedAmount from PAkistan


mean(Top_25_PAK$Rating)

mean(Bottom_25_PAK$Rating)

which.VIE <- which(Top_25$Country == "VIE" ) #the top 25% by RevisedAmount from Vietnam
Top_25_VIE <- Top_25[which.VIE,]

Bottom_25_VIE <- Bottom_25[which.VIE,]#the top 25% by RevisedAmount from Vietnam


mean(Top_25_VIE$Rating)

mean(Bottom_25_VIE$Rating)
