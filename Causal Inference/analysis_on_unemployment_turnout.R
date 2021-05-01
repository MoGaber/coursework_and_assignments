### Loading Data
require(readstata13)
require(foreign)
require(multiwayvcov)
require(mlogit)
require(xtable)
require(stargazer)
require(ggplot2)
require(car)
require(mgcv)
require(reshape2)
require(nnet)
library('Matching')

#Load in replication data from B&W
county.data <- read.dta13("BW JOP county replication data.dta")
indiv.data <- read.dta("BW JOP individual replication data.dta")
state.data <- read.dta("BW JOP state replication data.dta")


###------- REplication table 4

#No Year Fixed Effects
#TABLE 4

state.data.t4 <- state.data[ which(state.data$presyear==0 
                                   & state.data$year >= '1980'
                                   & state.data$GubElection == 1), ]

#The regression they ran, without year fixed effects
table4.1 <- lm(vep ~ uerate + incparty + uerate*incparty + s_black + college + SenElection
               + factor(fips_state), data=state.data.t4)
summary(table4.1)


### With year fixed effects

#Re-calculate Table 4 regression with year fixed effects
table4 <- lm(vep ~ uerate + incparty + uerate*incparty + s_black + college + SenElection
             + factor(fips_state) + factor(year), data=state.data.t4)
summary(table4)
### Takeaways
#They did not control for Year even though they stated that they did
#Once you control for Year, every coefficient goes to 0 (minimal effect). 

######### ----- Extension: Gen Match
# get quantiles on enemployment
ue_q = quantile(state.data.t4$uerate,c(.25,.75))

# make new dataset w control  beng <= 25% unemployment control 
# and treat being >= 75% unemployment
state.data.t4.match = state.data.t4[state.data.t4$uerate <= ue_q[1] | state.data.t4$uerate >= ue_q[2],]
state.data.t4.match$treat = ifelse(state.data.t4.match$uerate >= ue_q[2], 1, 0)

# remove NAs
state.data.t4.match <- state.data.t4.match[complete.cases(state.data.t4.match), ]
which(is.na(state.data.t4.match) == TRUE)

# Gen Match
library('Matching')
library('rgenoud')

Tr = state.data.t4.match$treat

X = cbind(state.data.t4.match$incparty, 
          state.data.t4.match$s_black, state.data.t4.match$college, 
          state.data.t4.match$SenElection, factor(state.data.t4.match$fips_state),
          factor(state.data.t4.match$year))


# X = cbind(state.data.t4.match$year,state.data.t4.match$fips_state,
#       state.data.t4.match$s_black, state.data.t4.match$SenElection,
#       state.data.t4.match$presyear, state.data.t4.match$college,
#       state.data.t4.match$college_diff, state.data.t4.match$pci_0000,
#       state.data.t4.match$prior_ue, state.data.t4.match$partyspending,
#       state.data.t4.match$priorue_diff, state.data.t4.match$spending_diff,
#       state.data.t4.match$share_open, state.data.t4.match$share_qual_out2,
#       state.data.t4.match$open_all_share, state.data.t4.match$quality_share_new,
#       state.data.t4.match$outparty_spend, state.data.t4.match$share_open_diff,
#       state.data.t4.match$open_all_share_diff, state.data.t4.match$share_qual_out2_diff,
#       state.data.t4.match$quality_share_diff, state.data.t4.match$outparty_spend_diff,
#       state.data.t4.match$s_black_diff, state.data.t4.match$pci0000_diff,
#       state.data.t4.match$senelec_diff, state.data.t4.match$incparty,
#       state.data.t4.match$GubElection, state.data.t4.match$vep,
#       state.data.t4.match$uerate)

genout1 = GenMatch(Tr = Tr, X = X,pop.size=200,max.generations=25)
mout1 = Match(Tr = Tr, X = X, Weight.matrix=genout1)
summary(mout1)


mb1  <- MatchBalance(Tr ~  incparty + s_black + 
                       college + SenElection + factor(fips_state) + factor(year), 
                     data = state.data.t4.match, match.out = mout1, nboots=500)



# checking effect

mout1y = Match(Tr = Tr, X = X, Y= state.data.t4.match$vep, Weight.matrix=genout1)
summary(mout1y)

mout1y

#Trying to improve Gen Match
genout2 = GenMatch(Tr = Tr, X = X,pop.size=200,max.generations=25,caliper=1)
mout2 = Match(Tr = Tr, X = X, Weight.matrix=genout2,caliper=1)
summary(mout2)


mb2  <- MatchBalance(Tr ~  incparty + s_black + 
                       college + SenElection + factor(fips_state) + factor(year), 
                     data = state.data.t4.match, match.out = mout2, nboots=500)

mout2y = Match(Tr = Tr, X = X, Y = state.data.t4.match$vep, Weight.matrix=genout2,caliper=1)
summary(mout2y)


**Dropped too many**
  
#While we got the exact same coefficient on state unemployment rate, we've dropped so many variables that it is no longer generalizable to the whole population 
  #(left with 6 treatment obs, dropped 11).

#Since match balance can only be achieved when dropping a significant ammount of variables, we can conclude that the treatment and observation groups are 
  #statistically significantly different and hence there is already inherent selection bias in the observations that is not controlled for. The incumbent party, college, 
  #and other factors significantly affect unemployment rate in a way that cannot be controlled for given the data we have. Thus, to isolate the effect of unemployment
  
  
#########-------- Gen Match for effect of Republican Incumbent
  
# Gen Match
library('Matching')
library('rgenoud')

Trr = ifelse(state.data.t4.match$incparty =='R', 1, 0)

Xr = cbind(state.daata.t4.match$uerate, 
           state.data.t4.match$s_black, state.data.t4.match$college, 
           state.data.t4.match$SenElection, factor(state.data.t4.match$fips_state),
           factor(state.data.t4.match$year))


# X = cbind(state.data.t4.match$year,state.data.t4.match$fips_state,
#       state.data.t4.match$s_black, state.data.t4.match$SenElection,
#       state.data.t4.match$presyear, state.data.t4.match$college,
#       state.data.t4.match$college_diff, state.data.t4.match$pci_0000,
#       state.data.t4.match$prior_ue, state.data.t4.match$partyspending,
#       state.data.t4.match$priorue_diff, state.data.t4.match$spending_diff,
#       state.data.t4.match$share_open, state.data.t4.match$share_qual_out2,
#       state.data.t4.match$open_all_share, state.data.t4.match$quality_share_new,
#       state.data.t4.match$outparty_spend, state.data.t4.match$share_open_diff,
#       state.data.t4.match$open_all_share_diff, state.data.t4.match$share_qual_out2_diff,
#       state.data.t4.match$quality_share_diff, state.data.t4.match$outparty_spend_diff,
#       state.data.t4.match$s_black_diff, state.data.t4.match$pci0000_diff,
#       state.data.t4.match$senelec_diff, state.data.t4.match$incparty,
#       state.data.t4.match$GubElection, state.data.t4.match$vep,
#       state.data.t4.match$uerate)

genoutr1 = GenMatch(Tr = Trr, X = Xr,pop.size=200,max.generations=25)
moutr1 = Match(Tr = Trr, X = Xr, Weight.matrix=genoutr1)
summary(mout1r)



mb1r  <- MatchBalance(Trr ~  uerate + s_black + 
                        college + SenElection + factor(fips_state) + factor(year), 
                      data = state.data.t4.match, match.out = moutr1, nboots=500)

# checking effect

mout1ry = Match(Tr = Trr, X = Xr, Y= state.data.t4.match$vep, Weight.matrix=genoutr1)
summary(mout1y)

# Trying to improve GM on Republican

# Gen Match


Trr = ifelse(state.data.t4.match$incparty =='R', 1, 0)

Xr = cbind(state.data.t4.match$uerate, 
           state.data.t4.match$s_black, state.data.t4.match$college, 
           state.data.t4.match$SenElection, factor(state.data.t4.match$fips_state),
           factor(state.data.t4.match$year))


# X = cbind(state.data.t4.match$year,state.data.t4.match$fips_state,
#       state.data.t4.match$s_black, state.data.t4.match$SenElection,
#       state.data.t4.match$presyear, state.data.t4.match$college,
#       state.data.t4.match$college_diff, state.data.t4.match$pci_0000,
#       state.data.t4.match$prior_ue, state.data.t4.match$partyspending,
#       state.data.t4.match$priorue_diff, state.data.t4.match$spending_diff,
#       state.data.t4.match$share_open, state.data.t4.match$share_qual_out2,
#       state.data.t4.match$open_all_share, state.data.t4.match$quality_share_new,
#       state.data.t4.match$outparty_spend, state.data.t4.match$share_open_diff,
#       state.data.t4.match$open_all_share_diff, state.data.t4.match$share_qual_out2_diff,
#       state.data.t4.match$quality_share_diff, state.data.t4.match$outparty_spend_diff,
#       state.data.t4.match$s_black_diff, state.data.t4.match$pci0000_diff,
#       state.data.t4.match$senelec_diff, state.data.t4.match$incparty,
#       state.data.t4.match$GubElection, state.data.t4.match$vep,
#       state.data.t4.match$uerate)

genoutr2 = GenMatch(Tr = Trr, X = Xr,pop.size=200,max.generations=25, caliper = 1)
moutr2 = Match(Tr = Trr, X = Xr, Weight.matrix=genoutr2, caliper = 1)
summary(moutr2)


mb2r  <- MatchBalance(Trr ~  uerate + s_black + 
                        college + SenElection + factor(fips_state) + factor(year), 
                      data = state.data.t4.match, match.out = moutr2, nboots=500)


# checking effect

moutr2y = Match(Tr = Trr, X = Xr, Y= state.data.t4.match$vep, Weight.matrix=genoutr2)
summary(mout1y)



# Sensitivity Analysis on Incumbent Party

library('rbounds') 

psens(mout1ry, Gamma = 1.5, GammaInc = 0.05)


# GenMatch Rep Inc x Unemployment

# Gen Match
library('Matching')
library('rgenoud')

TrRe = ifelse(state.data.t4.match$incparty =='R', 1, 0)*state.data.t4.match$treat

XRe = cbind(state.data.t4.match$s_black, state.data.t4.match$college, 
            state.data.t4.match$SenElection, factor(state.data.t4.match$fips_state),
            factor(state.data.t4.match$year))


# X = cbind(state.data.t4.match$year,state.data.t4.match$fips_state,
#       state.data.t4.match$s_black, state.data.t4.match$SenElection,
#       state.data.t4.match$presyear, state.data.t4.match$college,
#       state.data.t4.match$college_diff, state.data.t4.match$pci_0000,
#       state.data.t4.match$prior_ue, state.data.t4.match$partyspending,
#       state.data.t4.match$priorue_diff, state.data.t4.match$spending_diff,
#       state.data.t4.match$share_open, state.data.t4.match$share_qual_out2,
#       state.data.t4.match$open_all_share, state.data.t4.match$quality_share_new,
#       state.data.t4.match$outparty_spend, state.data.t4.match$share_open_diff,
#       state.data.t4.match$open_all_share_diff, state.data.t4.match$share_qual_out2_diff,
#       state.data.t4.match$quality_share_diff, state.data.t4.match$outparty_spend_diff,
#       state.data.t4.match$s_black_diff, state.data.t4.match$pci0000_diff,
#       state.data.t4.match$senelec_diff, state.data.t4.match$incparty,
#       state.data.t4.match$GubElection, state.data.t4.match$vep,
#       state.data.t4.match$uerate)

genoutRe = GenMatch(Tr = TrRe, X = XRe,pop.size=200,max.generations=25)
moutRe = Match(Tr = TrRe, X = XRe, Weight.matrix=genoutRe)
summary(moutRe)




