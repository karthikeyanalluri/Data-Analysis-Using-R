options("scipen"=99999, digits=3)

library(tsDyn)
library(vars)
library(quantmod)
library(tseries)
library(urca)
library(dynlm)

df <- read.csv("trial.csv", header = TRUE)
dfts = ts(df[2:9],  start = c(1995, 1), frequency = 4)
plot.ts(dfts)

# define variables

lfeddebt0 <- log(dfts[,1])
unemp0 <- dfts[,2]
limports0 <- log(dfts[,3])
lexports0 <- log(dfts[,4])
lfdi0 <- log(dfts[,5])
lgdp0 <- log(dfts[,6])
cpi0 <- dfts[,7]
corruption0 <-dfts[,8]

summary(dfts)

######### checking stationarity with intord######################
source(file="intord.R")
par(mar = c(3, 3, 3, 3))
intord(lfeddebt0) #I(1)
length(lfeddebt0)
print(lfeddebt0)


intord(unemp0) #I(1)
length(unemp0)
print(unemp0)


intord(limports0) #I(1) # seasonality
length(limports0) #112
print(limports0)
dlimports0 <- diff(limports0,4)
length(dlimports0)
print(dlimports0)
intord(dlimports0)

intord(lexports0) # I(2)
length(lexports0)
print(lexports0)
dlexports0 <- diff(lexports0,4)
intord(dlexports0) # I(1)
length(dlexports0)
print(dlexports0)

intord(lfdi0) # I(1)
length(lfdi0)
print(lfdi0)

intord(lgdp0) #I(2) # seasonality
length(lgdp0)
print(lgdp0)
dlgdp0 <- diff(lgdp0,4) 
intord(dlgdp0) # I(1)
length(dlgdp0)
print(dlgdp0)

intord(cpi0) #I(1) 
length(cpi0)
print(cpi0)


#intord(corruption0) #I(0) # seasonality
intord(corruption0) # I(1)
length(corruption0)
print(corruption0)

summary(dfts)



# do not have the same number of observations

lfeddebt = ts(lfeddebt0[5:112], start = c(1996,1), frequency = 4)
print(lfeddebt)
length(lfeddebt)

unemp = ts(unemp0[5:112], start = c(1996,1), frequency = 4)
print(unemp)
length(unemp)

dlimports = ts(dlimports0[1:108], start = c(1996,1), frequency = 4)
print(dlimports)
length(dlimports)

dlexports = ts(dlexports0[1:108], start = c(1996,1), frequency = 4)
print(dlexports)
length(dlexports)



lfdi = ts(lfdi0[5:112], start = c(1996,1), frequency = 4)
print(lfdi)
length(lfdi)

dlgdp = ts(dlgdp0[1:108], start = c(1996,1), frequency = 4)
print(dlgdp)
length(dlgdp)

cpi = ts(cpi0[5:112], start = c(1996,1), frequency = 4)
print(cpi)
length(cpi)

#corruption = ts(corruption0[6:112], start = c(1996,2),frequency = 4)
corruption = ts(corruption0[5:112], start = c(1996,1),frequency = 4)
print(corruption)
length(corruption)

################ differencing #######################

dlfeddebt <- diff(lfeddebt) 
intord(dlfeddebt)
length(dlfeddebt) #107
print(dlfeddebt)

dunemp <- diff(unemp) 
intord(dunemp)
length(dunemp) #107
print(dunemp)

ddlimports <-diff(dlimports)
intord(ddlimports)
length(ddlimports)
print(ddlimports)


#dlimports <- diff(limports,4)
#intord(dlimports)
#length(dlimports) #108
#print(dlimports)

#dlimports0 <- diff(limports0,lag=4)
#intord(dlimports0)

#dlimports0 <- diff(limports0) 
#intord(dlimports0)


ddlexports <- diff(dlexports)
intord(ddlexports)
length(ddlexports) #107
print(ddlexports)

dlfdi <- diff(lfdi) 
intord(dlfdi)
length(dlfdi) #107
print(dlfdi)

ddlgdp <- diff(dlgdp)
intord(ddlgdp)
length(ddlgdp) #107
print(ddlgdp)

dcpi <- diff(cpi) 
intord(dcpi)
length(dcpi) #107
print(dcpi)

dcorruption <- diff(corruption)
intord(dcorruption)
length(dcorruption) #107
print(dcorruption)

#dcorruption0 <- diff(corruption0,)
#intord(dcorruption0)

#ddcorruption0 <- diff(dcorruption0)
#intord(ddcorruption0)



#lengths of variables at I(1)
length(lfeddebt)     #108
length(unemp)        #108
length(dlimports)    #108
length(dlexports)    #108
length(lfdi)         #108
length(dlgdp)        #108
length(cpi)          #108
length(corruption)   #108

# lengths of variables at I(0) # run these after changing the observations to same length
length(dlfeddebt)     #107
length(dunemp)        #107
length(ddlimports)    #107
length(ddlexports)    #107
length(dlfdi)         #107
length(ddlgdp)        #107
length(dcpi)          #107
length(dcorruption)   #107


####at I(1)
# lfeddebt, unemp, dlimports, dlexports, lfdi, dlgdp, cpi
#### At I(0)
# dlfeddebt, dunemp, ddlimports, ddlexports, dlfdi, ddlgdp, dcpi, corruption0

#y <- cbind(lfeddebt, unemp, dlimports, dlexports, lfdi, dlgdp, cpi)
#dy <- cbind(dlfeddebt, dunemp, ddlimports, ddlexports, dlfdi, ddlgdp, dcpi, corruption) # corruption0

y <- cbind(lfeddebt, unemp, dlimports, dlexports, lfdi, dlgdp, cpi, corruption)
dy <- cbind(dlfeddebt, dunemp, ddlimports, ddlexports, dlfdi, ddlgdp, dcpi, dcorruption) # corruption0

#checking for number of lags
VARselect(y, lag.max=8, type="const") # here SC(n) = 1. so use at least 3
#VARselect(y, lag.max=12, type="const") # here SC(n) = 1. so use at least 3



print(y)
print(dy)

nlags=2

############################# co-integration test#####################################
##############Johansen Procedure###################

# For this variables must be at level  #
jc <- ca.jo(y, type="eigen", ecdet="const",K=nlags) # i have two coint relation
summary(jc) # test should be more than 1pct . here nothing is significant

jct <- ca.jo(y, type="trace", ecdet="const",K=nlags) 
summary(jct) # here 2 are significant - r<= 1,2


# eigenvalues, etc., can be taken from the output of ca.jo
eigenvals <- jct@lambda  ###If Jc is significant we shall use Jc and jc everywhere below. If jct is only significant we shall use only jct here and below

# cointegrating relationships
cointv <- jct@V
cointj <- cointv[,2] ## considered lags = 2 here
yym <- as.matrix(y)
ecmj <- yym%*%cointj[1:8] + cointj[9] ### here 1:3 where 3 is the maximum number of variables I have in the model and at cointj[4] we should add 1 and  write 4
#ecmj <- yym%*%cointj[1:7] + cointj[8] ### here 1:3 where 3 is the maximum number of variables I have in the model and at cointj[4] we should add 1 and  write 4
intord(ecmj)

##fail to reject @1% but rejecting @5%  It means they are cointegrating at 5% level.



############## Engle-Granger#######################
#lfeddebt, unemp, dlimports, dlexports, lfdi, dlgdp, cpi


#c1 <- lm(lfeddebt ~ unemp+ dlimports + dlexports +lfdi+ dlgdp + cpi) 
#c2 <- lm(unemp ~ dlimports + dlexports +lfdi+ dlgdp + cpi+lfeddebt)
#c3 <- lm(dlimports ~ dlexports +lfdi+ dlgdp + cpi+lfeddebt+unemp)
#c4 <- lm(dlexports ~ lfdi+ dlgdp + cpi+lfeddebt+unemp+dlimports)
#c5 <- lm(lfdi~ dlgdp + cpi+lfeddebt+unemp+dlimports+dlexports)
#c6 <- lm(dlgdp ~ cpi+lfeddebt+unemp+dlimports+dlexports+lfdi)
#c7 <- lm(cpi~lfeddebt+unemp+dlimports+dlexports+lfdi+dlgdp)

c1 <- lm(lfeddebt ~ unemp+ dlimports + dlexports +lfdi+ dlgdp + cpi + corruption) 
c2 <- lm(unemp ~ dlimports + dlexports +lfdi+ dlgdp + cpi+lfeddebt + corruption)
c3 <- lm(dlimports ~ dlexports +lfdi+ dlgdp + cpi+lfeddebt+unemp+ corruption)
c4 <- lm(dlexports ~ lfdi+ dlgdp + cpi+lfeddebt+unemp+dlimports+ corruption)
c5 <- lm(lfdi~ dlgdp + cpi+lfeddebt+unemp+dlimports+dlexports+ corruption)
c6 <- lm(dlgdp ~ cpi+lfeddebt+unemp+dlimports+dlexports+lfdi+ corruption)
c7 <- lm(cpi~lfeddebt+unemp+dlimports+dlexports+lfdi+dlgdp+ corruption)
c8 <- lm(corruption ~ unemp+ dlimports + dlexports +lfdi+ dlgdp + cpi + lfeddebt)

summary(c1)
ecm1 <- c1$residuals
#dev.off()
par(mar = c(3, 3, 3, 3))
intord(ecm1)


summary(c2)
ecm2 <- c2$residuals
#dev.off()                   
par(mar = c(3, 3, 3, 3))
intord(ecm2)                        #stationary

summary(c3)
ecm3 <- c3$residuals                       
#dev.off()
par(mar = c(3, 3, 3, 3))
intord(ecm3)

summary(c4)
ecm4 <- c4$residuals                        
#dev.off()
par(mar = c(3, 3, 3, 3))
intord(ecm4)                         #stationary

summary(c5)
ecm5 <- c5$residuals
#dev.off()
par(mar = c(3, 3, 3, 3))
intord(ecm5)

summary(c6)
ecm6 <- c6$residuals                        
#dev.off()
par(mar = c(3, 3, 3, 3))
intord(ecm6)                          #stationary

summary(c7)
ecm7 <- c7$residuals
#dev.off()
par(mar = c(3, 3, 3, 3))
intord(ecm7)

summary(c8)
ecm8 <- c8$residuals
#dev.off()
par(mar = c(3, 3, 3, 3))
intord(ecm8)

# hence from model 2,4,6 shows that they are stationary and the sd doesnt fall less than half, hence there is cointigration. hence we can proceed with VECM


##########################Estimating VECM###############################################################################################
##Buidling ecmj and ecm including one lag on both ecmj or ecm in the VAR as an exog variable 

# dlfeddebt, dunemp, ddlimports, ddlexports, dlfdi, ddlgdp, dcpi,

par(mfrow=c(1,2))
plot(ecm1,type='l')
plot(ecmj,type='l')

#create a lag for ECM term,
ec <- embed(ecm2,2)##for engel granger -- here 2 is the first lag
ecm9 <- ec[,2]
#ecm8 <- ec[,2]
ecj <- embed(ecmj,2) ## for johansen --- here 2 is the first lag
ecmj1 <- ecj[,2]

library(vars)

# First a VAR(1): p = 2 based on BIC
#var1 <- VAR(dy, p=9, type="cons", season = 4) # type ="cons","trend","both"
var1 <- VAR(dy, p=9, type="cons") # type ="cons","trend","both"
summary(var1)
dev.off()
par(mar = c(3, 3, 3, 3))
plot(var1, names = "dlfeddebt")
plot(var1, names = "dunemp")
plot(var1, names = "ddlimports")
plot(var1, names = "ddlexports")
plot(var1, names = "dlfdi")
plot(var1, names = "ddlgdp")
plot(var1, names = "dcpi")
plot(var1, names = "dcorruption")
AIC(var1)
BIC(var1)
logLik(var1)

# VECM with Johansen
var2 <- VAR(dy, p=9, type="cons",exogen=ecmj1,season=4)
summary(var2)
AIC(var2)
BIC(var2)
logLik(var2)

# VECM with EG
var3 <- VAR(dy, p=9, type="cons",exogen=ecm9,season=4)
#var3 <- VAR(dy, p=2, type="cons",exogen=ecm8,season=4)
summary(var3)
AIC(var3)
BIC(var3)
logLik(var3)

###### plots of fit results ########
#dev.off()
par(mar = c(3, 3, 3, 3))
plot(var2, names = "dlfeddebt")
plot(var2, names = "dunemp")
plot(var2, names = "ddlimports")
plot(var2, names = "ddlexports")
plot(var2, names = "dlfdi")
plot(var2, names = "ddlgdp")
plot(var2, names = "dcpi")
plot(var2, names = "dcorruption")

plot(var3, names = "dlfeddebt")
plot(var3, names = "dunemp")
plot(var3, names = "ddlimports")
plot(var3, names = "ddlexports")
plot(var3, names = "dlfdi")
plot(var3, names = "ddlgdp")
plot(var3, names = "dcpi")
plot(var3, names = "dcorruption")
############# Granger causality ####################################################################

#if p-value>0.05 - we fail to reject null hypothesis

# Johansen VECM procedure - unrestricted

#y <- cbind(lfeddebt, unemp, dlimports, dlexports, lfdi, dlgdp, cpi)
#y <- cbind(lfeddebt, unemp, dlimports, dlexports, lfdi, dlgdp, cpi, corruption)

####### Does lfeddebt granger cause lfdi? 
vecm1 <- ca.jo(y, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=1) 

# restricted
lyr <- cbind(lfeddebt,lfdi)    #  drop (lmspus) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1)  

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 





####### Does unemp granger cause lfdi? 
vecm1 <- ca.jo(y, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=1)  

# restricted
lyr <- cbind(unemp,lfdi)    #  drop (lmspus) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1)  

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 




####### Does dlimports granger cause lfdi? 
vecm1 <- ca.jo(y, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=1) 

# restricted
lyr <- cbind(dlimports,lfdi)   
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1) 

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 




####### Does dlexports granger cause lfdi? 
vecm1 <- ca.jo(y, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=1)  

# restricted
lyr <- cbind(dlexports,lfdi)    #  drop (lmspus) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1)  

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 



####### Does dlgdp granger cause lfdi? 
vecm1 <- ca.jo(y, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=1)  

# restricted
lyr <- cbind(dlgdp,lfdi)    #  drop (lmspus) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1)  

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 



####### Does cpi granger cause lfdi? 
vecm1 <- ca.jo(y, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=1)  

# restricted
lyr <- cbind(cpi,lfdi)    #  drop (lmspus) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1) 

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 


####### Does corruption granger cause lfdi? 
vecm1 <- ca.jo(y, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=1)  

# restricted
lyr <- cbind(corruption,lfdi)    #  drop (lmspus) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1) 

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F")


#y <- cbind(lfeddebt, unemp, dlimports, dlexports, lfdi, dlgdp, cpi, )
#y <- cbind(lfeddebt, unemp, dlimports, dlexports, lfdi, dlgdp, cpi, corruption)


####### Does lfdi granger cause lfeddebt? 
vecm1 <- ca.jo(y, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=1)  

# restricted
lyr <- cbind(lfdi,lfeddebt)    #  drop (lmspus) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1) 

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 



####### Does lfdi granger cause unemp? 
vecm1 <- ca.jo(y, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=1)  

# restricted
lyr <- cbind(lfdi,unemp)    #  drop (lmspus) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1)  

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 





####### Does lfdi granger cause dlimports? 
vecm1 <- ca.jo(y, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=1)  

# restricted
lyr <- cbind(lfdi,dlimports)    #  drop (lmspus) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1) 

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 



####### Does lfdi granger cause dlexports? 
vecm1 <- ca.jo(y, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=1) 

# restricted
lyr <- cbind(lfdi,dlexports)    #  drop (lmspus) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1)  

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 



####### Does lfdi granger cause dlgdp? 
vecm1 <- ca.jo(y, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=1)  

# restricted
lyr <- cbind(lfdi,dlgdp)    #  drop (lmspus) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1)  

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 



####### Does lfdi granger cause cpi? 
vecm1 <- ca.jo(y, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=1)  

# restricted
lyr <- cbind(lfdi,cpi)    #  drop (lmspus) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1)  

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 



####### Does lfdi granger cause corruption? 
vecm1 <- ca.jo(y, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=1)  

# restricted
lyr <- cbind(lfdi,corruption)    #  drop (lmspus) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1)  

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 









###############################################################################################


#################### Impulse Response ######################
#dy <- cbind(dlfeddebt, dunemp, ddlimports, ddlexports, dlfdi, ddlgdp, dcpi)

# variables in var2: dlfeddebt, dunemp, ddlimports, ddlexports, dlfdi, ddlgdp, dcpi

#irfs <- irf(var2, impulse = "dlfeddebt", response = c("dlfdi"), boot = TRUE)
#plot(irfs)

irfs <- irf(var1, impulse = "dlfeddebt", response = c("dlfdi"), boot = TRUE)
plot(irfs)

irfs <- irf(var1, impulse = "dunemp", response = c("dlfdi"), boot = TRUE)
plot(irfs)

irfs <- irf(var1, impulse = "ddlimports", response = c("dlfdi"), boot = TRUE)
plot(irfs)

irfs <- irf(var1, impulse = "ddlexports", response = c("dlfdi"), boot = TRUE)
plot(irfs)

irfs <- irf(var1, impulse = "ddlgdp", response = c("dlfdi"), boot = TRUE)
plot(irfs)

irfs <- irf(var1, impulse = "dcpi", response = c("dlfdi"), boot = TRUE)
plot(irfs)

irfs <- irf(var1, impulse = "dcorruption", response = c("dlfdi"), boot = TRUE)
plot(irfs)



############# variance decomposition ###################
par(mar = c(2, 2, 2, 2))
# Variance Decompositions (contribution of each variable to predicting a variable)
vard <- fevd(var2, n.ahead=4)
vard
vard$dha
plot(vard, col=1:8)

######## test for serial correlation for residuals ##########

#dy <- cbind(dlfeddebt, dunemp, ddlimports, ddlexports, dlfdi, ddlgdp, dcpi)
#dy <- cbind(dlfeddebt, dunemp, ddlimports, ddlexports, dlfdi, ddlgdp, dcpi, dcorruption)

# BoX-Ljung Q Statistic for dlfeddebt
#resi = var2$varresult$dlfeddebt$residuals
resi = var1$varresult$dlfeddebt$residuals
b = Box.test(resi,lag = 20, type="Ljung-Box")
b
blt = rep(0,20)
for (i in 1:20){
  b = Box.test(resi,lag = i, type="Ljung-Box")
  blt[i]=b$p.value
}
blt


# BoX-Ljung Q Statistic for dunemp
resi = var1$varresult$dunemp$residuals
b = Box.test(resi,lag = 20, type="Ljung-Box")
b
blt = rep(0,20)
for (i in 1:20){
  b = Box.test(resi,lag = i, type="Ljung-Box")
  blt[i]=b$p.value
}
blt


# BoX-Ljung Q Statistic for ddlimports
resi = var1$varresult$ddlimports$residuals
b = Box.test(resi,lag = 20, type="Ljung-Box")
b
blt = rep(0,20)
for (i in 1:20){
  b = Box.test(resi,lag = i, type="Ljung-Box")
  blt[i]=b$p.value
}
blt


# BoX-Ljung Q Statistic for ddlexports
resi = var1$varresult$ddlexports$residuals
b = Box.test(resi,lag = 20, type="Ljung-Box")
b
blt = rep(0,20)
for (i in 1:20){
  b = Box.test(resi,lag = i, type="Ljung-Box")
  blt[i]=b$p.value
}
blt


# BoX-Ljung Q Statistic for dlfdi
resi = var1$varresult$dlfdi$residuals
b = Box.test(resi,lag = 20, type="Ljung-Box")
b
blt = rep(0,20)
for (i in 1:20){
  b = Box.test(resi,lag = i, type="Ljung-Box")
  blt[i]=b$p.value
}
blt


# BoX-Ljung Q Statistic for ddlgdp
resi = var1$varresult$ddlgdp$residuals
b = Box.test(resi,lag = 20, type="Ljung-Box")
b
blt = rep(0,20)
for (i in 1:20){
  b = Box.test(resi,lag = i, type="Ljung-Box")
  blt[i]=b$p.value
}
blt


# BoX-Ljung Q Statistic for dcpi
resi = var1$varresult$dcpi$residuals
b = Box.test(resi,lag = 20, type="Ljung-Box")
b
blt = rep(0,20)
for (i in 1:20){
  b = Box.test(resi,lag = i, type="Ljung-Box")
  blt[i]=b$p.value
}
blt



# BoX-Ljung Q Statistic for dcorruption
resi = var1$varresult$dcorruption$residuals
b = Box.test(resi,lag = 20, type="Ljung-Box")
b
blt = rep(0,20)
for (i in 1:20){
  b = Box.test(resi,lag = i, type="Ljung-Box")
  blt[i]=b$p.value
}
blt



################ when tested for serial correlation above using VAR1 specification, we found no serial coreltion. 
################ VECM is not possible because the exogenous term is not significat. - it should be both negative and significant. since it is not siginificant, we cannot use VECM.
################ seasonal dummies are also ot significant whole checking in VAR1. hence eliminated season = 4
################




################# Forecasting ##########################################
### Forecasting with a VAR 4 period

fcast <- predict(var1, n.ahead = 4, ci = 0.95)

plot(fcast)

fcast

# plotting forecasts for one variable dlfdi
lhfaf <- fcast$fcst$dlfdi[,1]
lhfaflow <- fcast$fcst$dlfdi[,2]
lhfafupp <- fcast$fcst$dlfdi[,3]

ff <- cbind(lhfaf,lhfaflow,lhfafupp)
matplot(ff,col=c(1,2,2),lty=1,lwd=2,type='l')



### Forecasting with a VAR 8 period

fcast <- predict(var1, n.ahead = 8, ci = 0.95)

plot(fcast)

fcast

# plotting forecasts for one variable dlfdi
lhfaf <- fcast$fcst$dlfdi[,1]
lhfaflow <- fcast$fcst$dlfdi[,2]
lhfafupp <- fcast$fcst$dlfdi[,3]

ff <- cbind(lhfaf,lhfaflow,lhfafupp)
matplot(ff,col=c(1,2,2),lty=1,lwd=2,type='l')


### Forecasting with a VAR 12 period
fcast <- predict(var1, n.ahead = 12, ci = 0.95)

plot(fcast)

fcast

# plotting forecasts for one variable dlfdi
lhfaf <- fcast$fcst$dlfdi[,1]
lhfaflow <- fcast$fcst$dlfdi[,2]
lhfafupp <- fcast$fcst$dlfdi[,3]

ff <- cbind(lhfaf,lhfaflow,lhfafupp)
matplot(ff,col=c(1,2,2),lty=1,lwd=2,type='l')










