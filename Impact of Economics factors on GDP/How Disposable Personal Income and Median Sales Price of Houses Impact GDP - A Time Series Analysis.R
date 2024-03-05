library(tsDyn)
library(vars)
library(quantmod)
library(tseries)
library(urca)
library(dynlm)


df <- read.csv("Alldata.csv", header = TRUE)
dfts = ts(df[2:8],  start = c(1963, 1), frequency = 4)

plot.ts(dfts)

#*******************************************define variables**************************************************
unemp0 <- dfts[,1]
lgdp0 <- log(dfts[,2])
ldpi0 <- log(dfts[,3])
ffe0 <- dfts[,4]
cpi0 <- dfts[,5]
fnfhwage0 <- dfts[,6]
lmspus0 <- log(dfts[,7])


intord = function(y){
  n = length(y)
  
  dy = diff(y)
  d2y = diff(dy)
  
  z = cbind(y[3:n],dy[2:(n-1)],d2y)
  
  par(mfrow=c(3,2))
  plot(z[,1],col=1,type='l',main=paste("SD=",round(sd(z[,1]),4)),ylab="y", xlab="Level")
  abline(h=mean(z[,1]),col=2)
  acf(y,lwd=3,main="ACF for level")
  plot(z[,2],col=3,type='l',main=paste("SD=",round(sd(z[,2]),4)),ylab=expression(paste(Delta * y)), xlab="1st difference")
  abline(h=mean(z[,2]),col=2)
  acf(z[,2],lwd=3,main="ACF for 1st difference")
  plot(z[,3],col=4,type='l',main=paste("SD=",round(sd(z[,3]),4)),ylab=expression(paste(Delta^2 * y)), xlab="2nd difference")
  abline(h=mean(z[,3]),col=2)
  plot(1:10, xaxt='n', yaxt ='n', col="white", main="ADF test & critical values", ylab="", xlab="")
  
  
  
  # ADF test first round
  
  # pmax is selected max lag length, maxp must be at least 2
  pmax = 12
  maxp = pmax + 1
  
  
  n = length(y)
  dy = diff(y)
  
  
  z = embed(dy,maxp)
  
  zz = embed(y,maxp); y1 = zz[,2]
  xx = cbind(z[,1],y[maxp:(n-1)],z[,2:maxp])
  nobs = nrow(xx)
  # DF test (0 lags)
  c = rep(1,nrow(xx))
  xvars = cbind(c,y[maxp:(n-1)])
  yvar = xx[,1]
  ixx = solve(t(xvars)%*%xvars)
  bh = ixx%*%t(xvars)%*%yvar
  yh = xvars%*%bh
  res = yvar - yh
  rss = t(res)%*%res
  k = ncol(xvars)
  s2 = as.numeric(rss/(nobs-k))
  covb = s2*ixx
  seb = sqrt(diag(covb))
  
  bic = rep(0,maxp)
  adft = bic
  adft[1] = bh[2]/seb[2]
  bic[1] = log(rss/nobs) + log(nobs)*(k+1)/nobs
  
  for (i in 3:(maxp+1)) {
    xvars = cbind(c,xx[,2:i])
    ixx = solve(t(xvars)%*%xvars)
    bh = ixx%*%t(xvars)%*%yvar
    yh = xvars%*%bh
    res = yvar - yh
    rss = t(res)%*%res
    k = ncol(xvars)
    s2 = as.numeric(rss/(nobs-k))
    covb = s2*ixx
    seb = sqrt(diag(covb))
    adft[i-1] = bh[2]/seb[2]
    bic[i-1] = log(rss/nobs) + log(nobs)*(k+1)/nobs
  }
  
  ind = which.min(bic)
  # cat("ADF t-value","lags")
  round1 = c(round(adft[ind],2))
  
  # ADF test second round
  
  y = dy
  
  n = length(y)
  dy = diff(y)
  
  
  z = embed(dy,maxp)
  
  zz = embed(y,maxp); y1 = zz[,2]
  xx = cbind(z[,1],y[maxp:(n-1)],z[,2:maxp])
  nobs = nrow(xx)
  # DF test (0 lags)
  c = rep(1,nrow(xx))
  xvars = cbind(c,y[maxp:(n-1)])
  yvar = xx[,1]
  ixx = solve(t(xvars)%*%xvars)
  bh = ixx%*%t(xvars)%*%yvar
  yh = xvars%*%bh
  res = yvar - yh
  rss = t(res)%*%res
  k = ncol(xvars)
  s2 = as.numeric(rss/(nobs-k))
  covb = s2*ixx
  seb = sqrt(diag(covb))
  
  bic = rep(0,maxp)
  adft = bic
  adft[1] = bh[2]/seb[2]
  bic[1] = log(rss/nobs) + log(nobs)*(k+1)/nobs
  
  for (i in 3:(maxp+1)) {
    xvars = cbind(c,xx[,2:i])
    ixx = solve(t(xvars)%*%xvars)
    bh = ixx%*%t(xvars)%*%yvar
    yh = xvars%*%bh
    res = yvar - yh
    rss = t(res)%*%res
    k = ncol(xvars)
    s2 = as.numeric(rss/(nobs-k))
    covb = s2*ixx
    seb = sqrt(diag(covb))
    adft[i-1] = bh[2]/seb[2]
    bic[i-1] = log(rss/nobs) + log(nobs)*(k+1)/nobs
  }
  bic
  ind = which.min(bic)
  cat("ADF t-value","lags")
  round2 = c(round(adft[ind],2))
  rbind(round1,round2)
  
  ADF.statistics = cbind(round1,round2)
  
  # MacKinnon critical values
  c1 = -3.43035 - 6.5393/nobs - 16.786/nobs^2 - 79.433/nobs^3
  c5 = -2.86154 - 2.8903/nobs - 4.234/nobs^2 - 40.04/nobs^3
  c10 = -2.56677 - 1.5384/nobs - 2.809/nobs^2
  
  # cat("10%, 5% and 1% critical values")
  Critical.values = round(c(c10,c5,c1),2)
  
  
  line1<-expression(paste("   ",y,"           ", Delta * y,"        ","10%       5%	      1%"))
  line2<-paste(ADF.statistics[1],"    ",ADF.statistics[2],"  "
               ,Critical.values[1],"   ",Critical.values[2],"   ",Critical.values[3])
  
  legend("center","(x,y)", # places a legend at the appropriate place 
         c(line1,line2), # puts text in the legend 
         lty=c(1,2), # gives the legend appropriate symbols (lines)       
         lwd=c(3,2),col=c("white","white"),bty = "n",cex=1.3) # gives the legend lines the correct color and width
  
  ADF.statistics
  Critical.values
  
  
  list(adf.stat = ADF.statistics, critvals = Critical.values)
  
  
  
}

#*************************************************Test for Stationarity*****************************************

par(mar = c(3, 3, 3, 3))
#intord(unemp0) #I(0) or barely I(1)
#intord(diff(unemp0,4)) # becomes stationary
intord(lgdp0) #I(2)
dlgdp = diff(lgdp0,4) # seasonally differenced
intord(dlgdp) #I(1)
intord(ldpi0) #(I(1)), because sd reduced more than half
#intord(ffe0) #(I(1))
#intord(cpi0) #(I(1))
#intord(fnfhwage0) #(I(1))
intord(lmspus0) #(I(1))

# do not have the same number of observations
# remove one period
#unemp = unemp0[-1] # remove first observation othersize lm0[13:120]
#unemp = ts(unemp0[5:240],  start = c(1964, 1), frequency = 4) 
ldpi = ts(ldpi0[5:240],  start = c(1964, 1), frequency = 4)
#ffe = ts(ffe0[5:240],  start = c(1964, 1), frequency = 4)
#cpi = ts(cpi0[5:240],  start = c(1964, 1), frequency = 4)
#fnfhwage = ts(fnfhwage0[5:240],  start = c(1964, 1), frequency = 4)
lmspus = ts(lmspus0[5:240],  start = c(1964, 1), frequency = 4)

#first differencing
#dunemp <- diff(unemp) #(I(0))
ddlgdp <- diff(dlgdp)#(I(0))
dldpi <- diff(ldpi)#(I(0))
#dffe <- diff(ffe)#(I(0))
#dcpi <- diff(cpi)#(I(0))
#dfnfhwage <- diff(fnfhwage)#(I(0)
dlmspus <- diff(lmspus)#(I(0)

n <- length(dlgdp)

#variables in level -- I(1)
# unemp,dlgdp,ldpi,ffe,cpi,fnfhwage,lmspus)
#ly <- cbind(cpi,fnfhwage,lmspus)
#ly <- cbind(unemp,dlgdp,lmspus)
#ly <- cbind(ldpi,ffe,cpi,lmspus)
#ly <- cbind(ldpi,ffe,cpi)
#ly <- cbind(ldpi,cpi,fnfhwage)
#ly <- cbind(ldpi,cpi,lmspus)
#ly <- cbind(dlgdp,cpi,lmspus)
ly <- cbind(dlgdp,ldpi,lmspus)

#variables in first differencing -- I(0)
#dy <- cbind(dcpi,dfnfhwage,dlmspus)
#dy <- cbind(dunemp,dlgdp,dlmspus)
#dy <- cbind(dldpi,dffe,dcpi,dlmspus)
#dy <- cbind(ddlgdp,dcpi,dlmspus)
dy <- cbind(ddlgdp,dldpi,dlmspus)

#*******************************************Selection of lags******************************************

# lag selection criteria
VARselect(ly, lag.max=12, type="const")

#*********************************************Co-integration*********************************************
## cointegration - variables must be in levels! #
jc <- ca.jo(ly, type="eigen", ecdet="const",K=4) # k based on SC(n) value from VARselect, but min 2
summary(jc)
jct <- ca.jo(ly, type="trace", ecdet="const",K=4) 
summary(jct)

# eigenvalues, etc., can be taken from the output of ca.jo
eigenvals <- jc@lambda

# cointegrating relationships
cointv <- jc@V
cointj <- cointv[,2]
yym <- as.matrix(ly)
ecmj <- yym%*%cointj[1:3] + cointj[4] # eigenvectors with data plus intercept
intord(ecmj)


# Engle-Granger
c1 <- lm(dlgdp~ldpi+lmspus) # 
summary(c1)
ecm <- c1$residuals
#dev.off()
par(mar = c(3, 3, 3, 3))
intord(ecm)


# include one lag of either ecmj or ecm in the VAR as an exog. variable
# to estimate the VECM

par(mfrow=c(1,2))
plot(ecm,type='l')
plot(ecmj,type='l')

#create a lag for ECM ter,
ec <- embed(ecm,2) # first lag
ecm1 <- ec[,2]
ecj <- embed(ecmj,2) # first lag
ecmj1 <- ecj[,2]

# (VECM since appear co-integrated)

#****************************VAR, VECM(Johansen), VECM(Engle-Granger) models*******************************
# First a VAR(1): p = 2 based on BIC
var1 <- VAR(dy, p=4, type="cons", season = 4) # type ="cons","trend","both"
summary(var1)
dev.off()
par(mar = c(3, 3, 3, 3))
plot(var1, names = "ddlgdp")
plot(var1, names = "dldpi")
plot(var1, names = "dlmspus")
BIC(var1)
AIC(var1)

# VECM with Johansen
var2 <- VAR(dy, p=4, type="cons",exogen=ecmj1,season=4)
summary(var2)
BIC(var2)
AIC(var2)

# VECM with EG
var3 <- VAR(dy, p=4, type="cons",exogen=ecm1,season=4)
summary(var3)
BIC(var3)
AIC(var3)

# plots of fit results
dev.off()
par(mar = c(3, 3, 3, 3))
plot(var2, names = "ddlgdp")
plot(var2, names = "dldpi")
plot(var2, names = "dlmspus")

plot(var3, names = "ddlgdp")
plot(var3, names = "dldpi")
plot(var3, names = "dlmspus")

#********************************************** Granger causality*******************************************************8
# Johansen VECM procedure - unrestricted
# ly <- cbind(dlgdp,ldpi,lmspus) - unrestricted setup

####
####### Does lmspus granger cause dlgdp? Ans: No
vecm1 <- ca.jo(ly, ecdet = "trend", type="eigen", K=4, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=1)  #dlgdp is 1st equation

# restricted
lyr <- cbind(dlgdp,ldpi)    #  drop (lmspus) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1)  #dlgdp is 1st equation

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 
# as p-value > 0.05, we fail to reject the null:there is no Granger causality, so lmspus does not granger cause dlgdp



####
####### Deos ldpi granger cause dlgdp? Ans: yes
vecm1 <- ca.jo(ly, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=1)  #dlgdp is 1st equation

# restricted
lyr <- cbind(dlgdp,lmspus)    #  drop (ldpi) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1)  #dlgdp is 1st equation

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 
# as p-value < 0.05, we reject the null:there is no Granger causality, so ldpi granger causes dlgdp



####
####### Deos dlgdp granger cause ldpi? Ans: No
vecm1 <- ca.jo(ly, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=2)  #ldpi is 2nd equation

# restricted
lyr <- cbind(ldpi,lmspus)    #  drop (dlgdp) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1)  #ldpi is 1st equation

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 
# as p-value > 0.05, we fail to reject the null:there is no Granger causality, so dlgdp does not granger cause ldpi



####
####### Deos lmspus granger cause ldpi? Ans: No
vecm1 <- ca.jo(ly, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=2)  #ldpi is 2nd equation

# restricted
lyr <- cbind(ldpi,dlgdp)    #  drop (lmspus) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1)  #ldpi is 1st equation

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 
# as p-value > 0.05, we fail to reject the null:there is no Granger causality, so lmspus does not granger cause ldpi



####
####### Deos dlgdp granger cause lmspus? Ans: yes
vecm1 <- ca.jo(ly, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=3)  #lmspus is 3rd equation

# restricted
lyr <- cbind(lmspus,ldpi)    #  drop (dlgdp) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1)  #lmspus is 1st equation

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 
# as p-value < 0.05, we reject the null:there is no Granger causality, so dlgdp granger causes lmspus



####
####### Deos ldpi granger cause lmspus? Ans: No
vecm1 <- ca.jo(ly, ecdet = "trend", type="eigen", K=2, spec="longrun",
               season=4)
ve1 <- cajools(vecm1,reg.number=3)  #lmspus is 3rd equation

# restricted
lyr <- cbind(lmspus,dlgdp)    #  drop (ldpi) one variable for restricted model
vecmr <- ca.jo(lyr, ecdet = "const", type="eigen", K=2, spec="longrun",
               season=4)
ve1r <- cajools(vecmr,reg.number=1)  #lmspus is 1st equation

# Joint F-test for Granger causality
anova(ve1, ve1r, test="F") 
# as p-value > 0.05, we reject the null:there is no Granger causality, so ldpi does not granger cause lmspus


#***************************************Impulse Response Function********************************************

# variables in var2: dlgdp, ldpi, lmspus

# IRFs
irfs <- irf(var3, impulse = "ddlgdp", response = c("ddlgdp","dldpi","dlmspus"), boot = TRUE)
dev.off()
par(mar = c(1, 1, 1, 1))
plot(irfs)

irfs <- irf(var3, impulse = "dldpi", response = c("ddlgdp","dldpi","dlmspus"), boot = TRUE)
plot(irfs)

irfs <- irf(var3, impulse = "dlmspus", response = c("ddlgdp","dldpi","dlmspus"), boot = TRUE)
plot(irfs)

#****************************************Variance Decomposition***********************************************

# Variance Decompositions (contribution of each variable to predicting a variable)
vard <- fevd(var3, n.ahead=4)
vard
vard$dha
plot(vard, col=1:4)


#**********************************************Forecasting****************************************************

# Forecasting with a VECM 4 period
varf <- vec2var(vecm1, r = 1)
fcast <- predict(varf, n.ahead = 4, ci = 0.95) 
plot(fcast)
fcast

# Forecasting with a VECM 8 period
varf <- vec2var(vecm1, r = 1)
fcast <- predict(varf, n.ahead = 8, ci = 0.95) 
plot(fcast)
fcast


# plotting forecasts for one variable (lm)
lhfaf <- fcast$fcst$dlgdp[,1]
lhfaflow <- fcast$fcst$dlgdp[,2]
lhfafupp <- fcast$fcst$dlgdp[,3]

ff <- cbind(lhfaf,lhfaflow,lhfafupp)
matplot(ff,col=c(1,2,2),lty=1,lwd=2,type='l')

#*******************************************Test for Serial Correlation of residuals****************************************
#**************************************************Box-Ljung Q Statistic****************************************************
# BoX-Ljung Q Statistic for ddlgdp
resi = var2$varresult$ddlgdp$residuals
b = Box.test(resi,lag = 20, type="Ljung-Box")
b
blt = rep(0,20)
for (i in 1:20){
  b = Box.test(resi,lag = i, type="Ljung-Box")
  blt[i]=b$p.value
}
blt


# BoX-Ljung Q Statistic for dldpi
resi = var2$varresult$dldpi$residuals
b = Box.test(resi,lag = 20, type="Ljung-Box")
b
blt = rep(0,20)
for (i in 1:20){
  b = Box.test(resi,lag = i, type="Ljung-Box")
  blt[i]=b$p.value
}
blt


# BoX-Ljung Q Statistic for dlmspus
resi = var2$varresult$dlmspus$residuals
b = Box.test(resi,lag = 20, type="Ljung-Box")
b
blt = rep(0,20)
for (i in 1:20){
  b = Box.test(resi,lag = i, type="Ljung-Box")
  blt[i]=b$p.value
}
blt


#*******************************************Test for Volatility********************************************
#***********************************************ARCH Effects***********************************************

Arch1 <- arch.test(varf, lags.multi = 3, multivariate.only = TRUE)
Arch1

# as p-value <0.05, we reject the null (no conditional heteroskedasticity).
# so there is conditional heteroskedasticity.

dev.off()
par(mar = c(1, 1, 1, 1))
plot(resi)
plot(resi^2)

