##Loading Packages 
#install.packages("flextable")
#install.packages("knitr")
#install.packages("officer")
library("officer")
library(flextable)
library("knitr")
library(rmarkdown)
library("rstudioapi")
library("stargazer")
library("plm")
library("lmtest")
library(plm)
library(car)
library(dplyr)
library("dynlm")
library(readxl)
library(fastDummies)
library(stargazer)
#Loading the Data
data <- read.csv("Panel Dataset final.csv")
#View(data)
attach(data)


#assigning state id #
data$id<-as.numeric(factor(data$State))

data.panel<-pdata.frame(data, index = c("id", "year"))
pdim(data.panel)
unique(data.panel$year)
names(data.panel)


summary(data)
options("scipen"=9999999, digits=7)


##Checking if panel data is balanced
pdim(data)

#Making Panel data Balanced 
#data.panel<-pdata.frame(data, index = c("id", "Year"))
##Checking if panel data is balanced
#pdim(data.panel)

#Assigning varibale
data.panel$LRPP<-log(data.panel$RPP)
data.panel$LHPI<- log(data.panel$HPI)
data.panel$LWAGE <- log(data.panel$Average.Hourly.Earnings)



#####################Dynamic Panel################################
# Anderson and Hsiao Estimator
#install.packages("AER")
library(AER)
#taking differnce of all variables:
data.panel$D.UN<- diff(data.panel$Unemployment.Rate)
data.panel$D.LRPP<- diff(data.panel$RPP)
data.panel$D.LHPI<- diff(data.panel$LHPI)
data.panel$D.LWAGE<- diff(data.panel$LWAGE)


#taking lag for dependent variable 
data.panel$L.LHPI <-lag(data.panel$LHPI) ## taking first lag for dependent variable LHPI-log(HPI)
data.panel$DL.LHPI<-diff(data.panel$L.LHPI) ##taking difference of first lagged dependent variable
data.panel$L2.LHPI<-lag(data.panel$L.LHPI)  ## taking second difference of first lagged dependent variable

library(AER)
AH_2SLS <- ivreg(D.LHPI ~ DL.LHPI + D.UN + D.LRPP + D.LWAGE +  factor(year)-1  ##Here we assume these variables as exogenous- D.GDP + D.PCI + D.EMP + D.CONSUM  or control variables
                 | L2.LHPI + D.UN + D.LRPP + D.LWAGE +  factor(year)-1, data=data.panel)  ###here we have taken difference variable so we have to subtract 1 year frm the year dummy so we wrote factor(Year)-1
summary(AH_2SLS, diagnostics = TRUE) 


#AB_GMM_1 <- pgmm(LHPI ~  Unemployment.Rate + lag(Unemployment.Rate) + LRPP + lag(LRPP) + LWAGE+ lag(LWAGE)  | lag(LHPI, 2:99),   ## Here- all the lag values of dependent variable "inflation" will be used as IV i.e. lag(INF, 2:99). In this data set, there are data for 4 time periods and one time period i.e. current time lag value of inflation will be dependent and onetime period or lag value of inflation will be independent and other remaining 2 will be instrumental variable in the model so instead counting and putting the value we directly put 99 as maximum value 
#               data = data.panel, effect = "twoways", model = "twostep")
#summary(AB_GMM_1, time.dummies=TRUE)


AB_GMM <- pgmm(LHPI ~ lag(LHPI) + Unemployment.Rate + lag(Unemployment.Rate) + LRPP + lag(LRPP) + LWAGE+ lag(LWAGE)  | lag(LHPI, 2:99),   ## Here- all the lag values of dependent variable "inflation" will be used as IV i.e. lag(INF, 2:99). In this data set, there are data for 4 time periods and one time period i.e. current time lag value of inflation will be dependent and onetime period or lag value of inflation will be independent and other remaining 2 will be instrumental variable in the model so instead counting and putting the value we directly put 99 as maximum value 
               data = data.panel, effect = "twoways", model = "twostep")   ###here mode = twostep -- > 2 step GMM and Here effect = two ways includes both time factor and unobserved heterogeneity we don't need to put them separately  
summary(AB_GMM, time.dummies=TRUE)

#Arellano and Bond Estimator, One-step
AB_GMM1 <- pgmm(LHPI ~ lag(LHPI) + log(Unemployment.Rate) + lag(log(Unemployment.Rate)) + LRPP + lag(LRPP) + LWAGE+ lag(LWAGE)  | lag(LHPI, 2:99), 
                data = data.panel, effect = "twoways", model = "onestep")  
summary(AB_GMM1, time.dummies=TRUE)

stargazer(AB_GMM, AB_GMM1, type="text", keep.stat=c('rsq','n'), 
          omit=c("year"), title="Table 2: House Price Index, TwoStep vs OneStep")


AB_GMM3 <- pgmm(LHPI ~ lag(LHPI) + Unemployment.Rate + lag(Unemployment.Rate) + LRPP + lag(LRPP) + LWAGE+ lag(LWAGE)  | lag(LHPI, 3:99), 
                data = data.panel, effect = "twoways", model = "twostep")
summary(AB_GMM3, time.dummies=TRUE)





###########################Static Model################################################################


##Analyzing through different estimators 
# Pooled OLS
pols<-plm(LHPI ~ Unemployment.Rate + LRPP + LWAGE + factor(year), data=data.panel, model="pooling")
summary(pols)
# RE 
re<-plm(LHPI ~ Unemployment.Rate + LRPP + LWAGE + factor(year),data=data.panel, model="random")
summary(re)
# FD 
fd<-plm(LHPI ~ Unemployment.Rate + LRPP + LWAGE + factor(year), data=data.panel, model="fd")
summary(fd)
# FE 
fe<-plm(LHPI ~ Unemployment.Rate + LRPP + LWAGE + factor(year), data=data.panel, model="within")
summary(fe)


##Reporting output
stargazer(pols,re,fd,fe, type="text", keep.stat=c('rsq','n'), 
          omit=c("year"), omit.labels=c("Year Dummies"),
          title="Table 1 LHPI - pooled OLS RE FD FE")

##Looking for the best estimate
# BP test for unobserved heterogeneity
bptest(pols)



#Hausman Test 

# FE VS RE
phtest(fe,re)

#Serial correlation test for FD
pwfdtest(fd)

##FE serial correlation test
pwartest(fe)

##Thus RE is best Estimator 
stargazer(re,type="text", keep.stat=c('rsq','n'), 
          omit=c("year"), omit.labels=c("Year Dummies"),
          title="Table 2 LHPI - Random Effect")
