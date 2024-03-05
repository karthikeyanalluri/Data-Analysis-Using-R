install.packages("maptools")
install.packages("spdep")
install.packages("leaflet")
install.packages("RColorBrewer")
install.packages("rgeos")
install.packages("spatialreg")
install.packages("rgdal")
install.packages("rgdal")
install.packages("spatialprobit")

library(spatialprobit)
library(maptools)
library(spdep)
library(spatialreg)
library(spdep)
library(leaflet)
library(RColorBrewer)
library(rgeos)
library(rgdal)

data = read.csv("theft_crime.csv",header=TRUE)
crime = as.double(data$bulglary > 300)
n=length(crime)
crime
x = cbind(data$poverty,data$unemp)
coords = cbind(data$LON,data$LAT)

k1 <- knn2nb(knearneigh(coords, k=5))
W <- nb2listw(k1, zero.policy = T)
Wt <- as(W, "CsparseMatrix")
trMatc <- trW(Wt, type="mult")
#Wx <- as.matrix(Wt)%*%x
#data=data.frame(x1 = x,
#                Wx = Wx,
#                y = y)
data=data.frame(poverty = x[,1],
                unemp = x[,2],
                crime = crime)

reg = crime ~ poverty + unemp
# Estimate SAR probit model
sarprobit.fit1 <- sarprobit(reg,data=data, Wt, ndraw = 5000, burn.in = 1000, thinning = 1, m = 10)

summary(sarprobit.fit1)
plot(sarprobit.fit1)

# evaluate direct, indirect, and total effects
impacts(sarprobit.fit1)

####### LPM 

reg2 <- lm(crime ~ poverty + unemp, data = data)
summary(reg2)
