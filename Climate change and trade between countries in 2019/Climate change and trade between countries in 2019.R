# Clear the console
rm(list=ls())

# Set the working directory


# install.packages("stargazer")
# install.packages("lmtest")
# install.package("ggplot")
# install.packages("gapminder")
# install.packages("plotly")
# install.packages("dplyr")


library(dplyr)
library(carData)
library(stargazer)
library(lmtest)
library(gapminder)
library(plotly)
library(car)

# force R not to use scientific notation 
options("scipen"=99999, digits=3)

# load the dataset to analyze
data = read.csv("Project data.csv",header=TRUE)
attach(data)

# Regressions

tradereg <- lm(log(trade) ~ trees + CO2 + GDP)
summary(tradereg)

exportreg <- lm(log(export) ~ trees + CO2 + GDP)
summary(exportreg)

importreg <- lm(log(import) ~ trees + CO2 + GDP)
summary(importreg)

FDIreg <- lm(log(FDI) ~ trees + CO2 + GDP)
summary(FDIreg)

######interpretations
#1. as CO2 increases by i unit, trade increases by 2.965%.
#2. as trees inccreases by 1 unit, trade increases by 0.486%
# as CO2 increaese by 1 unit, trade increases by 6.535%
# as GDP increases by 1 unit, trade increases by 3.245%
#3. as CO2 increaese by 1 unit, trade increases by 2.489%
# as GDP increases by 1 unit, trade increases by 4.787%
#4. as trees inccreases by 1 unit, trade increases by 1.022%
#as GDP increases by 1 unit, trade increases by 13.818%





## Check Correlation

data %>% select(trees, CO2, GDP) %>% cor()

# interpretation: trees and CO2 are negatively correlated and 

#########VIF##################

vif(tradereg)
vif(exportreg)
vif(importreg)
vif(FDIreg)
##VIF to test multicollinearity, 
#we use VIF(variance inflation factor) to measures the correlation and strength of correlation between the predictor variables in a regression model.

################################
###A value of 1 indicates there is no correlation between a given predictor
#variable and any other predictor variables in the model.

### A value between 1 and 5 indicates moderate correlation between a given predictor variable and other predictor variables in the model,
#but this is often not severe enough to require attention.

### A value greater than 5 indicates potentially severe correlation between a given predictor variable and other predictor variables in the model
#In this case, the coefficient estimates and p-values in the regression output are likely unreliable.
###############################


##VIF interpretation
# since the value is 1.01 indicates low to moderate correlation between the independent variables in the models.


# Stargazer 
stargazer(tradereg, exportreg, importreg,keep.stat=c("n","rsq"),type="text",out="reg_reporting.txt", title="Table 1. Impact of CO2 and renewable energy use on Trade")

# Write down the model for the Squared residuals??
data$residualSQ<-tradereg$residuals^2
data$fit<-tradereg$fitted.values 

# BP test

bptest(tradereg, data=data)
bptest(exportreg, data=data)
bptest(importreg, data=data)


bptest(tradereg, data=data)
####interpretation: The test statistic is 5 and the corresponding p-value is 0.2. since the p-value is not less than 0.05, we fail to reject the null hypothesis.
################### Since  We do not have sufficient evidence to say that heteroscedasticity is present in the regression model.
bptest(exportreg, data=data)
####interpretation: The test statistic is 6 and the corresponding p-value is 0.1. since the p-value is not less than 0.05, we fail to reject the null hypothesis.
################### Since  We do not have sufficient evidence to say that heteroscedasticity is present in the regression model.
bptest(importreg, data=data)
####interpretation: The test statistic is 5 and the corresponding p-value is 0.2. since the p-value is not less than 0.05, we fail to reject the null hypothesis.
################### Since  We do not have sufficient evidence to say that heteroscedasticity is present in the regression model.

#here the null hypothesis is taken as Homoskedasticity and alternate hypothesis is taken as heteroskedasticity


# interactive plot

library(data.table)

mean(CO2)

df %>%
  mutate(CO2abvavg = case_when(CO2>=208782 ~ CO2))


p <- ggplot(data=data, aes(export,CO2)) +
  geom_point(aes(color=Country)) +
  ggtitle("Country wise CO2 AND Exports")

ggplotly(p)
summary(CO2)


mean(CO2)
CO2abvavg=CO2[CO2>=5.61]
summary(CO2abvavg)
CO2abvavg

length(trees)
length(CO2abvavg)

mean(CO2)
treeabvavg=trees[trees>=31.1]
summary(CO2abvavg)
CO2abvavg

length(trees)
length(CO2abvavg)




