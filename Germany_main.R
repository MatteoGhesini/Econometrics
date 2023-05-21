library(lmtest)
library(dynlm)
library(orcutt)
library(FinTS)
library(sandwich)
library(car)
library(nlWaldTest)
library(forecast)
library(xts)
library(readxl)
library(ggplot2)
library(foreign) #Or you can use require(foreign)
library(xts)
library(ggplot2)
library(pastecs)
require(psych)
library(MASS)
library(ggplot2)
library(aTSA)
library(vars)
library(tidyverse)
library(car)
library(stargazer)
library(plm)
library(ecm)
library(panelr)
library(aod)
require(data.table)
require(tidyverse)
require(ggpubr)

dataset <- read_excel("Inflation-Unemployment.xlsx")

################################################################################
##### INITIAL STATISTICAL ANALYSIS #############################################
################################################################################

describe(dataset[,-1])

b = c(2,5)
x11()
boxplot(dataset[,-1])
cor.plot(dataset[,b])
dataset$'Shock Event Ger' <- as.factor(dataset$'Shock Event Ger')

################################################################################
##### TIME SERIES CONSTRUCTION #################################################
################################################################################

dataset1 <- ts(dataset, start = c(1971,1), end = (2020))
g_cpi <- dataset1[,2]
g_u <- dataset1[,5]
shockg <- dataset1[,8]

u<-data.frame(value=g_u, YEAR=1971:2020)
cpi<-data.frame(value=g_cpi, YEAR=1971:2020)
{
  x11()
  p = ggplot(u) +
    geom_line(aes(x=as.numeric(YEAR),y=as.numeric(value),col="Unemployment")) +
    geom_line(data=cpi, aes(x=as.numeric(YEAR),y=as.numeric(value),col="Inflation")) +
    labs(title = "Time Series") + 
    xlab("year") +  ylab("") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values = c("blue", "red"), guide = guide_legend(title = "Data"))
  
  print(p)
}

#JOHANSEN->
#           trace H0: tao = tao* < k, H1: tao = k
#           eigenvalue: tao = tao* <k
#           tao = tao *
#KESS->
#           H0: stationary, H1: non-stationary
#When we have a non stationary process we use the difference

# Augmented Dickey Fuller

# H0: non stationary
# H1: stationary


library(aTSA)
adf.test(as.matrix(g_cpi)) 
adf.test(as.matrix(g_u))

dg_u <- diff(g_u)
dg_cpi <- diff(g_cpi)

adf.test(as.matrix(dg_cpi)) 
adf.test(as.matrix(dg_u))

# KPSS 

kpss.test(as.matrix(g_cpi))
kpss.test(as.matrix(g_u))

kpss.test(as.matrix(dg_cpi))
kpss.test(as.matrix(dg_u))

#the variables are I(1)

################################################################################
##### SELECT VARIABLES #########################################################
################################################################################

library(vars)
y <- cbind(dg_u, dg_cpi ) #matrix
colnames(y) <- c("d.g_u","d.g_cpi")
y <- na.trim(y)

#VAR -> y(t) + A(1) * y(t-1) + ... + A(p) * y(t-p) + eps(t)    eps ~ N(o, sigma)
#VEC -> delta(y(t)) = mu + beta(1) * delta(y(t-1)) + ... beta(p) * delta(y(t-p)) + eps(t)

################################################################################
##### COINTEGRATION TEST: JOHANSEN TEST ########################################
################################################################################

# We choose nlags such that we reject r = 0 and accept r=1
library(urca)
y.CA <- ca.jo(cbind(dg_u, dg_cpi), type="trace", K=6, ecdet = c("none", "const", "trend"), spec=c("longrun", "transitory"))
summary(y.CA)
# 25.21 > 6.50 --> we reject r=0
# 5.40 < 11.65 --> we accept r=1

y.CA <- ca.jo(y, type="eigen", K=6)
summary(y.CA) 

# Alternative: Engle-Granger test

library(aTSA)
coint.test(as.matrix(dg_u), as.matrix(dg_cpi) ,d = 0, nlag = 6, output = TRUE)
# H0: no cointegration

# Estimate VEC model

y.VEC <- cajorls(y.CA)

# to see t-statistics and p-values
summary(y.VEC$rlm)


source("Germany_long_inflation.R")
source("Germany_long_unemployment.R")
source("Germany_short_inflation.R")
source("Germany_short_unemployment.R")







