library(lmtest)
library(dynlm)
library(orcutt)
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

dataset <- read_excel("Inflation-Unemployment.xlsx")

################################################################################
##### INITIAL STATISTICAL ANALYSIS #############################################
################################################################################

describe(dataset[,-1])

b = c(2,3,5,6)
x11()
boxplot(dataset[,-1])
cor.plot(dataset[,b])
dataset$`Shock Event USA` <- as.factor(dataset$`Shock Event USA`)

################################################################################
##### TIME SERIES CONSTRUCTION #################################################
################################################################################

dataset1 <- ts(dataset, start = c(1971,1), end = (2020))
usa_cpi <- dataset1[,3]
usa_u <- dataset1[,6]
shockusa <- dataset1[,9]

u<-data.frame(value=usa_u, YEAR=1971:2020)
cpi<-data.frame(value=usa_cpi, YEAR=1971:2020)
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


library(urca)
dusa_cpi <- diff(usa_cpi)
dusa_u <- diff(usa_u)

d2usa_cpi <- diff(usa_cpi, differences = 2)
d2usa_u <- diff(usa_u, differences = 2)

################################################################################
##### STATIONARITY TEST: AUGMENTED DICKEY FULLER ###############################
################################################################################
# Augmented Dickey Fuller
# H0: non stationary
# H1: stationary

library(aTSA)
adf.test(as.matrix(usa_cpi)) 
adf.test(as.matrix(usa_u))

adf.test(as.matrix(dusa_cpi)) 
adf.test(as.matrix(dusa_u))

################################################################################
##### STATIONARITY TEST: KPSS ##################################################
################################################################################
#KPSS->
#           H0: stationary, H1: non-stationary
#When we have a non stationary process we use the difference

kpss.test(as.matrix(usa_cpi))
kpss.test(as.matrix(usa_u))

kpss.test(as.matrix(dusa_cpi))
kpss.test(as.matrix(dusa_u))

################################################################################
##### PLOT DIFFERENCES ON TIMES SERIES #########################################
################################################################################

u<-data.frame(value=dusa_u, YEAR=1972:2020)
cpi<-data.frame(value=dusa_cpi, YEAR=1972:2020)
{
  x11()
  p = ggplot(u) +
    geom_line(aes(x=as.numeric(YEAR),y=as.numeric(value),col="Unemployment")) +
    geom_line(data=cpi, aes(x=as.numeric(YEAR),y=as.numeric(value),col="Inflation")) +
    labs(title = "Difference on Time Series") + 
    xlab("year") +  ylab("") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values = c("blue", "red"), guide = guide_legend(title = "Data"))
  
  print(p)
}

################################################################################
##### SELECT VARIABLES #########################################################
################################################################################

library(vars)
y <- cbind(dusa_u, dusa_cpi ) #matrix
colnames(y) <- c("d.usa_u","d.usa_cpi")
y <- na.trim(y)

#VAR -> y(t) + A(1) * y(t-1) + ... + A(p) * y(t-p) + eps(t)    eps ~ N(o, sigma)
#VEC -> delta(y(t)) = mu + beta(1) * delta(y(t-1)) + ... beta(p) * delta(y(t-p)) + eps(t)

################################################################################
##### COINTEGRATION TEST: JOHANSEN TEST ########################################
################################################################################
#JOHANSEN->
#           trace H0: tao = tao* < k, H1: tao = k
#           eigenvalue: tao = tao* <k
#           tao = tao *

library(urca)
y.CA <- ca.jo(cbind(dusa_u, dusa_cpi), type="trace", K=5, ecdet = c("none", "const", "trend"), spec=c("longrun", "transitory"))
summary(y.CA)
# We choose nlags such that:
# 21.92 > 15.66 --> we reject r=0
# 6.14 < 6.50 --> we accept r=1

y.CA <- ca.jo(y, type="eigen", K=5)
summary(y.CA) 

################################################################################
##### ALTERNATIVE TEST: ENGLE-GRANGER TEST #####################################
################################################################################

library(aTSA)
coint.test(as.matrix(dusa_u), as.matrix(dusa_cpi) ,d = 0, nlag = 5, output = TRUE)
# H0: no cointegration

################################################################################
##### ESTIMATE VEC MODEL #######################################################
################################################################################

y.VEC <- cajorls(y.CA)
summary(y.VEC$rlm)

################################################################################
##### USA: LONG RUN (INFLATION) ################################################
################################################################################
source("USA_long_inflation.R")

################################################################################
##### USA: LONG RUN (UNEMPLOYMENT) #############################################
################################################################################
source("USA_long_unemployment.R")

################################################################################
##### USA: SHORT RUN (INFLATION) ###############################################
################################################################################
source("USA_short_inflation.R")

################################################################################
##### USA: SHORT RUN (UNEMPLOYMENT) ############################################
################################################################################
source("USA_short_unemployment.R")

################################################################################
##### USA: DUMMY ###############################################################
################################################################################
source("USA_dummy.R")



