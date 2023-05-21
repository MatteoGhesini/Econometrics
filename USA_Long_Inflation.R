################################################################################
##### USA: LONG RUN (INFLATION) ################################################
################################################################################

usa_u_1 <- lag(usa_u, -1, na.pad = TRUE) #last term the last part of vector is deleted
usa_cpi_1 <- lag(usa_cpi, -1, na.pad = TRUE)

#MA(1)
usafitLR <- dynlm(usa_cpi ~  usa_u + usa_u_1)
summary(usafitLR)

###################

#AR(1)
ARusafitLR <- dynlm(usa_cpi ~  usa_u + usa_cpi_1)
summary(ARusafitLR)


###############

#ARMA(1,1)
ARMAusafitLR <- dynlm(usa_cpi ~  usa_u + usa_cpi_1 + usa_u_1)
summary(ARMAusafitLR)


##########

#ARMA(2,2)
usa_cpi_2 <- lag(usa_cpi, -2, na.pad = TRUE) 
usa_u_2 <- lag(usa_u, -2, na.pad = TRUE)

ARMA2usafitLR <- dynlm(usa_cpi ~  usa_u + usa_cpi_1 + usa_u_1 + usa_u_2 + usa_cpi_2)
summary(ARMA2usafitLR)

#ARMA(2,1)

ARMA21usafitLR <- dynlm(usa_cpi ~  usa_u + usa_cpi_1 + usa_u_1 + usa_cpi_2)
summary(ARMA21usafitLR)

#AR(2)
AR2usafitLR <- dynlm(usa_cpi ~  usa_u + usa_cpi_1 + usa_cpi_2)
summary(AR2usafitLR)

AIC(usafitLR, ARusafitLR, ARMAusafitLR, ARMA2usafitLR, ARMA21usafitLR, AR2usafitLR)
BIC(usafitLR, ARusafitLR, ARMAusafitLR, ARMA2usafitLR, ARMA21usafitLR, AR2usafitLR)

#we choose AR(2) for the information criteria

################TEST 

#TEST ETEROSCHEDASTICIT
bptest(AR2usafitLR, studentize = FALSE) #ACCETTO H0 -> Omoschedasticità

#ArchTest
library(FinTS)
archTestusa_cpi<- ArchTest(AR2usafitLR$residuals, lags=2, demean=FALSE)
archTestusa_cpi

#DISTRIBUZIONE F-STATISTIC
resettest(usa_cpi ~  usa_u + usa_cpi_1 + usa_cpi_2) #ACCETTO H0 -> Non ho bisogno di termini di y^2,...

#WALD
library(aod)
wald.test(Sigma = vcov(AR2usafitLR), b = coef(AR2usafitLR), Terms = 2:4)

#GRANGER TEST
library(lmtest)
grangertest(usa_cpi,usa_u, order=2) #inflation-> unemployment
grangertest(usa_u,usa_cpi, order=2) #unemployment non implica inflazione


#AUTOCORRELATION
x11()
dwtest(AR2usafitLR)
Box.test(AR2usafitLR$residuals, type = "Ljung-Box", lag = 6)
Box.test(AR2usafitLR$residuals, type = "Box-Pierce", lag = 6) 
acf(AR2usafitLR$residuals)