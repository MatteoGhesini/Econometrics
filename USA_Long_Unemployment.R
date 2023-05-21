################################################################################
##### USA: LONG RUN (UNEMPLOYMENT) #############################################
################################################################################

#MA(1)
usaufitLR <- dynlm(usa_u ~  usa_cpi + usa_cpi_1)
summary(usaufitLR)

#AR(1)
ARusaufitLR <- dynlm(usa_u ~  usa_cpi + usa_u_1)
summary(ARusaufitLR)

#AR(2)
AR2usaufitLR <- dynlm(usa_u ~  usa_cpi + usa_u_1 + usa_u_2)
summary(AR2usaufitLR)

#ARMA(1,1)
ARMAusaufitLR <- dynlm(usa_u ~  usa_cpi + usa_u_1 + usa_cpi_1)
summary(ARMAusaufitLR)

#ARMA(2,2)

ARMA2usaufitLR <- dynlm(usa_u ~  usa_cpi + usa_u_1 + usa_cpi_1 + usa_u_2 + usa_cpi_2)
summary(ARMA2usaufitLR)

#ARMA(2,1)

ARMA21usaufitLR <- dynlm(usa_u ~  usa_cpi + usa_u_1 + usa_cpi_1 + usa_u_2 )
summary(ARMA21usaufitLR)

#ARMA(1,2)

ARMA12usaufitLR <- dynlm(usa_u ~  usa_cpi + usa_u_1 + usa_cpi_1 + usa_cpi_2 )
summary(ARMA12usaufitLR)

AIC(usaufitLR, ARusaufitLR, ARMAusaufitLR, ARMA2usaufitLR, ARMA21usaufitLR, ARMA12usaufitLR, AR2usaufitLR)
BIC(usaufitLR, ARusaufitLR, ARMAusaufitLR, ARMA2usaufitLR, ARMA21usaufitLR, ARMA12usaufitLR, AR2usaufitLR)

#we choose ARMA(2,1) for the information criteria

################TEST 

#TEST ETEROSCHEDASTICITà
bptest(ARMA21usaufitLR, studentize = FALSE) #RIFUTO H0 -> Eteroschedasticità

#ARCH TEST
#library(FinTS)
archTestusa_u_LR<- ArchTest(ARMA21usaufitLR$residuals, lags=2, demean=FALSE)
archTestusa_u_LR

#DISTRIBUZIONE F-STATISTIC
resettest(usa_u ~  usa_cpi + usa_u_1 +usa_u_2+ usa_cpi_1 ) #ACCETTO H0 -> Non ho bisognon di termini di y^2,...

#WALD

wald.test(Sigma = vcov(ARMA21usaufitLR), b = coef(ARMA21usaufitLR), Terms = 2:5)

#AUTOCORRELATION

X11()
dwtest(ARMA21usaufitLR)
Box.test(ARMA21usaufitLR$residuals, type = "Ljung-Box", lag = 6)
Box.test(ARMA21usaufitLR$residuals, type = "Box-Pierce", lag = 6) 
acf(ARMA21usaufitLR$residuals)