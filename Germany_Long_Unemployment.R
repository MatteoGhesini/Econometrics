################################################################################
##### GERMANY: LONG RUN (UNEMPLOYMENT) #########################################
################################################################################

#MA(1)
gufitLR <- dynlm(g_u ~  g_cpi + g_cpi_1)
summary(gufitLR)

#AR(1)
ARgufitLR <- dynlm(g_u ~  g_cpi + g_u_1)
summary(ARgufitLR)

#ARMA(1,1)
ARMAgufitLR <- dynlm(g_u ~  g_cpi + g_u_1 + g_cpi_1)
summary(ARMAgufitLR)

#ARMA(2,2)

ARMA2gufitLR <- dynlm(g_u ~  g_cpi + g_u_1 + g_cpi_1 + g_u_2 + g_cpi_2)
summary(ARMA2gufitLR)

#ARMA(2,1)

ARMA21gufitLR <- dynlm(g_u ~  g_cpi + g_u_1 + g_cpi_1 + g_u_2 )
summary(ARMA21gufitLR)

#ARMA(1,2)

ARMA12gufitLR <- dynlm(g_u ~  g_cpi + g_u_1 + g_cpi_1 + g_cpi_2 )
summary(ARMA12gufitLR)

#AR(2)

AR2gufitLR <- dynlm(g_u ~  g_cpi + g_cpi_1 + g_cpi_2 )
summary(ARMA12gufitLR)

AIC(gufitLR, ARgufitLR, ARMAgufitLR, ARMA2gufitLR, ARMA21gufitLR, ARMA12gufitLR, AR2gufitLR)
BIC(gufitLR, ARgufitLR, ARMAgufitLR, ARMA2gufitLR, ARMA21gufitLR, ARMA12gufitLR, AR2gufitLR)

#we choose ARMA(2,1) for the information criteria

################TEST 

#TEST ETEROSCHEDASTICITà
bptest(ARMA21gufitLR, studentize = FALSE) 

#ARCH TEST VISTO CHE ABBIAMO RIFUTATO IL BP

archTestLRg_u <- ArchTest(ARMA21gufitLR$residuals, lags = 2, demean = FALSE)
archTestLRg_u

#DISTRIBUZIONE F-STATISTIC
resettest(g_u ~  g_cpi + g_u_1 + g_cpi_1 + g_u_2) 

#WALD

wald.test(Sigma = vcov(ARMA21gufitLR), b = coef(ARMA21gufitLR), Terms = 2:5)

#AUTOCORRELATION

dwtest(ARMA21gufitLR)
Box.test(ARMA21gufitLR$residuals, type = "Ljung-Box", lag = 6)
Box.test(ARMA21gufitLR$residuals, type = "Box-Pierce", lag = 6) 
x11()
acf(ARMA21gufitLR$residuals)