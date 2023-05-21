################################################################################
##### GERMANY: SHORT RUN (UNEMPLOYMENT) ########################################
################################################################################

#MA(1)
gufitSR <- dynlm(dg_u ~  dg_cpi + dg_cpi_1)
summary(gufitSR)

#AR(1)

ARgufitSR <- dynlm(dg_u ~  dg_cpi + dg_u_1)
summary(ARgufitSR)

#ARMA(1,1)

ARMAgufitSR <- dynlm(dg_u ~  dg_cpi + dg_u_1 + dg_cpi_1)
summary(ARMAgufitSR)

#ARMA(2,2)

ARMA2gufitSR <- dynlm(dg_u ~  dg_cpi + dg_u_1 + dg_cpi_1 + dg_u_2 + dg_cpi_2)
summary(ARMA2gufitSR)

#ARMA(2,1)

ARMA21gufitSR <- dynlm(dg_u ~  dg_cpi + dg_u_1 + dg_cpi_1 + dg_u_2)
summary(ARMA21gufitSR)

#AR(2)

AR2gufitSR <- dynlm(dg_u ~  dg_cpi + dg_u_1 + dg_u_2)
summary(AR2gufitSR)

AIC(gufitSR, ARgufitSR, ARMAgufitSR, ARMA2gufitSR, ARMA21gufitSR, AR2gufitSR)
BIC(gufitSR, ARgufitSR, ARMAgufitSR, ARMA2gufitSR, ARMA21gufitSR, AR2gufitSR)

#the best one is ARMA(1,1)

#TEST STATISTICI

#TEST ETEROSCHEDASTICITà
bptest(ARMAgufitSR, studentize = FALSE) 

#ARCH TEST

archTestSRdg_u <- ArchTest(ARMAgufitSR$residuals, lags = 1, demean = FALSE)
archTestSRdg_u

#DISTRIBUZIONE F-STATISTIC
resettest(dg_u ~  dg_cpi + dg_cpi_1 + dg_u_1) 

#WALD

wald.test(Sigma = vcov(ARMAgufitSR), b = coef(ARMAgufitSR), Terms = 2:4)

#AUTOCORRELATION

dwtest(ARMAgufitSR)
Box.test(ARMAgufitSR$residuals, type = "Ljung-Box", lag = 6)
Box.test(ARMAgufitSR$residuals, type = "Box-Pierce", lag = 6) 
x11()
acf(ARMAgufitSR$residuals)