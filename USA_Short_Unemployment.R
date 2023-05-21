################################################################################
##### USA: SHORT RUN (UNEMPLOYMENT) ############################################
################################################################################

#MA(1)
usaufitSR <- dynlm(dusa_u ~  dusa_cpi + dusa_cpi_1)
summary(usaufitSR)

#AR(1)

ARusaufitSR <- dynlm(dusa_u ~  dusa_cpi + dusa_u_1)
summary(ARusaufitSR)

#ARMA(1,1)

ARMAusaufitSR <- dynlm(dusa_u ~  dusa_cpi + dusa_u_1 + dusa_cpi_1)
summary(ARMAusaufitSR)

#ARMA(2,2)

ARMA2usaufitSR <- dynlm(dusa_u ~  dusa_cpi + dusa_u_1 + dusa_cpi_1 + dusa_u_2 + dusa_cpi_2)
summary(ARMA2usaufitSR)

#ARMA(2,1)

ARMA21usaufitSR <- dynlm(dusa_u ~  dusa_cpi + dusa_u_1 + dusa_cpi_1 + dusa_u_2)
summary(ARMA21usaufitSR)


#ARMA(1,2)
ARMA12usaufitSR <- dynlm(dusa_u ~  dusa_u_1 + dusa_cpi + dusa_cpi_1 + dusa_cpi_2)
summary(ARMA12usaufitSR)

#AR(2)
AR2usaufitSR <- dynlm(dusa_u ~  dusa_cpi + dusa_u_1 + dusa_u_2)
summary(AR2usaufitSR)

#AR(3)
dusa_cpi_3 <- lag(dusa_cpi, -3, na.pad = TRUE) 
dusa_u_3 <- lag(dusa_u, -3, na.pad = TRUE)
AR3usaufitSR <- dynlm(dusa_u ~  dusa_cpi + dusa_u_1 + dusa_u_2+ dusa_u_3)
summary(AR3usaufitSR)

#AIC & BIC
AIC(usaufitSR, ARusaufitSR, ARMAusaufitSR, ARMA2usaufitSR, ARMA21usaufitSR, AR2usaufitSR, AR3usaufitSR)
BIC(usaufitSR, ARusaufitSR, ARMAusaufitSR, ARMA2usaufitSR, ARMA21usaufitSR, AR2usaufitSR, AR3usaufitSR)

#the best one is AR(3)

#TEST STATISTICI

#TEST ETEROSCHEDASTICITà
bptest(AR3usaufitSR, studentize = FALSE) #ACCETTO H0 -> Omoschedasticità

#ARCH TEST
#library(FinTS)
archTestusa_u<- ArchTest(AR3usaufitSR$residuals, lags=1, demean=FALSE)
archTestusa_u

#DISTRIBUZIONE F-STATISTIC
resettest(dusa_u ~  dusa_cpi + dusa_u_1+ dusa_u_2+ dusa_u_3) 

#WALD

wald.test(Sigma = vcov(AR3usaufitSR), b = coef(AR3usaufitSR), Terms = 2:5)

#AUTOCORRELATION
x11()
dwtest(AR3usaufitSR)
Box.test(AR3usaufitSR$residuals, type = "Ljung-Box", lag = 6)
Box.test(AR3usaufitSR$residuals, type = "Box-Pierce", lag = 6) 
ACF_SR_USA_Unempl <-AR3usaufitSR$residuals
acf(ACF_SR_USA_Unempl)