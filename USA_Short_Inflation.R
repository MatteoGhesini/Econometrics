################################################################################
##### USA: SHORT RUN (INFLATION) ###############################################
################################################################################

dusa_cpi_1 <- lag(dusa_cpi, -1, na.pad = TRUE)
dusa_u_1 <- lag(dusa_u, -1, na.pad = TRUE)

#MA(1)
usafitSR <- dynlm(dusa_cpi ~  dusa_u + dusa_u_1)
summary(usafitSR)

#AR(1)
ARusafitSR <- dynlm(dusa_cpi ~  dusa_u + dusa_cpi_1)
summary(ARusafitSR)

###############

#ARMA(1,1)
ARMAusafitSR <- dynlm(dusa_cpi ~  dusa_u + dusa_cpi_1 + dusa_u_1)
summary(ARMAusafitSR)

#ARMA(2,2)
dusa_cpi_2 <- lag(dusa_cpi, -2, na.pad = TRUE) 
dusa_u_2 <- lag(dusa_u, -2, na.pad = TRUE)

ARMA2usafitSR <- dynlm(dusa_cpi ~  dusa_u + dusa_cpi_1 + dusa_u_1+ dusa_cpi_2 + dusa_u_2)
summary(ARMA2usafitSR)

#ARMA(2,1)
ARMA21usafitSR <- dynlm(dusa_cpi ~  dusa_u + dusa_cpi_1 + dusa_u_1+ dusa_cpi_2)
summary(ARMA21usafitSR)

#ARMA(1,2)
ARMA12usafitSR <- dynlm(dusa_cpi ~  dusa_u + dusa_cpi_1 + dusa_u_1+ dusa_u_2)
summary(ARMA12usafitSR)

#AR(2)
AR2usafitSR <- dynlm(dusa_cpi ~  dusa_u + dusa_cpi_1 + dusa_cpi_2)
summary(AR2usafitSR)

#AR(3)
dusa_cpi_3 <- lag(dusa_cpi, -3, na.pad = TRUE) 
dusa_u_3 <- lag(dusa_u, -3, na.pad = TRUE)
AR3usafitSR <- dynlm(dusa_cpi ~  dusa_u + dusa_cpi_1 + dusa_cpi_2+ dusa_cpi_3)
summary(AR3usafitSR)


#AIC & BIC

AIC(usafitSR, ARusafitSR, ARMAusafitSR, ARMA2usafitSR, ARMA21usafitSR, AR2usafitSR, AR3usafitSR)
BIC(usafitSR, ARusafitSR, ARMAusafitSR, ARMA2usafitSR, ARMA21usafitSR, AR2usafitSR, AR3usafitSR)

#we choose AR(3)

#TEST ETEROSCHEDASTICITà
bptest(AR3usafitSR, studentize = FALSE) #ACCETTO H0 -> Omoschedasticità

#ARCH TEST
#library(FinTS)
archTestusa_cpi_SR<- ArchTest(AR3usafitSR$residuals, lags=3, demean=FALSE)
archTestusa_cpi_SR

#DISTRIBUZIONE F-STATISTIC
resettest(dusa_cpi ~  dusa_u + dusa_cpi_1 + dusa_cpi_2+ dusa_cpi_3) #ACCETTO H0 -> Non ho bisognon di termini di y^2,...

#WALD

wald.test(Sigma = vcov(AR3usafitSR), b = coef(AR3usafitSR), Terms = 2:5)

#AUTOCORRELATION

x11()
dwtest(AR3usafitSR)
Box.test(AR3usafitSR$residuals, type = "Ljung-Box", lag = 6)
Box.test(AR3usafitSR$residuals, type = "Box-Pierce", lag = 6)
acf(AR3usafitSR$residuals)