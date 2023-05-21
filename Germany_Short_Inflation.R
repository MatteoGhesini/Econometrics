################################################################################
##### GERMANY: SHORT RUN (INFLATION) ###########################################
################################################################################

dg_cpi_1 <- lag(dg_cpi, -1, na.pad = TRUE)
dg_u_1 <- lag(dg_u, -1, na.pad = TRUE)

#MA(1)
gfitSR <- dynlm(dg_cpi ~  dg_u + dg_u_1)
summary(gfitSR)

#AR(1)
ARgfitSR <- dynlm(dg_cpi ~  dg_u + dg_cpi_1)
summary(ARgfitSR)

###############

#ARMA(1,1)
ARMAgfitSR <- dynlm(dg_cpi ~  dg_u + dg_cpi_1 + dg_u_1)
summary(ARMAgfitSR)

#ARMA(2,2)
dg_cpi_2 <- lag(dg_cpi, -2, na.pad = TRUE) 
dg_u_2 <- lag(dg_u, -2, na.pad = TRUE)

ARMA2gfitSR <- dynlm(dg_cpi ~  dg_u + dg_cpi_1 + dg_u_1+ dg_cpi_2 + dg_u_2)
summary(ARMA2gfitSR)

#ARMA(2,1)
ARMA21gfitSR <- dynlm(dg_cpi ~  dg_u + dg_cpi_1 + dg_u_1+ dg_cpi_2)
summary(ARMA21gfitSR)

#AR(2)
AR2gfitSR <- dynlm(dg_cpi ~  dg_u + dg_cpi_1 + dg_cpi_2)
summary(AR2gfitSR)

AIC(gfitSR, ARgfitSR, ARMAgfitSR, ARMA2gfitSR, ARMA21gfitSR, AR2gfitSR)
BIC(gfitSR, ARgfitSR, ARMAgfitSR, ARMA2gfitSR, ARMA21gfitSR, AR2gfitSR)

#we choose ARMA(2,0)

#TEST ETEROSCHEDASTICITà
bptest(AR2gfitSR, studentize = FALSE) 

#ARCH TEST VISTO CHE ABBIAMO RIFUTATO IL BP

archTestSRdg_cpi <- ArchTest(AR2gfitSR$residuals, lags = 2, demean = FALSE)
archTestSRdg_cpi

#DISTRIBUZIONE F-STATISTIC
resettest(dg_cpi ~  dg_u + dg_cpi_1 + dg_cpi_2) 

#WALD

wald.test(Sigma = vcov(AR2gfitSR), b = coef(AR2gfitSR), Terms = 2:4)

#AUTOCORRELATION

dwtest(AR2gfitSR)
Box.test(AR2gfitSR$residuals, type = "Ljung-Box", lag = 6)
Box.test(AR2gfitSR$residuals, type = "Box-Pierce", lag = 6) 
x11()
acf(AR2gfitSR$residuals)