################################################################################
##### GERMANY: LONG RUN (INFLATION) ############################################
################################################################################

g_u_1 <- lag(g_u, -1, na.pad = TRUE) #last term the last part of vector is delete
g_cpi_1 <- lag(g_cpi, -1, na.pad = TRUE)

#MA(1)
gfitLR <- dynlm(g_cpi ~  g_u + g_u_1)
summary(gfitLR)

###################

#AR(1)
ARgfitLR <- dynlm(g_cpi ~  g_u + g_cpi_1)
summary(ARgfitLR)


###############

#ARMA(1,1)
ARMAgfitLR <- dynlm(g_cpi ~  g_u + g_cpi_1 + g_u_1)
summary(ARMAgfitLR)


##########

#ARMA(2,2)
g_cpi_2 <- lag(g_cpi, -2, na.pad = TRUE) 
g_u_2 <- lag(g_u, -2, na.pad = TRUE)

ARMA2gfitLR <- dynlm(g_cpi ~  g_u + g_cpi_1 + g_u_1 + g_u_2 + g_cpi_2)
summary(ARMA2gfitLR)

#ARMA(2,1)

ARMA21gfitLR <- dynlm(g_cpi ~  g_u + g_cpi_1 + g_u_1 + g_cpi_2)
summary(ARMA21gfitLR)

#AR(2)
AR2gfitLR <- dynlm(g_cpi ~  g_u + g_cpi_1 + g_cpi_2)
summary(AR2gfitLR)

AIC(gfitLR, ARgfitLR, ARMAgfitLR, ARMA2gfitLR, ARMA21gfitLR, AR2gfitLR)
BIC(gfitLR, ARgfitLR, ARMAgfitLR, ARMA2gfitLR, ARMA21gfitLR, AR2gfitLR)

#we choose ARMA(2,0) for the information criteria

################TEST 

#TEST ETEROSCHEDASTICITà
bptest(AR2gfitLR, studentize = FALSE)

#ARCH TEST

archTestLRg_cpi <- ArchTest(AR2gfitLR$residuals, lags = 2, demean = FALSE)
archTestLRg_cpi

#DISTRIBUZIONE F-STATISTIC
resettest(g_cpi ~  g_u + g_cpi_1 + g_cpi_2) 

#WALD

wald.test(Sigma = vcov(AR2gfitLR), b = coef(AR2gfitLR), Terms = 2:4)

#GRANGER-CAUSALITY

library(lmtest)

grangertest(g_cpi, g_u, order = 2) #inflation->unemployment
grangertest(g_u, g_cpi, order = 2) #unemployent -> inflation

#AUTOCORRELATION

dwtest(AR2gfitLR)
Box.test(AR2gfitLR$residuals, type = "Ljung-Box", lag = 6)
Box.test(AR2gfitLR$residuals, type = "Box-Pierce", lag = 6) 
x11()
acf(AR2gfitLR$residuals)