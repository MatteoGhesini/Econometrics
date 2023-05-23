
usa_u_1 <- lag(usa_u, -1, na.pad = TRUE) #last term the last part of vector is deleted
usa_cpi_1 <- lag(usa_cpi, -1, na.pad = TRUE)

################################################################################
##### MOVING AVERAGE (1) MODEL #################################################
################################################################################
{
  usafitLR <- dynlm(usa_cpi ~  usa_u + usa_u_1)
  summary(usafitLR)
}

################################################################################
##### AUTOREGRESSIVE (1) MODEL #################################################
################################################################################

{
  ARusafitLR <- dynlm(usa_cpi ~  usa_u + usa_cpi_1)
  summary(ARusafitLR)
}

################################################################################
##### AUTOREGRESSIVE MOVING AVERAGE (1,1) MODEL ################################
################################################################################

{
  ARMAusafitLR <- dynlm(usa_cpi ~  usa_u + usa_cpi_1 + usa_u_1)
  summary(ARMAusafitLR)
}

################################################################################
##### AUTOREGRESSIVE MOVING AVERAGE (2,2) MODEL ################################
################################################################################

usa_cpi_2 <- lag(usa_cpi, -2, na.pad = TRUE) 
usa_u_2 <- lag(usa_u, -2, na.pad = TRUE)

{
  ARMA2usafitLR <- dynlm(usa_cpi ~  usa_u + usa_cpi_1 + usa_u_1 + usa_u_2 + usa_cpi_2)
  summary(ARMA2usafitLR)
}

################################################################################
##### AUTOREGRESSIVE MOVING AVERAGE (2,1) MODEL ################################
################################################################################

{
  ARMA21usafitLR <- dynlm(usa_cpi ~  usa_u + usa_cpi_1 + usa_u_1 + usa_cpi_2)
  summary(ARMA21usafitLR)
}

################################################################################
##### AUTOREGRESSIVE (2) MODEL #################################################
################################################################################

{
  AR2usafitLR <- dynlm(usa_cpi ~  usa_u + usa_cpi_1 + usa_cpi_2)
  summary(AR2usafitLR)
}

################################################################################
##### SELECTING BEST MODEL #####################################################
################################################################################

AIC(usafitLR, ARusafitLR, ARMAusafitLR)
AIC(ARMA2usafitLR, ARMA21usafitLR, AR2usafitLR)
BIC(usafitLR, ARusafitLR, ARMAusafitLR)
BIC(ARMA2usafitLR, ARMA21usafitLR, AR2usafitLR)

#we choose AR(2) for the information criteria

################################################################################
##### HETEROSKEDASTICITY TEST ##################################################
################################################################################

bptest(AR2usafitLR, studentize = FALSE) #ACCETTO H0 -> Omoschedasticità

################################################################################
##### ARCH TEST ################################################################
################################################################################

library(FinTS)
archTestusa_cpi<- ArchTest(AR2usafitLR$residuals, lags=2, demean=FALSE)
archTestusa_cpi

################################################################################
##### F-STATISTIC DISTRIBUTION TEST ############################################
################################################################################

resettest(usa_cpi ~  usa_u + usa_cpi_1 + usa_cpi_2) #ACCETTO H0 -> Non ho bisogno di termini di y^2,...

################################################################################
##### WALD TEST ################################################################
################################################################################

library(aod)
wald.test(Sigma = vcov(AR2usafitLR), b = coef(AR2usafitLR), Terms = 2:4)

################################################################################
##### GRANGER-CAUSALITY TEST ###################################################
################################################################################

library(lmtest)
grangertest(usa_cpi,usa_u, order=2) #inflation-> unemployment
grangertest(usa_u,usa_cpi, order=2) #unemployment non implica inflazione

################################################################################
##### DURBIN-WATSON TEST #######################################################
################################################################################

dwtest(AR2usafitLR)
Box.test(AR2usafitLR$residuals, type = "Ljung-Box", lag = 6)
Box.test(AR2usafitLR$residuals, type = "Box-Pierce", lag = 6) 
{
  x11()
  acf(AR2usafitLR$residuals)
}


