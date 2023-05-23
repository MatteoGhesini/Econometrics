
dusa_cpi_1 <- lag(dusa_cpi, -1, na.pad = TRUE)
dusa_u_1 <- lag(dusa_u, -1, na.pad = TRUE)

################################################################################
##### MOVING AVERAGE (1) MODEL #################################################
################################################################################

{
  usafitSR <- dynlm(dusa_cpi ~  dusa_u + dusa_u_1)
  summary(usafitSR)
}

################################################################################
##### AUTOREGRESSIVE (1) MODEL #################################################
################################################################################

{
  ARusafitSR <- dynlm(dusa_cpi ~  dusa_u + dusa_cpi_1)
  summary(ARusafitSR)
}

################################################################################
##### AUTOREGRESSIVE MOVING AVERAGE (1,1) MODEL ################################
################################################################################

{
  ARMAusafitSR <- dynlm(dusa_cpi ~  dusa_u + dusa_cpi_1 + dusa_u_1)
  summary(ARMAusafitSR)
}

################################################################################
##### AUTOREGRESSIVE MOVING AVERAGE (2,2) MODEL ################################
################################################################################

dusa_cpi_2 <- lag(dusa_cpi, -2, na.pad = TRUE) 
dusa_u_2 <- lag(dusa_u, -2, na.pad = TRUE)

{
  ARMA2usafitSR <- dynlm(dusa_cpi ~  dusa_u + dusa_cpi_1 + dusa_u_1+ dusa_cpi_2 + dusa_u_2)
  summary(ARMA2usafitSR)
}

################################################################################
##### AUTOREGRESSIVE MOVING AVERAGE (2,1) MODEL ################################
################################################################################

{
  ARMA21usafitSR <- dynlm(dusa_cpi ~  dusa_u + dusa_cpi_1 + dusa_u_1+ dusa_cpi_2)
  summary(ARMA21usafitSR)
}

################################################################################
##### AUTOREGRESSIVE MOVING AVERAGE (1,2) MODEL ################################
################################################################################

{
  ARMA12usafitSR <- dynlm(dusa_cpi ~  dusa_u + dusa_cpi_1 + dusa_u_1+ dusa_u_2)
  summary(ARMA12usafitSR)
}

################################################################################
##### AUTOREGRESSIVE (2) MODEL #################################################
################################################################################

{
  AR2usafitSR <- dynlm(dusa_cpi ~  dusa_u + dusa_cpi_1 + dusa_cpi_2)
  summary(AR2usafitSR)
}

################################################################################
##### AUTOREGRESSIVE (3) MODEL #################################################
################################################################################

dusa_cpi_3 <- lag(dusa_cpi, -3, na.pad = TRUE) 
dusa_u_3 <- lag(dusa_u, -3, na.pad = TRUE)

{
  AR3usafitSR <- dynlm(dusa_cpi ~  dusa_u + dusa_cpi_1 + dusa_cpi_2+ dusa_cpi_3)
  summary(AR3usafitSR)
}

################################################################################
##### SELECTING BEST MODEL #####################################################
################################################################################

AIC(usafitSR, ARusafitSR, ARMAusafitSR)
AIC(ARMA2usafitSR, ARMA21usafitSR, AR2usafitSR)
AIC(AR3usafitSR)
BIC(usafitSR, ARusafitSR, ARMAusafitSR)
BIC(ARMA2usafitSR, ARMA21usafitSR, AR2usafitSR)
BIC(AR3usafitSR)

#we choose AR(3)

################################################################################
##### HETEROSKEDASTICITY TEST ##################################################
################################################################################

bptest(AR3usafitSR, studentize = FALSE) #ACCETTO H0 -> Omoschedasticità

################################################################################
##### ARCH TEST ################################################################
################################################################################

archTestusa_cpi_SR<- ArchTest(AR3usafitSR$residuals, lags=3, demean=FALSE)
archTestusa_cpi_SR

################################################################################
##### F-STATISTIC DISTRIBUTION TEST ############################################
################################################################################

resettest(dusa_cpi ~  dusa_u + dusa_cpi_1 + dusa_cpi_2+ dusa_cpi_3) #ACCETTO H0 -> Non ho bisognon di termini di y^2,...

################################################################################
##### WALD TEST ################################################################
################################################################################

wald.test(Sigma = vcov(AR3usafitSR), b = coef(AR3usafitSR), Terms = 2:5)

################################################################################
##### DURBIN-WATSON TEST #######################################################
################################################################################

dwtest(AR3usafitSR)
Box.test(AR3usafitSR$residuals, type = "Ljung-Box", lag = 6)
Box.test(AR3usafitSR$residuals, type = "Box-Pierce", lag = 6)

{
  x11()
  acf(AR3usafitSR$residuals)
}


