
# We create a dummy variable: 1 is for the recession, 0 no
{
  usafitmodel <- dynlm(usa_cpi ~ usa_u + usa_cpi_1 + usa_cpi_2+ shockusa)
  summary(usafitmodel)
}
{
  dusafitmodel <- dynlm(dusa_cpi ~  dusa_u + dusa_cpi_1 + dusa_cpi_2+ shockusa)
  summary(dusafitmodel)
}

################################################################################
##### HETEROSKEDASTICITY TEST ##################################################
################################################################################

bptest(usafitmodel, studentize = FALSE)
bptest(dusafitmodel, studentize = FALSE) 

################################################################################
##### ARCH TEST ################################################################
################################################################################

archTestusa_u<- ArchTest(usafitmodel$residuals, lags=2, demean=FALSE)
archTestusa_u

archTestusa_du<- ArchTest(dusafitmodel$residuals, lags=2, demean=FALSE)
archTestusa_du

dshockusa <- shockusa[-1]

################################################################################
##### F-STATISTIC DISTRIBUTION TEST ############################################
################################################################################

resettest(usa_cpi ~  usa_u + usa_u_1 +usa_cpi_1 + shockusa) 
resettest(dusa_cpi ~  dusa_u + dusa_u_1 +dusa_cpi_1 + shockusa[-1]) 

################################################################################
##### WALD TEST ################################################################
################################################################################

wald.test(Sigma = vcov(usafitmodel), b = coef(dusafitmodel), Terms = 2:5)
wald.test(Sigma = vcov(dusafitmodel), b = coef(dusafitmodel), Terms = 2:5)

################################################################################
##### DURBIN-WATSON TEST #######################################################
################################################################################

{
  x11()
  dwtest(usafitmodel)
  acf(usafitmodel$residuals)
}

{
  x11()
  dwtest(dusafitmodel)
  acf(dusafitmodel$residuals)
}


