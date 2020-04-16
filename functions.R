
#creates plot values from coefficients
perfCoeff <- function(Te,Tc,C0,C1,C2,C3,C4,C5,C6,C7,C8,C9) {
  C0 + C1*Te + C2*Tc + C3*(Te^2) + C4*Tc*Te + C5*(Tc^2) + 
    C6*(Te^3) + C7*Tc*(Te^2) + C8*Te*(Tc^2) + C9*(Tc^3)
}


#makes coefficients
makeCoefficientsWithLM <- function(testedTeTemps, testedTcTemps, metricStoredValues){
  df <- data.frame(metric = metricStoredValues, C1 = testedTeTemps,
                   C2 = testedTcTemps, C3 = testedTeTemps^2,
                   C4 = testedTeTemps*testedTcTemps, C5 = testedTcTemps^2,
                   C6 = testedTeTemps^3, C7 = testedTcTemps*testedTeTemps^2,
                   C8 = testedTeTemps*testedTcTemps^2, C9 = testedTcTemps^3)
  
  mod = lm(metric~.,df)
  return(coef(mod))
}

#cooks distance
getCooks <- function(testedTeTemps, testedTcTemps, metricStoredValues){
  df <- data.frame(metric = metricStoredValues, C1 = testedTeTemps,
                   C2 = testedTcTemps, C3 = testedTeTemps^2,
                   C4 = testedTeTemps*testedTcTemps, C5 = testedTcTemps^2,
                   C6 = testedTeTemps^3, C7 = testedTcTemps*testedTeTemps^2,
                   C8 = testedTeTemps*testedTcTemps^2, C9 = testedTcTemps^3)
  
  mod = lm(metric~.,df)
  return(cooks.distance(mod))
}

#allows for row deletion within table
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

env_points <- function (evapInputs, condInputs) {
  # Returns the envelope boundary points
  x = evapInputs %>%
    str_split_fixed(pattern = ',',n = str_count(input$evapEnvTemps,',') +1) %>%
    as.numeric()
  y = condInputs %>%
    str_split_fixed(pattern = ',',n = str_count(input$condEnvTemps,',') +1) %>%
    as.numeric()
  ENV_Plot = as.data.frame(cbind(x,y))
  
  return (ENV_Plot) 
}

#calculate curve % shift
curveShift <- function(evap, cond, Ccoefs, Pcoefs, desiredEER){
  simCAP = mapply(perfCoeff,evap,cond,Ccoefs[1],Ccoefs[2],Ccoefs[3],Ccoefs[4],Ccoefs[5],
                  Ccoefs[6],Ccoefs[7],Ccoefs[8],Ccoefs[9],Ccoefs[10])
  simPOW = mapply(perfCoeff,evap,cond,Pcoefs[1],Pcoefs[2],Pcoefs[3],Pcoefs[4],Pcoefs[5],
                  Pcoefs[6],Pcoefs[7],Pcoefs[8],Pcoefs[9],Pcoefs[10])
  xcap = (simPOW*desiredEER)/simCAP
  xpow = (simCAP/desiredEER)/simPOW
  shiftList <- as.numeric(c(xcap,xpow))
  return(shiftList)
}






