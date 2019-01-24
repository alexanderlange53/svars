# F test for weak instruments from Lunsford 2016
#
# H_0: Instrument is weak
#
# Threshold ist test staistic of 9.06

weak_instrument_test <- function(Tob, u, k, instruments){

  Pi <- solve(crossprod(u)/Tob) %*% (crossprod(u, instruments)/Tob)

  F_stat <- ((Tob - k)/k) * (crossprod(instruments) - crossprod((instruments - u %*% Pi))) /  crossprod((instruments - u %*% Pi))

  return(F_stat)
}
