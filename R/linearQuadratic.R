.linearQuadratic <- function(D, pars, SF_as_log = TRUE) {
  
  SF <- -(pars[[1]] * D + pars[[2]] * D ^ 2)

  if (!SF_as_log) {
    SF <- exp(SF)
  }

  return(SF)
}
