.linearQuadratic <- function(D, pars, SF_as_log = TRUE) {
  CoreGx:::.sanitizeInput(x = D,
                          pars = pars,
                          x_as_log = FALSE,
                          y_as_log = SF_as_log,
                          y_as_pct = FALSE,
                          trunc = FALSE,
                          verbose = FALSE)
  
  SF <- -(pars[[1]] * D + pars[[2]] * D ^ 2)

  if (!SF_as_log) {
    SF <- exp(SF)
  }

  return(SF)
}
