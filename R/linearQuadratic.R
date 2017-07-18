.linearQuadratic <- function(D, alpha, beta, SF_as_log = TRUE) {
  SF <- -(alpha * D + beta * D ^ 2)

  if (!SF_as_log) {
    SF <- exp(SF)
  }

  return(SF)
}
