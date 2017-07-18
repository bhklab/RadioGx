#' .linearQuadratic: computes response to radiation dose
#' @param D
#' @param alpha
#' @param beta
#' @param SF_as_log
#' .linearQuadratic()

.linearQuadratic <- function(D, alpha, beta, SF_as_log = TRUE) {
  SF <- -(alpha * D + beta * D ^ 2)
  
  if (!SF_as_log) {
    SF <- exp(SF)
  }
  
  return(SF)
}