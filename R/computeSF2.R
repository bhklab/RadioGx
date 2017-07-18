#' computeSF2: computes SF2 given alpha, beta
#' @param alpha parameter in equation y = exp(-alpha * x - beta * x ^ 2)
#' @param beta parameter in equation y = exp(-alpha * x - beta * x ^ 2)
#' @param SF_as_log should log10(SF2) be returned instead of SF2? Defaults to TRUE.
#' @example computeSF2(0.2, 0.1)
#' @export
#' computeSF2()

computeSF2 <- function(alpha, beta, SF_as_log = TRUE) {
  SF <- .linearQuadratic(D = 2,
                         alpha = alpha,
                         beta = beta,
                         SF_as_log = SF_as_log)
  
  if (!SF_as_log) {
    SF <- exp(SF)
  }
  
  return(SF)
}