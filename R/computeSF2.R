#' Compute SF2
#'
#' @description This function computes the survival fraction after administering 2 units of radiation, given alpha and beta in the equation SF = exp(-alpha * D - beta * D ^ 2).
#' @param alpha parameter in equation y = exp(-alpha * x - beta * x ^ 2)
#' @param beta parameter in equation y = exp(-alpha * x - beta * x ^ 2)
#' @param SF_as_log should log10(SF2) be returned instead of SF2? Defaults to TRUE.
#' @example computeSF2(0.2, 0.1)
#' @export

computeSF2 <- function(alpha, beta, SF_as_log = TRUE) {
  
  CoreGx:::.sanitizeInput(pars = c(alpha, beta),
                          x_as_log = FALSE,
                          y_as_log = SF_as_log,
                          y_as_pct = FALSE,
                          trunc = FALSE,
                          verbose = FALSE)
  
  SF <- .linearQuadratic(D = 2,
                         alpha = alpha,
                         beta = beta,
                         SF_as_log = SF_as_log)

  if (!SF_as_log) {
    SF <- exp(SF)
  }

  return(SF)
}
