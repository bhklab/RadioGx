#' Compute SF2
#'
#' @description This function computes the survival fraction after administering 2 units of radiation, given alpha and beta in the equation SF = exp(-alpha * D - beta * D ^ 2).
#' @param pars parameters (alpha, beta) in equation y = exp(-alpha * x - beta * x ^ 2)
#' @param SF_as_log should log10(SF2) be returned instead of SF2? Defaults to TRUE.
#' @example computeSF2(0.2, 0.1)
#' @export

computeSF2 <- function(pars) {
  
  CoreGx:::.sanitizeInput(pars = pars,
                          x_as_log = FALSE,
                          y_as_log = FALSE,
                          y_as_pct = FALSE,
                          trunc = FALSE,
                          verbose = FALSE)
  
  SF <- .linearQuadratic(D = 2,
                         pars = pars,
                         SF_as_log = FALSE)

  # if (!SF_as_log) {
    SF <- exp(SF)
  # }

  return(SF)
}
