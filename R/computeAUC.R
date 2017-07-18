#' computeAUC: computes AUC
#' @param alpha parameter in equation y = exp(-alpha * x - beta * x ^ 2)
#' @param beta parameter in equation y = exp(-alpha * x - beta * x ^ 2)
#' @param lower lower bound of dose region to compute AUC over
#' @param upper upper bound of dose region to compute AUC over
#' @param SF_as_log should AUC be computed with log10(survival fraction) instead of survival fraction? Defaults to FALSE.
#' @export
#' computeAUC()

computeAUC <- function(alpha, beta, lower, upper, SF_as_log = TRUE) {
  if (SF_as_log) {
    return(alpha / 2 * (lower ^ 2 - upper ^ 2) + beta / 3 * (lower ^ 3 - upper ^ 3))
  } else {
    if (beta == 0) {
      if (alpha == 0) {
        return(upper - lower)
      } else {
        return((exp(-alpha * lower) - exp(-alpha * upper)) / alpha)
      }
    } else {
      return(exp(alpha ^ 2 / 4 / beta) * sqrt(pi / beta) * (pnorm(sqrt(2 * beta) * (upper + alpha / 2 / beta)) -
                                                              pnorm(sqrt(2 * beta) * (lower + alpha / 2 / beta))))
    }
  }
}