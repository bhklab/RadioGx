#' computeD10: computes D10 given alpha, beta
#' @param alpha parameter in equation y = exp(-alpha * x - beta * x ^ 2)
#' @param beta parameter in equation y = exp(-alpha * x - beta * x ^ 2)
#' @export
#' computeD10()

computeD10 <- function(alpha, beta) {
  D <- computeDn(SF = 0.1,
                 alpha = alpha,
                 beta = beta,
                 SF_as_log = FALSE)
}