#' .residual: computes residual of point from fitted curve
#' @param x
#' @param y
#' @param n
#' @param pars
#' @param scale
#' @param family
#' @param trunc
#' .residual()

.residual <- function (x, y, n, pars, scale = 0.07, family = c("normal", "Cauchy"), trunc = FALSE) 
{
  return(CoreGx:::.residual(x = x, y = y, n = n, pars = pars, f = .linearQuadratic, scale = scale, family = family, trunc = trunc) *
           ifelse(pars[1] > pars[2], 1, Inf)) #ifelse takes care of requirement that alpha > beta
}