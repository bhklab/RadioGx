#' MeshEval: Evaluates function on a lattice for curve fitting
#' @param x
#' @param y
#' @param f
#' @param guess
#' @param lower_bounds
#' @param upper_bounds
#' @param density
#' @param n
#' @param family
#' @param trunc
#' .meshEval()

.meshEval <- function (x, y, f, guess, lower_bounds, upper_bounds, density, n, family, trunc) {
  return(CoreGx:::.meshEval(x = x,
                            y = y,
                            f = f,
                            guess = guess,
                            lower_bounds = lower_bounds,
                            upper_bounds = upper_bounds,
                            density = density,
                            n = n,
                            family = family,
                            trunc = trunc))
}