#' PatternSearch: Robust optimization algorithm for curve fitting of poor data
#' @param guess
#' @param guess_residual
#' @param span
#' @param precision
#' @param step
#' .patternSearch()

.patternSearch <- function(guess, guess_residual, span, precision, step) {
  return(CoreGx:::.patternSearch(guess = guess,
                                 guess_residual = guess_residual,
                                 span = span,
                                 precision = precision,
                                 step = step,
                                 f = .linearQuadratic))
}