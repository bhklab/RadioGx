.patternSearch <- function(guess, guess_residual, span, precision, step) {
  return(PharmacoGx-API-Design:::.patternSearch(guess = guess,
                                                guess_residual = guess_residual,
                                                span = span,
                                                precision = precision,
                                                step = step,
                                                f = .linearQuadratic))
}