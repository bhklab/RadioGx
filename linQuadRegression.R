linearQuadraticModel <- function (D,
                                  SF,
                                  lower_bounds = c(0, 0),
                                  upper_bounds = c(0.3, 0.3),
                                  scale = 0.07,
                                  family = c("normal", "Cauchy"),
                                  median_n = 1,
                                  SF_as_log = TRUE,
                                  trunc = FALSE,
                                  verbose = FALSE) {
  match.arg(family)
  
  if (!SF_as_log) {
    SF <- log(SF)
  }
  
  if (trunc) {
    SF[which(SF > 0)] <- 0
  }
  
  gritty_guess <- .makeGrittyGuess(lower_bounds = lower_bounds,
                                   upper_bounds = upper_bounds,
                                   D = D,
                                   SF = SF)
  
  guess <- CoreGx:::.fitCurve(x = D,
                              y = SF,
                              f = RadioGx:::.linearQuadratic,
                              density = c(100, 100),
                              step = c(0.005, 0.005),
                              precision = 0.005,
                              lower_bounds = lower_bounds,
                              upper_bounds = upper_bounds,
                              scale = scale,
                              family = family,
                              median_n = median_n,
                              trunc = trunc,
                              verbose = verbose,
                              gritty_guess = gritty_guess,
                              span = 0.1)
  
  return(list(alpha = guess[1], beta = guess[2]))
}