linearQuadraticModel <- function (D,
                                  SF,
                                  density = c(100, 100),
                                  step = 0.5 / density,
                                  precision = 0.005,
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
  
  dFromD2 <- abs(D - 2)
  dFromSF10 <- abs(exp(SF) - 0.1)
  D10Ind <- which.min(dFromSF10)[1]
  SF2Ind <- which.min(dFromD2)[1]
  
  if (D10Ind == SF2Ind) {
    dFromD2[SF2Ind] <- dFromD2[SF2Ind] + dFromD2[1] #make it not the smallest anymore
    SF2Ind <- which.min(dFromD2)[1]
  }
  
  DSF2 <- D[SF2Ind]
  SF2 <- SF[SF2Ind]
  D10 <- D[D10Ind]
  SFD10 <- SF[D10Ind]
  
  gritty_guess <- pmin(pmax(c((SF[D10Ind] * D[SF2Ind] ^ 2 - SF[SF2Ind] * D[D10Ind] ^ 2) / D[SF2Ind] / D[D10Ind] / (D[D10Ind] - D[SF2Ind]),
                              (SF[D10Ind] * D[SF2Ind] - SF[SF2Ind] * D[D10Ind]) / D[SF2Ind] / D[D10Ind] / (D[SF2Ind] - D[D10Ind])), lower_bounds), upper_bounds) # assumes the SF2Indth point is SF2 and D10Indth point is D10 and imputes alpha, beta from that assumption unless either would thus be out of bounds
  
  guess <- tryCatch(optim(par = gritty_guess,
                          fn = function(x) {.residual(D,
                                                      SF,
                                                      pars = x,
                                                      n = median_n,
                                                      scale = scale,
                                                      family = family,
                                                      trunc = trunc)},
                          lower = lower_bounds,
                          upper = upper_bounds,
                          method = "L-BFGS-B"),
                    error = function(e) {list(par = gritty_guess, convergence = -1)})
  failed = guess[["convergence"]] != 0
  guess <- guess[["par"]]
  
  guess_residual <- .residual(D,
                              SF,
                              pars = guess, 
                              n = median_n,
                              scale = scale,
                              family = family,
                              trunc = trunc)
  gritty_guess_residual <- .residual(log_conc,
                                     viability,
                                     pars = gritty_guess, 
                                     n = median_n,
                                     scale = scale,
                                     family = family,
                                     trunc = trunc)
  
  if (failed || any(is.na(guess)) || guess_residual >= gritty_guess_residual) {
    sieve_guess <- .meshEval(D,
                             SF,
                             lower_bounds = lower_bounds, 
                             upper_bounds = upper_bounds,
                             density = density, 
                             n = median_n,
                             scale = scale,
                             family = family,
                             trunc = trunc)
    sieve_guess_residual <- .residual(D,
                                      SF, 
                                      pars = sieve_guess,
                                      n = median_n,
                                      scale = scale, 
                                      family = family,
                                      trunc = trunc)
    guess <- sieve_guess
    guess_residual <- sieve_guess_residual
    
    guess <- .patternSearch(guess = sieve_guess,
                            guess_residual = sieve_guess_residual,
                            span = 0.1,
                            precision = precision,
                            step = step,
                            f = f)
  }
  
  return(list(alpha = guess[1], beta = guess[2]))
}