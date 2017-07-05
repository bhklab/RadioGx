linQuadRegression <- function (D,
                               SF,
                               density = c(100, 100),
                               step = 0.5 / density,
                               precision = 0.05,
                               lower_bounds = c(0, 0),
                               upper_bounds = c(0.3, 0.3),
                               scale = 0.07,
                               family = c("normal", "Cauchy"),
                               median_n = 1,
                               SF_as_log = TRUE,
                               trunc = TRUE,
                               verbose = FALSE) {
  
  if (!SF_as_log) {
    SF <- log(SF)
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
  
  gritty_guess <- c((SF[D10Ind] * D[SF2Ind] ^ 2 - SF[SF2Ind] * D[D10Ind] ^ 2) / D[SF2Ind] / D[D10Ind] / (D[D10Ind] - D[SF2Ind]),
                    (SF[D10Ind] * D[SF2Ind] - SF[SF2Ind] * D[D10Ind]) / D[SF2Ind] / D[D10Ind] / (D[SF2Ind] - D[D10Ind])) # assumes the SF2Indth point is SF2 and D10Indth point is D10 and imputes alpha, beta from that assumption
  
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
    
    span <- 1
    while (span > precision) {
      neighbours <- rbind(guess, guess, guess, guess)
      neighbour_residuals <- matrix(NA, nrow = 1, ncol = length(neighbours))
      neighbours[1, 1] <- pmin(neighbours[1, 1] + span * step[1], upper_bounds[1])
      neighbours[2, 1] <- pmax(neighbours[2, 1] - span * step[1], lower_bounds[1])
      neighbours[3, 2] <- pmin(neighbours[3, 2] + span * step[2], upper_bounds[2])
      neighbours[4, 2] <- pmax(neighbours[4, 2] - span * step[2], lower_bounds[2])

      for (i in 1:nrow(neighbours)) {
        neighbour_residuals[i] <- .residual(D, 
                                            SF,
                                            pars = neighbours[i, ],
                                            n = median_n, 
                                            scale = scale,
                                            family = family,
                                            trunc = trunc)
      }
      if (min(neighbour_residuals) < guess_residual) {
        guess <- neighbours[which.min(neighbour_residuals)[1], ]
        guess_residual <- min(neighbour_residuals)[1]
      }
      else {
        span <- span / 2
      }
    }
  }
  return(list(alpha = guess[1], beta = guess[2]))
}