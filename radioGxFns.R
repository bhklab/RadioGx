computeAUC <- function(alpha, beta, lower, upper, SF_as_log = TRUE) { #need to make this more closely mirror PharmacoGx's computeAUC
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

computeSFn <- function(D, alpha, beta, SF_as_log = TRUE) { # returns ln(SF); this and all functions below it assume SF is a fraction (or log thereof) and not a percentage (or log thereof)
  SF <- -(alpha * D + beta * D ^ 2)
  
  if (!SF_as_log) {
    SF <- exp(SF)
  }
  
  return(SF)
}

computeSF2 <- function(alpha, beta, SF_as_log = TRUE) {
  SF <- computeSFn(D = 2,
                   alpha = alpha,
                   beta = beta,
                   SF_as_log = SF_as_log)
  
  if (!SF_as_log) {
    SF <- exp(SF)
  }
  
  return(SF)
}

computeSF10 <- function(alpha, beta) {
  SF <- computeSFn(D = 10,
                   alpha = alpha,
                   beta = beta,
                   SF_as_log = SF_as_log)
  
  if (!SF_as_log) {
    SF <- exp(SF)
  }
  
  return(SF)
}

computeDn <- function(SF, alpha, beta, SF_as_log = TRUE) {
  if (!SF_as_log) {
    SF <- log(SF)
  }
  
  if (SF > 0) {
    warning("Positive log survival fraction ", SF,  "cannot be reached at any dose of radiation with linear quadratic paramaters alpha, beta > 0.")
  }
  if (beta == 0) {
    if (alpha == 0) {
      if (SF == 1) {
        return(0)
      } else {
        warning(paste0("Survival fraction ", SF, " cannot be reached at any dose of radiation with linear-quadratic parameters alpha = ", alpha, " and beta = ", beta, "."))
        return(NA)
      }
    } else {
      return(-SF / alpha)
    }
  } else {
    return((sqrt(alpha ^ 2 - 4 * beta * SF) - alpha) / 2 / beta)
  }
}

computeD10 <- function(alpha, beta) {
  D <- computeDn(SF = 0.1,
                 alpha = alpha,
                 beta = beta,
                 SF_as_log = FALSE)
}