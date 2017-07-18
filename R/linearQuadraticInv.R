#' .linearQuadraticInv: computes radiation dose given survival fraction, alpha, beta
#' @param SF
#' @param alpha
#' @param beta
#' @param SF_as_log
#' .linearQuadraticInv()

.linearQuadraticInv <- function(SF, alpha, beta, SF_as_log = TRUE) {
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