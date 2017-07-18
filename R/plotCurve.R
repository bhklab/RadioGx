#' plotCurve: Produces plot of dose and response
#' @param D vector of radiation doses
#' @param SF vector of survival fractions corresponding to the doses
#' @param alpha parameter in the equation SF = exp(-alpha * D - beta * D ^ 2)
#' @param beta parameter in the equation SF = exp(-alpha * D - beta * D ^ 2)
#' @param filename name of PDF which will be created by the function
#' @param fit_curve should the graph include a linear-quadratic curve of best fit? Defaults to TRUE.
#' @param SF_as_log should SF be expressed in log10 on the graph? Defaults to TRUE.
#' @export
#' plotCurve()

plotCurve <- function(D, SF, alpha, beta, filename = "dose_response_plot.pdf", fit_curve = TRUE, SF_as_log = TRUE) {
  
  padding <- 1.1
  
  if (fit_curve) {
    if (missing(alpha) || missing(beta)) {
      alphaBeta <- linQuadRegression(D, SF, SF_as_log = SF_as_log)
      alpha <- alphaBeta[["alpha"]]
      beta <- alphaBeta[["beta"]]
    }
    print(paste0("A linear-quadratic curve was fit to the data with parameters alpha = ", alpha, " and beta = ", beta, "."))
    trendlineDs <- .GetSupportVec(D)
    trendlineSFs <- computeSFFromD(trendlineDs, alpha = alpha, beta = beta, SF_as_log = SF_as_log)
  }
  
  xlim <- range(D)
  xlim <- mean(xlim) + padding * c((xlim[1] - mean(xlim)), xlim[2] - mean(xlim))
  
  if (SF_as_log) {
    if (!missing(SF)) {
      if (fit_curve) {
        ylim <- padding * c(min(c(SF, trendlineSFs[length(trendlineSFs)])), 0)
      } else {
        ylim <- padding * c(min(SF), 0)
      }
    } else {
      ylim <- padding * c(trendlineSFs[length(trendlineSFs)], 0)
    }
  } else {
    ylim <- c(0, 1)
  }
  
  pdf(file = filename)
  
  plot(NULL,
       xlim = xlim,
       ylim = ylim,
       xlab = "Dose (Gy)",
       ylab = "Survival Fraction",
       col = "red")
  
  if (!missing(SF)) {
    points(D, SF, col = "red", pch = 19)
  }
  
  if (missing(SF) || fit_curve) {
    lines(trendlineDs, trendlineSFs, col = "blue", pch = 19)
  }
  
  dev.off()
}