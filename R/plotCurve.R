#' Plot radiation dose-response curve
#'
#' @description This function plots doses of radiation against the cancer cell survival fractions thereby observed.
#' @param D vector of radiation doses
#' @param SF vector of survival fractions corresponding to the doses
#' @param alpha parameter in the equation SF = exp(-alpha * D - beta * D ^ 2)
#' @param beta parameter in the equation SF = exp(-alpha * D - beta * D ^ 2)
#' @param filename name of PDF which will be created by the function
#' @param fit_curve should the graph include a linear-quadratic curve of best fit? Defaults to TRUE.
#' @param SF_as_log should SF be expressed in log10 on the graph? Defaults to TRUE.
#' @example plotCurve(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), c(1.1, 0.8, 0.7, 0.45, 0.15, -0.1, -0.1, -0.4, -0.65, -0.75, -1.1))
#' @export
#' @importFrom graphics lines
#' @importFrom graphics plot
#' @importFrom graphics points
#' @importFrom grDevices dev
#' @importFrom grDevices pdf

plotCurve <- function(D, SF, alpha, beta, filename = "dose_response_plot.pdf", fit_curve = TRUE, SF_as_log = TRUE) {

  padding <- 1.1 # whitespace on graph around function range

  if (fit_curve) {
    if (missing(alpha) || missing(beta)) {
      alphaBeta <- linQuadRegression(D, SF, SF_as_log = SF_as_log)
      alpha <- alphaBeta[["alpha"]]
      beta <- alphaBeta[["beta"]]
    }
    print(paste0("A linear-quadratic curve was fit to the data with parameters alpha = ", alpha, " and beta = ", beta, "."))
    trendlineDs <- CoreGx:::.GetSupportVec(D)
    trendlineSFs <- .linearQuadratic(trendlineDs, alpha = alpha, beta = beta, SF_as_log = SF_as_log)
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
