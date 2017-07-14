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