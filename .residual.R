function (x, y, n, pars, scale = 0.07, family = c("normal", "Cauchy"), trunc = FALSE) 
{
  return(PharmacoGx-API-Design:::.residual(x = x, y = y, n = n, pars = pars, f = computeSFFromD, scale = scale, family = family, trunc = trunc) *
           ifelse(pars[1] > pars[2], 1, Inf)) #ifelse takes care of requirement that alpha > beta
}