## This function computes pars/AUC/SF2/D10 for the whole raw sensitivity data of a rset
#' @importFrom BiocParallel bplapply
.calculateFromRaw <- function(raw.sensitivity,
                              trunc=TRUE,
                              nthread=1,
                              family=c("normal", "Cauchy"),
                              scale = 5,
                              n = 1)
  {
  # Set multicore options
  op <- options()
  options(mc.cores=nthread)
  on.exit(options(op))


  family <- match.arg(family)

  AUC <- vector(length=dim(raw.sensitivity)[1])
  names(AUC) <- dimnames(raw.sensitivity)[[1]]

  SF2 <- vector(length=dim(raw.sensitivity)[1])
  names(SF2) <- dimnames(raw.sensitivity)[[1]]

  D10 <- vector(length=dim(raw.sensitivity)[1])
  names(D10) <- dimnames(raw.sensitivity)[[1]]

  if (nthread ==1){
    pars <- lapply(names(AUC), function(exp, raw.sensitivity, family, scale, n) {
      if(length(grep("///", raw.sensitivity[exp, , "Dose"])) > 0 | all(is.na(raw.sensitivity[exp, , "Dose"]))) {
        NA
      } else {
        return(unlist(linearQuadraticModel(raw.sensitivity[exp, , "Dose"], raw.sensitivity[exp, , "Response"], trunc=trunc, family=family, scale=scale, median_n=n)))
      }
    },raw.sensitivity=raw.sensitivity, family = family, scale = scale, n = n)
    names(pars) <- dimnames(raw.sensitivity)[[1]]
    AUC <- unlist(lapply(names(pars), function(exp,raw.sensitivity, pars) {
      if(any(is.na(pars[[exp]]))) {
        NA
      } else {
        computeAUC(D=raw.sensitivity[exp, , "Dose"], pars=pars[[exp]], trunc=trunc)
      }
    },raw.sensitivity=raw.sensitivity, pars=pars))
    SF2 <- unlist(lapply(names(pars), function(exp, pars) {
      if(any(is.na(pars[[exp]]))) {
        NA
      } else {
        computeSF2(pars=pars[[exp]])
      }
    }, pars=pars))
    D10 <- unlist(lapply(names(pars), function(exp, pars) {
      if(any(is.na(pars[[exp]]))) {
        NA
      } else {
        computeD10(pars=pars[[exp]])
      }
    }, pars=pars))

  } else {
    pars <- BiocParallel::bplapply(names(AUC), function(exp, raw.sensitivity, family, scale, n, trunc) {
      if(length(grep("///", raw.sensitivity[exp, , "Dose"])) > 0 | all(is.na(raw.sensitivity[exp, , "Dose"]))) {
        NA
      } else {
        linearQuadraticModel(raw.sensitivity[exp, , "Dose"], raw.sensitivity[exp, , "Response"], trunc=trunc, family=family, scale=scale, median_n=n)
      }
    }, raw.sensitivity=raw.sensitivity, family = family, scale = scale, n = n, trunc = trunc)
    names(pars) <- dimnames(raw.sensitivity)[[1]]
    AUC <- unlist(BiocParallel::bplapply(names(pars), function(exp, raw.sensitivity, pars, trunc) {
      if(any(is.na(pars[[exp]]))) {
        NA
      } else {
        computeAUC(D=raw.sensitivity[exp, , "Dose"], pars=pars[[exp]], trunc=trunc)
      }
    },raw.sensitivity=raw.sensitivity, pars=pars, trunc = trunc))
    SF2 <- unlist(BiocParallel::bplapply(names(pars), function(exp, pars, trunc) {
      if(any(is.na(pars[[exp]]))) {
        NA
      } else {
        computeSF2(pars=pars[[exp]])
      }
    }, pars=pars, trunc = trunc))
    D10 <- unlist(BiocParallel::bplapply(names(pars), function(exp, pars, trunc) {
      if(any(is.na(pars[[exp]]))) {
        NA
      } else {
        computeD10(pars=pars[[exp]])
      }
    }, pars=pars, trunc = trunc))
  }

  names(AUC) <- dimnames(raw.sensitivity)[[1]]
  names(SF2) <- dimnames(raw.sensitivity)[[1]]
  names(D10) <- dimnames(raw.sensitivity)[[1]]

  alpha <- vapply(pars, function(x) return(x[1]), numeric(1))
  beta <- vapply(pars, function(x) return(x[2]), numeric(1))


  return(list("AUC"=AUC, "SF2"=SF2, "D10"=D10 ,"alpha"=alpha, "beta"=beta))
}

updateMaxConc <- function(rSet) {
  sensitivityInfo(rSet)$max.conc <- apply(sensitivityRaw(rSet)[,,"Dose"], 1, max, na.rm=TRUE)
  return(rSet)
}
