#' Takes the sensitivity data from a RadioSet, and summarises them into a
#' drug vs cell line table
#'
#' This function creates a table with cell lines as rows and radiation types as columns,
#' summarising the drug senstitivity data of a RadioSet into drug-cell line
#' pairs
#'
#' @examples
#' data(clevelandSmall)
#' GDSCauc <- summarizeSensitivityProfiles(clevelandSmall, sensitivity.measure='AUC_published')
#'
#' @param object `RadioSet` The RadioSet from which to extract the data
#' @param sensitivity.measure `character` which sensitivity sensitivity.measure to use? Use the
#'   sensitivityMeasures function to find out what measures are available for each PSet.
#' @param cell.lines `character` The cell lines to be summarized.
#'    If any cell lines has no data, it will be filled with
#'   missing values
#' @param radiation.types `character` The radiation types to be summarized.
#'   If any radiation type has no data, it will be filled with
#'   missing values
#' @param summary.stat `character` which summary method to use if there are repeated
#'   cell line-drug experiments? Choices are "mean", "median", "first", or "last"
#' @param fill.missing `logical(1)` should the missing cell lines not in the
#'   molecular data object be filled in with missing values?
#' @param verbose Should the function print progress messages?
#'
#' @return [matrix] A matrix with cell lines going down the rows, radiation types across
#'   the columns, with the selected sensitivity statistic for each pair.
#'
#' @importMethodsFrom CoreGx summarizeSensitivityProfiles
#' @export
setMethod('summarizeSensitivityProfiles',
          signature(object="RadioSet"),
          function(object, sensitivity.measure="AUC_recomputed", cell.lines, radiation.types,
                   summary.stat=c("mean", "median", "first", "last", "max", "min"), fill.missing=TRUE,
                   verbose=TRUE) {
            .summarizeSensitivityProfilesRadioSet(
              object, sensitivity.measure, cell.lines, radiation.types, summary.stat, fill.missing, verbose
            )
          })

# Takes the sensitivity data from a RadioSet, and summarises them into a
# drug vs cell line table
#
# This function creates a table with cell lines as rows and radiation types as columns,
# summarising the drug senstitivity data of a RadioSet into drug-cell line
# pairs
#
# @examples
# data(clevelandSmall)
# GDSCauc <- summarizeSensitivityProfiles(clevelandSmall, sensitivity.measure='AUC_published')
#
# @param object [RadioSet] The RadioSet from which to extract the data
# @param sensitivity.measure `character` which sensitivity sensitivity.measure to use? Use the
#   sensitivityMeasures function to find out what measures are available for each PSet.
# @param cell.lines \code{character} The cell lines to be summarized.
#    If any cell lines has no data, it will be filled with
#   missing values
# @param radiation.types \code{character} The radiation types to be summarized.
#   If any radiation type has no data, it will be filled with
#   missing values
# @param summary.stat \code{character} which summary method to use if there are repeated
#   cell line-drug experiments? Choices are "mean", "median", "first", or "last"
# @param fill.missing \code{boolean} should the missing cell lines not in the
#   molecular data object be filled in with missing values?
# @param verbose Should the function print progress messages?
#
# @return [matrix] A matrix with cell lines going down the rows, radiation types across
#   the columns, with the selected sensitivity statistic for each pair.
#
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom stats median
#' @importFrom reshape2 acast
#' @keywords internal
.summarizeSensitivityProfilesRadioSet <- function(
  object,
  sensitivity.measure="AUC_recomputed",
  cell.lines,
  radiation.types,
  summary.stat=c("mean", "median", "first", "last", "max", "min"),
  fill.missing=TRUE,
  verbose=TRUE)
{
	summary.stat <- match.arg(summary.stat)
  #sensitivity.measure <- match.arg(sensitivity.measure)
  if (!(sensitivity.measure %in% c(colnames(sensitivityProfiles(object)),"max.conc"))) {
    stop (sprintf("Invalid sensitivity measure for %s, choose among: %s",
                  annotation(object)$name,
                  paste(colnames(sensitivityProfiles(object)),
                        collapse=", ")))
  }
  if (missing(cell.lines)) {
    cell.lines <- cellNames(object)
  }
  if (missing(radiation.types)) {
    if (sensitivity.measure != "Synergy_score")
    {
      radTypes <- radiationTypes(object)
    }else{
      radTypes <- sensitivityInfo(object)[grep("///",
                                             sensitivityInfo(object)$radiation.type),
                                        "radiation.type"]
    }
  }

  pp <- sensitivityInfo(object)
  ##FIXME: deal with duplicated rownames!
  ppRows <- which(pp$sampleid %in% cell.lines & pp$radiation.type %in% radTypes)
  if(sensitivity.measure != "max.conc") {
    dd <- sensitivityProfiles(object)
  } else {

    if(!"max.conc"%in% colnames(sensitivityInfo(object))){

      object <- updateMaxConc(object)

    }
    dd <- sensitivityInfo(object)

  }

  result <- matrix(NA_real_, nrow=length(radTypes), ncol=length(cell.lines))
  rownames(result) <- radTypes
  colnames(result) <- cell.lines

  pp_dd <- cbind(pp[,c("sampleid", "radiation.type")],
                 "sensitivity.measure"=dd[, sensitivity.measure])

  summary.function <- function(x) {
    if(all(is.na(x))){
      return(NA_real_)
    }
    switch(summary.stat,
        "mean" = {
          return(mean(as.numeric(x), na.rm=TRUE))
        },
        "median" = {
          return(median(as.numeric(x), na.rm=TRUE))
        },
        "first" = {
          return(as.numeric(x)[[1]])
        },
        "last" = {
          return(as.numeric(x)[[length(x)]])
        },
        "max"= {
          return(max(as.numeric(x), na.rm=TRUE))
        },
        "min" = {
          return(min(as.numeric(x), na.rm=TRUE))
        })
  }

  pp_dd <- pp_dd[pp_dd[,"sampleid"]%in%cell.lines &
                   pp_dd[,"radiation.type"]%in%radTypes,]

  tt <- reshape2::acast(pp_dd, radiation.type~cellid,
                        fun.aggregate=summary.function,
                        value.var="sensitivity.measure")

  result[rownames(tt), colnames(tt)] <- tt

	if (!fill.missing) {
    myRows <- apply(result, 1, function(x) !all(is.na(x)))
    myCols <- apply(result, 2, function(x) !all(is.na(x)))
    result <- result[myRows, myCols]
	}
  return(result)
}
