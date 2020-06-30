#' molecularProfiles Getter
#'
#' Getter method for the molecular profile data of the given molecular data
#'   type
#'
#' @examples
#' data(clevelandSmall)
#' Cleveland_mProf <- molecularProfiles(clevelandSmall, "rna")
#'
#' @param object The \code{RadioSet} to retrieve molecular profiles from
#' @param mDataType \code{character} The type of molecular data
#' @param assay \code{character} Name of the desired assay; if excluded defaults
#'   to first assay in the SummarizedExperiment for the given mDataType
#'
#' @return a \code{data.frame} with the experiment info
#'
#' @describeIn RadioSet Return the given type of molecular data from the RadioSet
#'
#'
#' @importFrom CoreGx molecularProfiles
#' @importFrom methods callNextMethod
#' @export
setMethod(molecularProfiles, signature("RadioSet", "character"),
          function(object, mDataType){
   callNextMethod(object, mDataType)
})

#' molecularProfiles<- Setter
#'
#' Setter method for the molecular profile data of the given molecular data
#'   type
#'
#' @examples
#' molecularProfiles(clevelandSmall, "rna") <-
#'   molecularProfiles(clevelandSmall, "rna")
#'
#' @param object The \code{RadioSet} to replace molecular profiles in
#' @param mDataType The type of molecular data to be updated
#' @param assay \code{character} Name or index of the assay data to return
#' @param value A \code{matrix} with the new profiles
#'
#' @return Updated \code{RadioSet}
#'
#' @describeIn RadioSet Update the given type of molecular data from the RadioSet
#'
#' @importFrom CoreGx molecularProfiles<-
#' @importFrom methods callNextMethod
#' @export
setReplaceMethod("molecularProfiles",
                 signature(object="RadioSet", mDataType ="character",
                           assay="character", value="matrix"),
                 function(object, mDataType, assay, value) {
  callNextMethod(object=object, mDataType=mDataType, assay=assay, value=value)
})
#' @describeIn RadioSet Update the given type of molecular data from
#'   the RadioSet
#' @export
setReplaceMethod("molecularProfiles",
                 signature = signature(object="RadioSet",
                                       mDataType ="character",
                                       assay="missing",
                                       value="matrix"),
                 function(object, mDataType, assay, value) {
  callNextMethod(object=object, mDataType=mDataType, assay=assay, value=value)
})