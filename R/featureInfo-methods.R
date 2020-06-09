#' featureInfo Getter
#'
#' Getter method for the feature data of the given molecular data
#'   type
#'
#' @examples
#' featInf <- featureInfo(clevelandSmall, "rna")
#'
#' @param object The \code{RadioSet} to retrieve feature annotations from
#' @param mDataType the type of molecular data
#'
#' @return A \code{DataFrame} containing the feature information
#'
#' @describeIn RadioSet Return the feature info for the given molecular data
#'
#' @importFrom CoreGx featureInfo
#' @importFrom methods callNextMethod
#' @export
setMethod("featureInfo",
          signature("RadioSet", "character"),
          function(object, mDataType) {
  callNextMethod(object, mDataType)
})

#' featureInfo<- Setter
#'
#' Setter method for the feature data of the given molecular data
#'   type
#'
#' @examples
#' featureInfo(clevelandSmall, "rna") <- featureInfo(clevelandSmall, "rna")
#'
#' @param object The \code{RadioSet} to replace gene annotations in
#' @param mDataType The type of molecular data to be updated
#' @param value A \code{data.frame} with the new feature annotations
#'
#' @return Updated \code{RadioSet}
#'
#' @describeIn RadioSet Replace the gene info for the molecular data
#'
#' @importFrom CoreGx featureInfo<-
#' @importFrom methods callNextMethod
#' @export
setReplaceMethod("featureInfo",
                 signature = signature(object="RadioSet",
                                       mDataType ="character",
                                       value="data.frame"),
                 function(object, mDataType, value) {
callNextMethod(object, mDataType, value)
})