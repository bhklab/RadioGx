#' phenoInfo Getter method
#'
#' Getter method for the phenotype information of a given molecular data type
#'
#' @examples
#' data(clevelandSmall)
#' phenoInf <- phenoInfo(clevelandSmall, mDataType="rna")
#'
#' @param object A \code{RadioSet} object
#' @param mDataType A \code{character} with the type of molecular data to
#'   return
#'
#' @return a \code{data.frame} with the experiment info
#'
#' @describeIn RadioSet Return the experiment info from the given type of
#'   molecular data in RadioSet
#'
#' @importFrom CoreGx phenoInfo
#' @importFrom methods callNextMethod
#' @export
setMethod("phenoInfo", signature = c(object="RadioSet", mDataType="character"),
          function(object, mDataType){
  callNextMethod(object, mDataType)
})

#' phenoInfo<- Setter method
#'
#' Setter method for the phenotype information of a given molecular data type
#'
#' @examples
#' phenoInfo(clevelandSmall, mDataType="rna") <- phenoInfo(clevelandSmall,
#'   mDataType="rna")
#'
#' @param object A \code{RadioSet} object
#' @param mDataType the type of molecular data
#'
#' @return The updated \code{RadioSet}
#'
#' @describeIn RadioSet Update the the given type of molecular data experiment
#'   info in the RadioSet
#'
#' @importFrom CoreGx phenoInfo<-
#' @importFrom methods callNextMethod
#' @export
setReplaceMethod("phenoInfo",
                 signature = signature(object="RadioSet",
                                       mDataType ="character",
                                       value="data.frame"),
                 function(object, mDataType, value) {
  callNextMethod(object=object, mDataType=mDataType, value=value)
})