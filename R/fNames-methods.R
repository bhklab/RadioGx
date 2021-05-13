#' fNames Getter
#'
#' Return the feature names for the specified molecular data type
#'
#' @examples
#' data(clevelandSmall)
#' fNames(clevelandSmall, "rna")
#'
#' @param object A \code{RadioSet}
#'
#' @return A \code{character} vector of the feature names
#'
#' @describeIn RadioSet Return the feature names used in the dataset
#'
#' @importMethodsFrom CoreGx fNames
#' @importFrom methods callNextMethod
#' @export
setMethod("fNames", signature("RadioSet", "character"), 
    function(object, mDataType)
{
    callNextMethod(object=object, mDataType=mDataType)
})

#' fNames<- Setter
#'
#' Setter for the feature names of a \code{SummarizedExperiment} in the
#'   molecularProfiles slot
#'
#' @examples
#' data(clevelandSmall)
#' fNames(clevelandSmall, 'rna') <- fNames(clevelandSmall, 'rna')
#'
#' @param object The \code{RadioSet} object to update
#' @param mDataType The molecular data type to update
#' @param value A \code{character} vector of the new cell names
#'
#' @return Updated \code{RadioSet}
#'
#' @describeIn RadioSet Set the feature names for a given molecular data type
#'
#' @importMethodsFrom CoreGx fNames<-
#' @importFrom methods callNextMethod
#' @export
setReplaceMethod("fNames", signature(object="RadioSet", mDataType='character',
    value="character"), function(object, mDataType, value) 
{
    callNextMethod(object=object, mDataType=mDataType, value=value)
})