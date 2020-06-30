#' datasetType Generic
#'
#' A generic for retrieving the dataset type of an rSet object
#'
#' @examples
#' data(clevelandSmall)
#' datasetType(clevelandSmall)
#'
#' @param object A \code{RadioSet} from which to retrieve the dataset type
#'
#' @return A \code{character} vector containing the dataset tpye
#'
#' @describeIn RadioSet Return the dataset type of an rSet object
#'
#' @importFrom methods callNextMethod
#' @importMethodsFrom CoreGx datasetType
#' @export
setMethod("datasetType", signature("RadioSet"), function(object) {
    callNextMethod(object)
})


#' datasetType<- Replacement Generic
#'
#' A generic for updating the dataset type of a RadioSet object
#'
#' @examples
#' data(clevelandSmall)
#' datasetType(clevelandSmall)
#'
#' @param object A \code{RadioSet} from which to retrieve the dataset type
#' @param value A \code{character} vector containing the dataset type
#'
#' @return A \code{character} vector containing the dataset tpye
#' @describeIn RadioSet Update the dataset type of an rSet and return a copy of
#'     the updated object
#'
#' @importFrom methods callNextMethod
#' @importMethodsFrom CoreGx datasetType<-
#' @export
setReplaceMethod("datasetType", signature("RadioSet"), function(object, value) {
    callNextMethod(object, value)
})