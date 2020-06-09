##TODO:: Export this to CoreGx
#' datasetType Generic
#'
#' A generic for retrieving the dataset type of an rSet object
#'
#' @examples
#' data(clevelandSmall)
#' datasetType(clevelandSmall)
#'
#' @param object A \code{RadioSet} from which to retrieve the dataset type
#' @param ... A \code{list} containing fall through arguments; this allows
#'   addition of new parameters to methods for this generic
#'
#' @return A \code{character} vector containing the dataset tpye
#'
#' @export
setGeneric("datasetType", function(object, ...) standardGeneric("datasetType"))

#' @inheritParams datasetType
#' @describeIn RadioSet Update the dataset type of an rSet and return a copy of
#'     the updated object
#' @export
setMethod("datasetType", signature("RadioSet"), function(object) {
    ##TODO:: Add error handling to this function
    object@datasetType
})


##TODO:: Export this to CoreGx
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
#'
#' @export
setGeneric("datasetType<-",  function(object, value) standardGeneric("datasetType<-"))
#' @inheritParams datasetType<-
#' @describeIn RadioSet Update the dataset type of an rSet and return a copy of
#'     the updated object
#' @export
setReplaceMethod("datasetType", signature("RadioSet"), function(object, value) {
    ##TODO:: Add error handling to this function
    object@datasetType <- value
    object
})