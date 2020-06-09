
##TODO:: Migrate this to CoreGx
#' sensitivityRaw Generic
#'
#' @examples
#' data(clevelandSmall)
#' sensitivityRaw(clevelandSmall)
#'
#' @param object A \code{RadiSet} to extract the raw sensitivity data from
#' @param ... A \code{list} to allow new parameters in specific methods
#'
#' @return A \code{array} containing the raw sensitivity data
#'
#' @export
setGeneric("sensitivityRaw", function(object, ...) standardGeneric("sensitivityRaw"))
#' @describeIn RadioSet Retrive the raw dose and viability data from an rSet
#' @inheritParams sensitivityRaw
#' @export
setMethod("sensitivityRaw", signature("RadioSet"), function(object) {
  object@sensitivity$raw
})

##TODO:: Migrate this to CoreGx
#' sensitivityRaw<- Replacement Generic
#'
#' @examples
#' data(clevelandSmall)
#' sensitivityRaw(clevelandSmall) <- sensitivityRaw(clevelandSmall)
#'
#' @param object A \code{RadioSet} to extract the raw sensitivity data from
#' @param ... A \code{list} to allow new parameters in specific methods
#' @param value A \code{array} containing the raw dose and viability data for the
#'   rSet
#'
#' @return A copy of the \code{RadioSet} containing the updated sensitivty data
#'
#' @export
setGeneric("sensitivityRaw<-", function(object, ..., value) standardGeneric("sensitivityRaw<-"))
#' @describeIn RadioSet Set the raw dose and viability data for an rSet and return
#'   and updated copty
#' @inheritParams sensitivityRaw<-
#' @export
setReplaceMethod("sensitivityRaw", signature("RadioSet", "array"),
                 function(object, value) {
  object@sensitivity$raw <- value
  object
})