
##TODO:: Migrate this to CoreGx
#' sensitivitySlot Generic
#'
#' @examples
#' data(clevelandSmall)
#' sensitivitySlot(clevelandSmall)
#'
#' @param object A \code{RadioSet} to extract the raw sensitivity data from
#' @param ... A \code{list} to allow new parameters in specific methods
#'
#' @return A \code{list} of the sensitivity slot contents
#'
#' @export
setGeneric("sensitivitySlot", function(object, ...) standardGeneric("sensitivitySlot"))
#' @describeIn RadioSet Retrieve the contents of the sensitivity slot
#' @inheritParams sensitivitySlot
#' @export
setMethod("sensitivitySlot", signature("RadioSet"), function(object) {
  object@sensitivity
})

##TODO:: Migrate this to CoreGx
#' sensitivitySlot<- Replacement Generic
#'
#' @examples
#' data(clevelandSmall)
#' sensitivitySlot(clevelandSmall) <- sensitivitySlot(clevelandSmall)
#'
#' @param object A \code{RadioSet} to extract the raw sensitivity data from
#' @param ... A \code{list} to allow new parameters in specific methods
#' @param value A \code{list} of new sensitivity slot data for the rSet
#'
#' @return A copy of the \code{RadioSet} containing the updated sensitivty slot
#'
#' @export
setGeneric("sensitivitySlot<-", function(object, ..., value) standardGeneric("sensitivitySlot<-"))
#' @describeIn RadioSet Set the raw dose and viability data for an rSet and return
#'   and updated copty
#' @inheritParams sensitivitySlot<-
#' @export
setReplaceMethod("sensitivitySlot", signature("RadioSet", "list"),
                 function(object, value) {
                   ##TODO:: Implement error handinlg for this slot
                   object@sensitivity <- value
                   object
                 })