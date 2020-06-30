#' sensitivitySlot Generic
#'
#' @examples
#' data(clevelandSmall)
#' sensitivitySlot(clevelandSmall)
#'
#' @param object A \code{RadioSet} to extract the raw sensitivity data from
#'
#' @return A \code{list} of the sensitivity slot contents
#'
#' @describeIn RadioSet Retrieve the contents of the sensitivity slot
#'
#' @importFrom methods callNextMethod
#' @importMethodsFrom CoreGx sensitivitySlot
#' @export
setMethod("sensitivitySlot", signature("RadioSet"), function(object) {
  callNextMethod(object)
})

##TODO:: Migrate this to CoreGx
#' sensitivitySlot<- Replacement Generic
#'
#' @examples
#' data(clevelandSmall)
#' sensitivitySlot(clevelandSmall) <- sensitivitySlot(clevelandSmall)
#'
#' @param object A \code{RadioSet} to extract the raw sensitivity data from
#' @param value A \code{list} of new sensitivity slot data for the rSet
#'
#' @return A copy of the \code{RadioSet} containing the updated sensitivty slot
#'
#' @describeIn RadioSet Set the raw dose and viability data for an rSet and return
#'   and updated copty
#'
#' @importFrom methods callNextMethod
#' @importMethodsFrom CoreGx sensitivitySlot<-
#' @export
setReplaceMethod("sensitivitySlot", signature("RadioSet", "list"),
                 function(object, value) {
                    callNextMethod(object=object, value=value)
                 })