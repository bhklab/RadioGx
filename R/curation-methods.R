#' curation Slot Getter
#'
#' @param object A \code{RadioSet}
#'
#' @return A \code{list} of unique cell and tissue identifiers to check validity
#'   of an rSet
#'
#' @examples
#' data(clevelandSmall)
#' curation(clevelandSmall)
#'
#' @describeIn RadioSet Retrieve the curation slot form an rSet
#' @importMethodsFrom CoreGx curation
#' @export
setMethod('curation', signature("RadioSet"), function(object) {
    callNextMethod(object)
})

#' curation<- Slot Setter
#'
#' @param object A \code{RadioSet}
#' @param value A \code{list} of curations for the cell and tissues types in the
#'   rSet object
#'
#' @return A copy of the \code{RadioSet} with the updated curation slot
#'
#' @examples
#' data(clevelandSmall)
#' curation(clevelandSmall) <- curation(clevelandSmall)
#'
#' @describeIn RadioSet Update the curations for cell and tissue types in an rSet object
#' @importMethodsFrom CoreGx curation<-
#' @export
setReplaceMethod("curation", signature("RadioSet", "list"), function(object, value) {
    callNextMethod(object=object, value=value)
})