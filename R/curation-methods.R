##TODO:: Export to CoreGx
#' curation Slot Getter
#'
#' @param object A \code{RadioSet}
#' @param ... A \code{list} to allow definition of new parameters on this generic
#'
#' @return A \code{list} of unique cell and tissue identifiers to check validity
#'   of an rSet
#'
#' @examples
#' data(clevelandSmall)
#' curation(clevelandSmall)
#'
#' @export
setGeneric("curation", function(object, ...) standardGeneric("curation"))
#' @describeIn RadioSet Retrieve the curation slot form an rSet
#' @inheritParams curation
#' @export
setMethod('curation', signature("RadioSet"), function(object) {
    object@curation
})

##TODO:: Export to CoreGx
##FIXME:: How do I import generics from BiocGenerics?
#' curation<- Slot Setter
#'
#' @param object A \code{RadioSet}
#' @param ... A \code{list} to allow definition of new parameters on this generic
#' @param value A \code{list} of curations for the cell and tissues types in the
#'   rSet object
#'
#' @return A copy of the \code{RadioSet} with the updated curation slot
#'
#' @examples
#' data(clevelandSmall)
#' curation(clevelandSmall) <- curation(clevelandSmall)
#'
#' @export
setGeneric("curation<-", function(object, ..., value) standardGeneric("curation<-"))
#' @describeIn RadioSet Update the annotation slot of a tSet
#' @inheritParams annotation<-
#' @export
setReplaceMethod("curation", signature("RadioSet", "list"), function(object, value) {
    object@curation <- value
    object
})