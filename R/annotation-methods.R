##TODO:: Export to CoreGx
##FIXME:: How do I import generics from BiocGenerics?
#' annotation Slot Getter
#'
#' @param object A \code{RadioSet}
#' @param ... A \code{list} to allow definition of new parameters on this generic
#'
#' @return A \code{list} of named annotaiton
#'
#' @examples
#' data(clevelandSmall)
#' annotation(clevelandSmall)
#'
#' @export
setGeneric("annotation", function(object, ...) standardGeneric("annotation"))
#' @describeIn RadioSet Retrieve the annotations slot form an rSet
#' @inheritParams annotation<-
#' @export
setMethod('annotation', signature("RadioSet"), function(object) {
    object@annotation
})

##TODO:: Export to CoreGx
##FIXME:: How do I import generics from BiocGenerics?
#' annotation<- Slot Setter
#'
#' @param object A \code{RadioSet}
#' @param ... A \code{list} to allow definition of new parameters on this generic
#' @param value A \code{list} of annotations to add to the annotatiosn slot of
#'   an rSet
#'
#' @return A copy of the \code{RadioSet} with the updated annotation slot
#'
#' @examples
#' data(clevelandSmall)
#' annotation(clevelandSmall) <- annotation(clevelandSmall)
#'
#' @export
setGeneric("annotation<-", function(object, ..., value) standardGeneric("annotation<-"))
#' @describeIn RadioSet Update the annotation slot of a tSet
#' @inheritParams annotation<-
#' @export
setReplaceMethod("annotation", signature("RadioSet", "list"), function(object, value) {
    object@annotation <- value
    object
})