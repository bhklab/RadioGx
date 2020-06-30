#' annotation Slot Getter
#'
#' @param object A \code{RadioSet}
#'
#' @return A \code{list} of named annotaiton
#'
#' @examples
#' data(clevelandSmall)
#' annotation(clevelandSmall)
#'
#' @describeIn RadioSet Retrieve the annotations slot form an rSet
#'
#' @importMethodsFrom CoreGx annotation
#' @export
setMethod('annotation', signature("RadioSet"), function(object) {
    callNextMethod(object)
})

#' annotation<- Slot Setter
#'
#' @param object A \code{RadioSet}
#' @param value A \code{list} of annotations to add to the annotatiosn slot of
#'   an rSet
#'
#' @return A copy of the \code{RadioSet} with the updated annotation slot
#'
#' @examples
#' data(clevelandSmall)
#' annotation(clevelandSmall) <- annotation(clevelandSmall)
#'
#' @describeIn RadioSet Update the annotation slot of a tSet
#'
#' @importMethodsFrom CoreGx annotation<-
#' @export
setReplaceMethod("annotation", signature("RadioSet", "list"), function(object, value) {
    ## TODO:: Determine why I get this error with callNextMethod: Error in .local(object, ..., value): unused argument (value)
    object@annotation <- value
    return(object)
})