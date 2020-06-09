#' sensNumber Getter
#'
#' Get a summary of the available sensitivity experiments in the RadioSet
#'
#' @examples
#' data(clevelandSmall)
#' sensNumber(clevelandSmall)
#'
#' @param object A \code{RadioSet}
#'
#' @return A \code{data.frame} with the number of sensitivity experiments per
#'   radiation type and cell line
#'
#' @describeIn RadioSet Return a summary of available sensitivity experiments
#'
#' @importFrom CoreGx sensNumber
#' @importFrom methods callNextMethod
#' @export
setMethod(sensNumber, "RadioSet", function(object){
    callNextMethod(object)
})

#' sensNumber<- Setter
#'
#'  Set the summary of the available sensitivity experiments in the RadioSet
#'
#' @examples
#' sensNumber(clevelandSmall) <- sensNumber(clevelandSmall)
#'
#' @param object A \code{RadioSet}
#' @param value A new \code{matrix} with the number of sensitivity
#'   experiments per drug and cell line
#'
#' @return The updated \code{RadioSet}
#'
#' @describeIn RadioSet Update the summary of available sensitivity
#'   experiments
#'
#' @importMethodsFrom CoreGx sensNumber<-
#' @importFrom methods callNextMethod
#' @export
setReplaceMethod('sensNumber',
                 signature = signature(object="RadioSet",
                                       value="matrix"),
                 function(object, value){
                     callNextMethod(object, value)
                 })
