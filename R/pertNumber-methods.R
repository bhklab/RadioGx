#' pertNumber Getter
#'
#' Get a summary of available perturbation experiments
#'
#' @examples
#' data(clevelandSmall)
#' pertNumber(clevelandSmall)
#'
#' @param object A \code{RadioSet}
#'
#' @return A 3D \code{array} with the number of perturbation experiments per
#'   radiation type and cell line, and data type
#'
#' @describeIn RadioSet Return the summary of available perturbation
#'   experiments
#'
#' @importMethodsFrom CoreGx pertNumber
#' @importFrom methods callNextMethod
#' @export
setMethod(pertNumber, "RadioSet", function(object) {
    callNextMethod(object)
})

#' pertNumber<- Setter
#'
#' Get the summary of the available sensitivity experiments in the RadioSet
#'
#' @examples
#' pertNumber(clevelandSmall) <- pertNumber(clevelandSmall)
#'
#' @param object A \code{RadioSet}
#' @param value A new 3D \code{array} with the number of perturbation experiments
#'  per radiation type and cell line, and data type
#'
#' @return The updated \code{RadioSet}
#'
#' @describeIn RadioSet Update the summary of available perturbation
#'   experiments
#'
#' @importMethodsFrom CoreGx pertNumber<-
#' @importFrom methods callNextMethod
#' @export
setReplaceMethod('pertNumber',
                 signature = signature(object="RadioSet",
                                       value="array"),
                 function(object, value) {
                     callNextMethod(object=object, value=value)
                 })
