#' name Getter
#'
#' Get the name of a RadioSet
#'
#' @examples
#' name(clevelandSmall)
#'
#' @param object A \code{RadioSet}
#'
#' @return The name of the RadioSet
#'
#' @describeIn RadioSet Return the name of the RadioSet
#'
#' @importFrom CoreGx name
#' @importFrom methods callNextMethod
#' @export
setMethod("name", "RadioSet", function(object) {
    callNextMethod(object)
})