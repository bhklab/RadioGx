#' mDataNames
#'
#' Returns the molecular data names for the RadioSet.
#'
#' @examples
#' mDataNames(clevelandSmall)
#'
#' @param object The parameter
#'
#' @return Vector of names of the molecular data types
#' @importFrom CoreGx mDataNames
#' @export
setMethod(
    "mDataNames", "RadioSet", function(object ) {
        callNextMethod(object)
    })