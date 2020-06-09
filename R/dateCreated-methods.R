#' dateCreated Getter
#'
#' Get the date a RadioSet object was created
#'
#' @examples
#' dateCreated(clevelandSmall)
#'
#' @param object A \code{RadioSet}
#'
#' @return The date the RadioSet was created
#'
#' @describeIn RadioSet Return the date the RadioSet was created
#'
#' @importFrom CoreGx dateCreated
#' @importFrom methods callNextMethod
#' @export
setMethod(dateCreated,
          signature = c("RadioSet"),
          function(object) {
  callNextMethod(object)
})