#' sensitivityRaw Getter
#'
#' @examples
#' data(clevelandSmall)
#' sensitivityRaw(clevelandSmall)
#'
#' @param object A \code{RadioSet} to extract the raw sensitivity data from
#'
#' @return A \code{array} containing the raw sensitivity data
#'
#' @describeIn RadioSet Retrive the raw dose and viability data from an rSet
#'
#' @importMethodsFrom CoreGx sensitivityRaw
#' @export
setMethod("sensitivityRaw", signature("RadioSet"), function(object) {
  callNextMethod(object)
})


#' sensitivityRaw<- Setter
#'
#' @examples
#' data(clevelandSmall)
#' sensitivityRaw(clevelandSmall) <- sensitivityRaw(clevelandSmall)
#'
#' @param object A \code{RadioSet} to extract the raw sensitivity data from
#' @param value A \code{array} containing the raw dose and viability data for the
#'   rSet
#'
#' @return A copy of the \code{RadioSet} containing the updated sensitivty data
#'
#' @describeIn RadioSet Update the raw dose and viability data in an rSet object
#'
#' @importMethodsFrom CoreGx sensitivityRaw<-
#' @export
setReplaceMethod('sensitivityRaw', signature("RadioSet"), function(object, value) {
  callNextMethod(object=object, value=value)
})
