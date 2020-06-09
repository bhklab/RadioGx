#' sensitivityMeasures Getter
#'
#' Get the types of sensitivity measurements available in a RadioSet
#'
#' @examples
#' data(clevelandSmall)
#' sensMeas <- sensitivityMeasures(clevelandSmall)
#'
#' @param object A \code{RadioSet} object to get the types of avaialble
#'   sensitivity measurements from
#'
#' @return A \code{character} vector of all the available sensitivity measures
#'
#' @describeIn RadioSet Returns the available sensitivity profile
#'   summaries, for example, whether there are IC50 values available
#'
#' @importFrom CoreGx sensitivityMeasures
#' @importFrom methods callNextMethod
#' @export
setMethod(sensitivityMeasures, "RadioSet", function(object){
  callNextMethod(object)
})