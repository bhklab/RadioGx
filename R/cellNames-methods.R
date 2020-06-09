
#' cellNames Getter
#'
#' Get the names of all cell lines in a RadioSet object
#'
#' @examples
#' data(clevelandSmall)
#' cellNames(clevelandSmall)
#'
#' @param object The \code{RadioSet} to return cell names from
#'
#' @return A \code{vector} of the cell line names in the RadioSet
#'
#' @describeIn RadioSet Return the cell names used in the dataset
#'
#' @importFrom CoreGx cellNames
#' @importFrom methods callNextMethod
#' @export
setMethod(cellNames, "RadioSet", function(object) {
  callNextMethod(object)
})

#' cellNames<- Setter
#'
#' Set the names of all cell lines in a RadioSet object
#'
#' @examples
#' data(clevelandSmall)
#' cellNames(clevelandSmall) <- cellNames(clevelandSmall)
#'
#' @param object The \code{RadioSet} to update
#' @param value A \code{character} vector of the new cell names
#'
#' @return Updated \code{RadioSet}
#'
#' @describeIn RadioSet Update the cell names used in the dataset
#'
#' @importMethodsFrom CoreGx cellNames<-
#' @export
setReplaceMethod("cellNames",
                 signature = signature(object="RadioSet",
                                       value="character"),
                 function(object, value){
    callNextMethod(object, value)
})