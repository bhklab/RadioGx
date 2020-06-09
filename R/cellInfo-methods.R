#####
# CELL SLOT GETTERS/SETTERS ----
#####

#' cellInfo Getter
#'
#' Get cell line information from a RadioSet object
#'
#' @examples
#' data(clevelandSmall)
#' cellInf <- cellInfo(clevelandSmall)
#'
#' @param object A \code{RadioSet} object
#'
#' @return a \code{data.frame} with the cell annotations
#'
#' @describeIn RadioSet Returns the annotations for all the cell lines tested
#'   on in the RadioSet
#'
#' @importFrom CoreGx cellInfo
#' @importFrom methods callNextMethod
#' @export
setMethod(cellInfo, "RadioSet", function(object){
   callNextMethod(object)
})

#' cellInfo Setter
#'
#' Set cell line annotations for a RadioSet object
#'
#' @examples
#' data(clevelandSmall)
#' cellInfo(clevelandSmall) <- cellInfo(clevelandSmall)
#'
#' @param object A \code{RadioSet} object
#' @param value A replacement value
#'
#' @return Updated \code{RadioSet}
#'
#' @importFrom CoreGx cellInfo<-
#' @importFrom methods callNextMethod
#' @describeIn RadioSet Update the cell line annotations
#' @export
setReplaceMethod("cellInfo",
                 signature = signature(object="RadioSet",value="data.frame"),
                 function(object, value){
  callNextMethod(object, value)
})