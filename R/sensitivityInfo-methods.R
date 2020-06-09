#' sensitivityInfo Getter
#'
#' Get the sensitivity information for the cell lines in the RadioSet
#'
#' @examples
#' sensInf<- sensitivityInfo(clevelandSmall)
#'
#' @param object A \code{RadioSet}
#'
#' @return a \code{DataFrame} with the experiment info
#'
#' @describeIn RadioSet Return the radiation dose sensitivity experiment info
#'
#' @importMethodsFrom CoreGx sensitivityInfo
#' @importFrom methods callNextMethod
#'
#' @export
setMethod(sensitivityInfo, "RadioSet", function(object) {
    callNextMethod(object)
})


#' sensitivityInfo<- Setter
#'
#' Set the sensitivity information for the cell lines in the RadioSet
#'
#' @examples
#' sensitivityInfo(clevelandSmall) <- sensitivityInfo(clevelandSmall)
#'
#' @param object The \code{RadioSet} to update
#' @param value A \code{data.frame} with the new sensitivity annotations
#'
#' @describeIn RadioSet Update the sensitivity experiment info
#'
#' @return Updated \code{RadioSet}
#'
#' @importMethodsFrom CoreGx sensitivityInfo<-
#' @export
setReplaceMethod("sensitivityInfo",
                 signature = signature(object="RadioSet",
                                       value="DataFrame"),
                 function(object, value) {
  callNextMethod(object, value)
})
