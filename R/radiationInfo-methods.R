#####
# RADIATION SLOT GETTERS/SETTERS ----
#####

#' radiationInfo Generic
#'
#' Generic for radiationInfo method
#'
#' @examples
#' radiationInfo(clevelandSmall)
#'
#' @param object A \code{RadioSet} object
#'
#' @return a \code{data.frame} with the radiation annotations
setGeneric("radiationInfo", function(object) standardGeneric("radiationInfo"))
#' @describeIn RadioSet Returns the annotations for all the radiations tested
#'   in the RadioSet
#' @export
setMethod(radiationInfo, "RadioSet", function(object){
  object@radiation
})

#' radiationInfo<- Generic
#'
#' Generic for radiationInfo replace method
#'
#' @examples

#' radiationInfo(clevelandSmall) <- radiationInfo(clevelandSmall)
#'
#'
#' @param object The \code{RadioSet} to replace radiation info in
#' @param value A \code{data.frame} with the new radiation annotations
#'
#' @return Updated \code{RadioSet}
setGeneric("radiationInfo<-", function(object, value)
  standardGeneric("radiationInfo<-"))
#' @describeIn RadioSet Update the radiation annotations
#' @export
setReplaceMethod("radiationInfo",
                 signature = signature(object="RadioSet",value="data.frame"),
                 function(object, value){
  object@radiation <- value
  object
})