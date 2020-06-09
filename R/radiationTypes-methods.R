#' radiationTypes Generic
#'
#' A generic for the radiationTypes method
#'
#' @examples
#' data(clevelandSmall)
#' radTypes <- radiationTypes(clevelandSmall)
#'
#' @param object A \code{RadioSet}
#'
#' @return A vector of the radiation names used in the RadioSet
setGeneric("radiationTypes", function(object) standardGeneric("radiationTypes"))
#'
#' @describeIn RadioSet Return the names of the radiations used in the RadioSet
#' @export
setMethod(radiationTypes, "RadioSet", function(object){
  rownames(radiationInfo(object))
})

#' radiationTypes<- Generic
#'
#' A generic for the radiationTypes replacement method
#'
#' @examples
#' radiationTypes(clevelandSmall) <- radiationTypes(clevelandSmall)
#'
#' @param object A \code{RadioSet} object to update
#' @param value A \code{character} vector of the new radiation names
#'
#' @return Updated \code{RadioSet}
setGeneric("radiationTypes<-",
           function(object, value) standardGeneric("radiationTypes<-"))
#' @describeIn RadioSet Update the radiation names used in the dataset
#' @export
setReplaceMethod("radiationTypes",
                 signature = signature(object="RadioSet",
                                       value="character"),
                 function(object, value){
    object <- updateRadId(object, value)
    return(object)
})