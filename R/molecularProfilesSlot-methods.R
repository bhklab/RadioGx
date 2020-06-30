#' molecularProfilesSlot Getter
#'
#' @describeIn RadioSet Get contents of molecularProfiles slot
#'
#' @examples
#' data(clevelandSmall)
#' molecularProfilesSlot(clevelandSmall)
#'
#' @param object A \code{RadioSet} from which to return a list of all availble
#'   SummarizedExperiment objects
#'
#' @return A \code{list} containing the molecularProfiles from a cSet
#'
#' @importFrom CoreGx molecularProfilesSlot
#' @importFrom methods callNextMethod
#' @export
setMethod("molecularProfilesSlot", signature("RadioSet"), function(object) {
  callNextMethod(object)
})

#' molecularProfilesSlot<- Setter
#'
#' @describeIn RadioSet Update the molecular profiles slot of a RadioSet and
#'    returns the updated copy
#'
#' @examples
#' data(clevelandSmall_cSet)
#' molecularProfilesSlot(clevelandSmall_cSet) <- molecularProfilesSlot(clevelandSmall_cSet)
#'
#' @param object A \code{RadioSet} object for which values will be replaced
#' @param value A \code{list} containing molecular profiles as SummarizedExperiments
#'
#' @return A copy of the \code{RadioSet} with the molecularProfiles slot updated
#'
#' @importFrom CoreGx molecularProfilesSlot<-
#' @importFrom methods callNextMethod
#' @export
setReplaceMethod("molecularProfilesSlot", signature("RadioSet"),
                 function(object, value){
  callNextMethod(object, value)
})
