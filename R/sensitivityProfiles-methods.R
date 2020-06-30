#' sensitivityProfiles Getter
#'
#' Get the sensitivity values for the cell lines in the RadioSet
#'
#' @examples
#' sensProf <- sensitivityProfiles(clevelandSmall)
#'
#' @param object The \code{RadioSet} to get sensitivity values from
#'
#' @return a \code{data.frame} with the experiment info
#'
#' @describeIn RadioSet Return the phenotypic data for the radiation dose
#'   sensitivity
#'
#' @importFrom CoreGx sensitivityProfiles
#' @importFrom methods callNextMethod
#' @export
setMethod(sensitivityProfiles, "RadioSet", function(object) {
  callNextMethod(object)
})

#' sensitivityProfiles<- Setter
#'
#' Set the sensitivity information for the cell lines in the RadioSet
#'
#' @examples
#' sensitivityProfiles(clevelandSmall) <- sensitivityProfiles(clevelandSmall)
#'
#' @param object The \code{RadioSet} to update
#' @param value A \code{data.frame} with the new sensitivity values
#'
#' @return Updated \code{RadioSet}
#'
#' @importFrom CoreGx sensitivityProfiles<-
#' @describeIn RadioSet Update the phenotypic data for the radiation dose
#'   sensitivity
#' @export
setReplaceMethod("sensitivityProfiles",
                 signature = signature(object="RadioSet",
                                       value="data.frame"),
                 function(object, value) {
    callNextMethod(object=object, value=value)
})
#' @describeIn RadioSet Update the phenotypic data for the radiation dose
#'   sensitivity
#' @export
setReplaceMethod("sensitivityProfiles",
                 signature = signature(object="RadioSet",
                                       value="matrix"),
                 function(object, value) {
    callNextMethod(object=object, value=value)
})
