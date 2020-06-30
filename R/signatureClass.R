setOldClass('sessionInfo', sessionInfo)

#' @importFrom utils sessionInfo
.RadioSig <- setClass('RadioSig', slots=list(

            RSetName='character',
            DateCreated = 'character',
            SigType = 'character',
            SessionInfo = 'sessionInfo',
            Call = 'character'), contains='array')

#' Radiation Signature Class Constructor
#'
#' A documented constructor to provide user friendly interface to .RadioSig
#'
#' @param Data The data
#' @param RSetName The name of the pSet
#' @param DateCreated The date the object was created
#' @param SigType The type of sensitivyt signature
#' @param SessionInfo The package version used to generate the object
#' @param Call The calls for sensitivity vs not
#'
#' @return A \code{RadioSig} object
#'
#' @export
RadioSig <- function(Data=array(NA, dim=c(0,0,0)),
                     RSetName='',
                     DateCreated=date(),
                     SigType='sensitivity',
                     SessionInfo=sessionInfo(),
                     Call='No Call Recorded')
{
return(.RadioSig(Data,
                 RSetName=RSetName,
                 DateCreated=DateCreated,
                 SigType=SigType,
                 SessionInfo=SessionInfo,
                 Call=Call))
}

#' Show RadioGx Signatures
#'
#' @examples
#' data(clevelandSmall)
#' rad.sensitivity <- radSensitivitySig(clevelandSmall, mDataType="rna",
#'              nthread=1, features = fNames(clevelandSmall, "rna")[1])
#' rad.sensitivity
#'
#' @param object \code{RadioSig}
#'
#' @return Prints the RadioGx Signatures object to the output stream, and returns invisible NULL.
#'
#' @export
setMethod("show", signature=signature(object='RadioSig'),
        function(object) {
        cat('RadioSet Name: ', attr(object, 'RSetName'), "\n")
        cat('Signature Type: ', attr(object, 'SigType'), "\n")
        cat("Date Created: ", attr(object, 'DateCreated'), "\n")
        cat("Number of Radiation Types: ", dim(object)[[2]], "\n")
        cat("Number of Genes/Probes: ", dim(object)[[1]], "\n")
           })

#' Show the Annotations of a signature object
#'
#' This funtion prints out the information about the call used to compute the rad signatures, and the session info
#' for the session in which the computation was done. Useful for determining the exact conditions used to generate signatures.
#'
#' @examples
#' data(clevelandSmall)
#' rad.sensitivity <- radSensitivitySig(clevelandSmall, mDataType="rna",
#'              nthread=1, features = fNames(clevelandSmall, "rna")[1])
#' showSigAnnot(rad.sensitivity)
#'
#' @param object An object of the \code{RadioSig} Class, as
#' returned by \code{radPerturbationSig} or \code{radSensitivitySig}
#'
#' @return Prints the RadioGx Signatures annotations to the output stream, and returns invisible NULL.
#'
#' @importMethodsFrom CoreGx showSigAnnot
#' @export
setMethod("showSigAnnot", signature(object='RadioSig'), function(object) {
  .showSigAnnotRadioSig(object)
})


#' @keywords internal
.showSigAnnotRadioSig <- function(object) {
  print(attr(object, 'Call'))
  print(attr(object, 'SessionInfo'))
  return(invisible(NULL))
}





