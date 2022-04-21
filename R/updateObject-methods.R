#' @include RadioSet-accessors.R
NULL

#' Update the RadioSet class after changes in it struture or API
#'
#' @param object A `RadioSet` object to update the class structure for.
#'
#' @return `RadioSet` with update class structure.
#'
#' @md
#' @importMethodsFrom CoreGx updateObject
#' @export
setMethod("updateObject", signature("RadioSet"), function(object) {
    cSet <- callNextMethod(object)
    rSet <- as(cSet, "RadioSet")
    names(curation(rSet)) <- gsub("drug", "treatment", names(curation(rSet)))
    if ("treatment" %in% names(curation(rSet))) {
        colnames(curation(rSet)$treatment) <- gsub("treatmentid", "treatmentid",
            colnames(curation(rSet)$treatment))
    }
    validObject(rSet)
    return(rSet)
})