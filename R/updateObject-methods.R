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
    # treatment slot
    colnames(treatmentInfo(rSet)) <- gsub("X.radiation.|treatmentid|radiation",
        "treatmentid", colnames(treatmentInfo(rSet)))
    # sensitivity slot
    colnames(sensitivityInfo(rSet)) <- gsub("treatmentid", "treatmentid",
        colnames(sensitivityInfo(rSet)))
    # curation slot
    names(curation(rSet)) <- gsub("radiation", "treatment", names(curation(rSet)))
    if ("radiation" %in% names(curation(rSet))) {
        colnames(curation(rSet)$radiation) <- gsub("treatmentid|radiation",
            "treatment", colnames(curation(rSet)$radiation))
    }
    # molecularProfiles slot
    for (i in seq_along(molecularProfilesSlot(rSet))) {
        colnames(colData(molecularProfilesSlot(rSet)[[i]])) <-
            gsub("treatmentid|radiation", "treatmentid",
                colnames(colData(molecularProfilesSlot(rSet)[[i]]))
            )
    }
    validObject(rSet)
    return(rSet)
})