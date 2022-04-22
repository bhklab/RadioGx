# rSet molecularProfiles from ESets to SEs
#
# Converts all ExpressionSet objects within the molecularProfiles slot of a
#  RadioSet to SummarizedExperiments
#
# @param rSet \code{S4} A RadioSet containing molecular data in ExpressionSets
#
# @return \code{S4} A RadioSet containing molecular data in a SummarizedExperiments
#
#' @importFrom SummarizedExperiment assay assays assayNames
#' @importClassesFrom SummarizedExperiment SummarizedExperiment Assays
#' @importFrom Biobase exprs fData pData annotation protocolData assayData experimentData
#' @importFrom S4Vectors SimpleList DataFrame
#' @importFrom stats setNames
#' @export
#' @keywords internal
.convertRsetMolecularProfilesToSE <- function(rSet) {

  eSets <- molecularProfilesSlot(rSet) # Extract eSet data

  molecularProfilesSlot(rSet) <-
    lapply(eSets,
           function(eSet){

             # Change rownames from probes to EnsemblGeneId for rna data type
             if (grepl("^rna$", Biobase::annotation(eSet))) {
               rownames(eSet) <- Biobase::fData(eSet)$EnsemblGeneId
             }

             # Build summarized experiment from eSet
             SE <- SummarizedExperiment::SummarizedExperiment(
               ## TODO:: Do we want to pass an environment for better memory efficiency?
               assays=SimpleList(as.list(Biobase::assayData(eSet))
               ),
               # Switch rearrange columns so that IDs are first, probes second
               rowData=S4Vectors::DataFrame(Biobase::fData(eSet),
                                            rownames=rownames(Biobase::fData(eSet))
               ),
               colData=S4Vectors::DataFrame(Biobase::pData(eSet),
                                            rownames=rownames(Biobase::pData(eSet))
               ),
               metadata=list("experimentData" = eSet@experimentData,
                             "annotation" = Biobase::annotation(eSet),
                             "protocolData" = Biobase::protocolData(eSet)
               )
             )
             ## TODO:: Determine if this can be done in the SE constructor?
             # Extract names from expression set
             SummarizedExperiment::assayNames(SE) <- Biobase::assayDataElementNames(eSet)
             # Assign SE to rSet
             mDataType <- Biobase::annotation(eSet)
             molecularProfilesSlot(rSet)[[mDataType]] <- SE
           })
  setNames(molecularProfilesSlot(rSet), names(eSets))
  rSet
}

## Validate rSet molecularProfiles Conversion
##
## Checks that all the information contained in an ExpressionSet molecularProfile
##   was successfully tranferred to the SummarizedExperiment molecularProfile
##
## @param rSet_new \code{S4} a rSet containing molecularProfiles as SummarizedExperiments
## @param rSet_old \code{S4} a rSet containing molecularProfiles as ExpressionSets
##
## @return \code{message} Any slots which are not the same
##
#' @importFrom assertthat are_equal
#' @importFrom SummarizedExperiment SummarizedExperiment Assays assay
#'   assayNames assayNames<-
#' @importFrom Biobase exprs fData pData annotation protocolData
#'   assayDataElementNames experimentData assayData
#' @keywords internal
.validateRsetMolecularProfilesToSEConversion <- function(rSet_old, rSet_new) {

  # Testing that rSets are in correct order
  message("Checking is rSet structures are correct")

  if(!all(vapply(rSet_old@molecularProfiles,
                 function(x) { is(x, "ExpressionSet") }, FUN.VALUE = logical(1)))
  ) message("Old rSet doesn't contain ExpressionSet objects, maybe argument order is wrong?")

  if(
    !all(vapply(molecularProfilesSlot(rSet_new),
                function(x) { is(x, "SummarizedExperiment") }, FUN.VALUE = logical(1)))
  ) message("New rSet doesn't contain SummarizedExperiment objects, maybe argument order is wrong?")

  # Comparing molecularProfiles slot data
  message("Checking molecularProfiles slots hold equivalent data.")

  for (i in seq_len(length(rSet_old@molecularProfiles))) {
    for (j in seq_along(assays(molecularProfilesSlot(rSet_new)[[i]]))) {
      if(!all(
          as.list(assayData(rSet_old@molecularProfiles[[i]]))[[j]],
            assay(molecularProfilesSlot(rSet_new)[[i]], j),
          na.rm = TRUE)) message("The assay data is not equivalent")
    }
  }
  ## TODO:: Rewrite this as an apply statement
  for (i in seq_len(length(rSet_old@molecularProfiles))) { # Have to compare like this due to NAs in data
    # Checking phenoData
    if(
      !(if (nrow(pData(rSet_old@molecularProfiles[[i]])) > 0) {
        all(
          as(rSet_old@molecularProfiles[[i]]@phenoData, "data.frame") ==
            as.data.frame(molecularProfilesSlot(rSet_new)[[i]]@colData[
              seq_len(length(molecularProfilesSlot(rSet_new)[[i]]@colData) -1)]),
          na.rm = TRUE)
      } else { FALSE })
    ) message("The phenoData is not equivalent")
    # Checking featureData
    if(
      !(if (nrow(fData(rSet_old@molecularProfiles[[i]])) > 0) {
        all(
          as(rSet_old@molecularProfiles[[i]]@featureData, "data.frame") ==
            as.data.frame(molecularProfilesSlot(rSet_new)[[i]]@elementMetadata[
              seq_len(length(molecularProfilesSlot(rSet_new)[[i]]@elementMetadata) - 1)]),
          na.rm = TRUE)
      } else { FALSE })
    ) message("The featureData is not equivalent")
    # Checking protocolData
    if(
      !all(
        as(rSet_old@molecularProfiles[[i]]@protocolData, "data.frame") ==
          as(molecularProfilesSlot(rSet_new)[[i]]@metadata$protocolData, "data.frame"),
        na.rm = TRUE)
      ) message("The protocolData is not equivalent")
  }

  if(!assertthat::are_equal(
    lapply(rSet_old@molecularProfiles, function(x) { annotation(x) }),
    lapply(molecularProfilesSlot(rSet_new), function(x) { metadata(x)$annotation }))
  )  message("The annotation is not equivalent")

  if(!assertthat::are_equal(
    lapply(rSet_old@molecularProfiles, function(x) { experimentData(x) }),
    lapply(molecularProfilesSlot(rSet_new), function(x) { metadata(x)$experimentData })
    )
  ) message("The experimentData is not equivalent")

  # Comparing remainder of rSet slots; should not be affect by conversion
  message("Comparing remainder of rSet slots")

  if (!assertthat::are_equal(rSet_old@annotation, rSet_new@annotation))
    message("annotation slots not equal!")

  if (!assertthat::are_equal(rSet_old@sample, rSet_new@sample))
    message("cell slots are not equal!")

  if (!assertthat::are_equal(rSet_old@radiation, rSet_new@radiation))
    message("radiation slots are not equal!")

  if (!assertthat::are_equal(rSet_old@treatmentResponse, sensitivitySlot(rSet_new)))
    message("sensitivty slots are not equal!")

  if (!assertthat::are_equal(rSet_old@datasetType, datasetType(rSet_new)))
    message("datasetType slots are not equal!")

  if (!assertthat::are_equal(rSet_old@perturbation, rSet_new@perturbation))
    message("perturbation slots are not equal!")

  if (!assertthat::are_equal(rSet_old@curation, rSet_new@curation))
    message("curation slots are not equal")
}