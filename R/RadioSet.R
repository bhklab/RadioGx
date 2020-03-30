#' A Class to Contain RadioGenomic datasets together with their curations
#'
#' The RadioSet (RSet) class was developed to contain and organise large
#' RadioGenomic datasets, and aid in their metanalysis. It was designed
#' primarily to allow bioinformaticians and biologists to work with data at the
#' level of genes and cell lines, providing a more naturally intuitive
#' interface and simplifying analyses between several datasets. As such, it was
#' designed to be flexible enough to hold datasets of two different natures
#' while providing a common interface. The class can accomidate datasets
#' containing both radiation dose response data, as well as datasets contaning
#' genetic profiles of cell lines pre and post treatement with compounds, known
#' respecitively as sensitivity and perturbation datasets.
#'
#' @slot annotation A \code{list} of annotation data about the RadioSet,
#'    including the \code{$name} and the session information for how the object
#'    was creating, detailing the exact versions of R and all the packages used
#' @slot molecularProfiles A \code{list} containing 4 \code{SummarizedExperiment}
#'   type object for holding data for RNA, DNA, SNP and Copy Number Variation
#'   measurements respectively, with associated \code{fData} and \code{pData}
#'   containing the row and column metadata
#' @slot cell A \code{data.frame} containg the annotations for all the cell
#'   lines profiled in the data set, across all data types
#' @slot radiation A \code{data.frame} containg the annotations for all the
#'   radiation treatment types used in the in the dataset, across all data types
#' @slot sensitivity A \code{list} containing all the data for the sensitivity
#'   experiments, including \code{$info}, a \code{data.frame} containing the
#'   experimental info,\code{$raw} a 3D \code{array} containing raw data,
#'   \code{$profiles}, a \code{data.frame} containing sensitivity profiles
#'   statistics, and \code{$n}, a \code{data.frame} detailing the number of
#'   experiments for each cell-radiation type pair
#' @slot perturbation A \code{list} containting \code{$n}, a \code{data.frame}
#'   summarizing the available perturbation data,
#' @slot curation A \code{list} containing mappings for
#'   \code{cell} and \code{tissue} names used in the data set to universal
#'   identifiers used between different RadioSet objects
#' @slot datasetType A \code{character} string of 'sensitivity',
#'   'perturbation', or both detailing what type of data can be found in the
#'   RadioSet, for proper processing of the data
#'
#' @return An object of the RadioSet class
#'
#' @importClassesFrom CoreGx CoreSet
.RadioSet <- setClass("RadioSet", slots = list(radiation="data.frame"),
                      contains = "CoreSet")


# The default constructor above does a poor job of explaining the required structure of a RadioSet.
# The constructor function defined below guides the user into providing the required components of the curation and senstivity lists
# and hides the annotation slot which the user does not need to manually fill.
# This also follows the design of the Expression Set class.

#' RadioSet constructor
#'
#' A constructor that simplifies the process of creating RadioSets, as well
#' as creates empty objects for data not provided to the constructor. Only
#' objects returned by this constructor are expected to work with the RadioSet
#' methods. For a much more detailed instruction on creating RadioSets, please
#' see the "CreatingRadioSet" vignette.
#'
#' @examples
#' ## For help creating a RadioSet object, please see the following vignette:
#' browseVignettes("PharmacoGx")
#'
#'@inheritParams CoreGx::CoreSet
# @param name A \code{character} string detailing the name of the dataset
# @param molecularProfiles A \code{list} of ExpressionSet objects containing
#   molecular profiles
# @param cell A \code{data.frame} containg the annotations for all the cell
#   lines profiled in the data set, across all data types
#' @param radiation A \code{data.frame} containg the annotations for all the radiations
#'   profiled in the data set, across all data types
# @param sensitivityInfo A \code{data.frame} containing the information for the
#   sensitivity experiments
# @param sensitivityRaw A 3 Dimensional \code{array} contaning the raw radiation
#   dose – response data for the sensitivity experiments
# @param sensitivityProfiles \code{data.frame} containing radiation sensitivity profile
#   statistics such as IC50 and AUC
# @param sensitivityN,perturbationN A \code{data.frame} summarizing the
#   available sensitivity/perturbation data
#' @param curationCell,curationTissue A \code{data.frame} mapping
#'   the names for radiations, cells and tissues used in the data set to universal
#'   identifiers used between different RadioSet objects
# @param datasetType A \code{character} string of 'sensitivity',
#'   'preturbation', or both detailing what type of data can be found in the
#'   RadioSet, for proper processing of the data
# @param verify \code{boolean} Should the function verify the RadioSet and
#   print out any errors it finds after construction?

#' @return An object of class RadioSet
#'
#' @import methods
#' @importFrom utils sessionInfo
#' @importFrom stats na.omit
#' @importFrom SummarizedExperiment rowData colData assay assays assayNames Assays
#' @importFrom S4Vectors DataFrame SimpleList metadata
#' @importFrom CoreGx CoreSet
#'
#' @export
RadioSet <-  function(name,
                          molecularProfiles=list(),
                          cell=data.frame(),
                          radiation=data.frame(),
                          sensitivityInfo=data.frame(),
                          sensitivityRaw=array(dim=c(0,0,0)),
                          sensitivityProfiles=matrix(),
                          sensitivityN=matrix(nrow=0, ncol=0),
                          perturbationN=array(NA, dim=c(0,0,0)),
                          curationCell = data.frame(),
                          curationTissue = data.frame(),
                          datasetType=c("sensitivity", "perturbation", "both"),
                          verify = TRUE)
{
    datasetType <- match.arg(datasetType)

    annotation <- list()
    annotation$name <- as.character(name)
    annotation$dateCreated <- date()
    annotation$sessionInfo <- sessionInfo()
    annotation$call <- match.call()

    #molecularProfiles <- list("dna"=dna, "rna"=rna, "snp"=snp, "cnv"=cnv)
    ## TODO:: If the colnames and rownames are not found below, it will fill with NAs. This is undersirable behaviour.
    #molecularProfiles <- list("dna"=dna, "rna"=rna, "snp"=snp, "cnv"=cnv)
    ## TODO:: Determine if I should use SummarizedExperiment construtor here?
    for (i in seq_along(molecularProfiles)){
      if (!is(molecularProfiles[[i]], "SummarizedExperiment")) {
        stop(sprintf("Please provide the %s data as a SummarizedExperiment",
                     names(molecularProfiles[i])))
      }else{
        rowData(molecularProfiles[[i]]) <-
          rowData(molecularProfiles[[i]])[rownames(assays(molecularProfiles[[i]])[[1]]), , drop=FALSE]
        colData(molecularProfiles[[i]]) <-
          colData(molecularProfiles[[i]])[colnames(assays(molecularProfiles[[i]])[[1]]), , drop=FALSE]
      }

    }

    sensitivity <- list()

    if (!all(rownames(sensitivityInfo) == rownames(sensitivityProfiles) & rownames(sensitivityInfo) == dimnames(sensitivityRaw)[[1]])){
        stop("Please ensure all the row names match between the sensitivity data.")
    }

    sensitivity$info <- as.data.frame(sensitivityInfo, stringsAsFactors = FALSE)
    sensitivity$raw <- sensitivityRaw
    sensitivity$profiles <- as.data.frame(sensitivityProfiles, stringsAsFactors = FALSE)
    sensitivity$n <- sensitivityN

    curation <- list()
    # curation$radiation <- as.data.frame(curationDrug, stringsAsFactors = FALSE)
    curation$cell <- as.data.frame(curationCell, stringsAsFactors = FALSE)
    curation$tissue <- as.data.frame(curationTissue, stringsAsFactors = FALSE)
    ### TODO:: Make sure to fix the curation to check for matching row names to the radiation and cell line matrices!!!!!!


    perturbation <- list()
    perturbation$n <- perturbationN
    if (datasetType == "perturbation" || datasetType == "both") {
        perturbation$info <- "The metadata for the perturbation experiments is available for each molecular type by calling the appropriate info function. \n For example, for RNA transcriptome perturbations, the metadata can be accessed using rnaInfo(rSet)."
    } else {
        perturbation$info <- "Not a perturbation dataset."
    }

    rSet  <- .RadioSet(annotation=annotation, molecularProfiles=molecularProfiles, cell=as.data.frame(cell), radiation=as.data.frame(radiation), datasetType=datasetType, sensitivity=sensitivity, perturbation=perturbation, curation=curation)
    if (verify) { checkRSetStructure(rSet)}
  if(length(sensitivityN) == 0 & datasetType %in% c("sensitivity", "both")) {
    rSet@sensitivity$n <- .summarizeSensitivityNumbers(rSet)
  }
    if(length(perturbationN) == 0  & datasetType %in% c("perturbation", "both")) {
      rSet@perturbation$n <- .summarizePerturbationNumbers(rSet)
    }
  return(rSet)
}

##TODO:: Figure out how to properly inherit params from CoreGx

#####
# CELL SLOT GETTERS/SETTERS ----
#####

#' cellInfo Generic
#'
#' @param object A \code{RadioSet} object
#'
#' @return a \code{data.frame} with the cell annotations
#'
#' @describeIn RadioSet Returns the annotations for all the cell lines tested on in the RadioSet
#'
#' @importFrom CoreGx cellInfo
#' @importFrom methods callNextMethod
#' @export
setMethod(cellInfo, "RadioSet", function(object){
   callNextMethod(object)
})

#' cellInfo Replace Method
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
setReplaceMethod("cellInfo", signature = signature(object="RadioSet",value="data.frame"), function(object, value){
  callNextMethod(object, value)
})

#####
# RADIATION SLOT GETTERS/SETTERS ----
#####

#' radiationInfo Generic
#'
#' Generic for radiationInfo method
#'
#' @examples
#' radiationInfo(Cleveland_small)
#'
#' @param rSet A \code{RadioSet} object
#'
#' @return a \code{data.frame} with the radiation annotations
setGeneric("radiationInfo", function(rSet) standardGeneric("radiationInfo"))
#' @describeIn RadioSet Returns the annotations for all the radiations tested in the RadioSet
#' @export
setMethod(radiationInfo, "RadioSet", function(rSet){
  rSet@radiation
})

#' radiationInfo<- Generic
#'
#' Generic for radiationInfo replace method
#'
#' @examples

#' radiationInfo(Cleveland_small) <- radiationInfo(Cleveland_small)
#'
#'
#' @param object The \code{RadioSet} to replace radiation info in
#' @param value A \code{data.frame} with the new radiation annotations
#'
#' @return Updated \code{RadioSet}
setGeneric("radiationInfo<-", function(object, value) standardGeneric("radiationInfo<-"))
#' @describeIn RadioSet Update the radiation annotations
#' @export
setReplaceMethod("radiationInfo", signature = signature(object="RadioSet",value="data.frame"), function(object, value){
  object@radiation <- value
  object
})

#####
# MOLECULAR PROFILES SLOT GETTERS/SETTERS ----
#####

#' phenoInfo Getter method
#'
#' Getter method for the phenotype information of a given molecular data type
#'
#' @examples
#' data(Cleveland_small)
#' phenoInf <- phenoInfo(Cleveland_small, mDataType="rna")
#'
#' @param object A \code{RadioSet} object
#' @param mDataType A \code{character} with the type of molecular data to
#'   return
#'
#' @return a \code{data.frame} with the experiment info
#'
#' @describeIn RadioSet Return the experiment info from the given type of
#'   molecular data in RadioSet
#'
#' @importFrom CoreGx phenoInfo
#' @importFrom methods callNextMethod
#' @export
setMethod("phenoInfo", signature = c(object="RadioSet", mDataType="character"),
          function(object, mDataType){
  callNextMethod(object, mDataType)
})

#' phenoInfo<- Setter method
#'
#' Setter method for the phenotype information of a given molecular data type
#'
#' @examples
#'
#' phenoInfo(Cleveland_small, mDataType="rna") <- phenoInfo(Cleveland_small,
#'   mDataType="rna")
#'
#' @param object A \code{RadioSet} object
#' @param mDataType the type of molecular data
#'
#' @return The updated \code{RadioSet}
#'
#' @describeIn RadioSet Update the the given type of molecular data experiment
#'   info in the RadioSet
#'
#' @importFrom CoreGx phenoInfo<-
#' @importFrom methods callNextMethod
#' @export
setReplaceMethod("phenoInfo",
                 signature = signature(object="RadioSet",
                                       mDataType ="character",
                                       value="data.frame"),
                 function(object, mDataType, value) {
  callNextMethod(object, mDataType, value)
})


#' molecularProfiles Getter
#'
#' Getter method for the molecular profile data of the given molecular data
#'   type
#'
#' @examples
#' data(Cleveland_small)
#' Cleveland_mProf <- molecularProfiles(Cleveland_small, "rna")
#'
#' @param object The \code{RadioSet} to retrieve molecular profiles from
#' @param mDataType \code{character} The type of molecular data
#' @param assay \code{character} Name of the desired assay; if excluded defaults
#'   to first assay in the SummarizedExperiment for the given mDataType
#'
#' @return a \code{data.frame} with the experiment info
#'
#' @describeIn RadioSet Return the given type of molecular data from the RadioSet
#'
#'
#' @importFrom CoreGx molecularProfiles
#' @importFrom methods callNextMethod
#' @export
setMethod(molecularProfiles, signature("RadioSet", "character"),
          function(object, mDataType){
   callNextMethod(object, mDataType)
})

#' molecularProfiles<- Setter
#'
#' Setter method for the molecular profile data of the given molecular data
#'   type
#'
#' @examples
#' molecularProfiles(Cleveland_small, "rna") <-
#'   molecularProfiles(Cleveland_small, "rna")
#'
#' @param object The \code{PharmacoSet} to replace molecular profiles in
#' @param mDataType The type of molecular data to be updated
#' @param assay \code{character} Name or index of the assay data to return
#' @param value A \code{matrix} with the new profiles
#'
#' @return Updated \code{RadioSet}
#'
#' @describeIn RadioSet Update the given type of molecular data from the RadioSet
#'
#' @importFrom CoreGx molecularProfiles<-
#' @importFrom methods callNextMethod
#' @export
setReplaceMethod("molecularProfiles",
                 signature = signature(object="PharmacoSet",
                                       mDataType ="character",
                                       assay="character",
                                       value="matrix"), function(object, mDataType, assay, value){
  callNextMethod(object, mDataType, assay, value)
})
#' @describeIn RadioSet Update the given type of molecular data from
#'   the RadioSet
#' @export
setReplaceMethod("molecularProfiles",
                 signature = signature(object="PharmacoSet",
                                       mDataType ="character",
                                       assay="missing",
                                       value="matrix"),
                 function(object, mDataType, assay, value) {
  callNextMethod(object, mDataType, assay, value)
})


#' featureInfo Getter
#'
#' Getter method for the feature data of the given molecular data
#'   type
#'
#' @examples
#' featInf <- featureInfo(Cleveland_small, "rna")
#'
#' @param object The \code{RadioSet} to retrieve feature annotations from
#' @param mDataType the type of molecular data
#'
#' @return A \code{DataFrame} containing the feature information
#'
#' @describeIn RadioSet Return the feature info for the given molecular data
#'
#' @importFrom CoreGx featureInfo
#' @importFrom methods callNextMethod
#' @export
setMethod("featureInfo",
          signature("RadioSet", "character"),
          function(object, mDataType) {
  callNextMethod(object, mDataType)
})

#' featureInfo<- Setter
#'
#' Setter method for the feature data of the given molecular data
#'   type
#'
#' @examples
#' featureInfo(Cleveland_small, "rna") <- featureInfo(Cleveland_small, "rna")
#'
#' @param object The \code{RadioSet} to replace gene annotations in
#' @param mDataType The type of molecular data to be updated
#' @param value A \code{data.frame} with the new feature annotations
#'
#' @return Updated \code{RadioSet}
#'
#' @describeIn RadioSet Replace the gene info for the molecular data
#'
#' @importFrom CoreGx featureInfo<-
#' @importFrom methods callNextMethod
#' @export
setReplaceMethod("featureInfo",
                 signature = signature(object="RadioSet",
                                       mDataType ="character",
                                       value="data.frame"),
                 function(object, mDataType, value) {
callNextMethod(object, mDataType, value)
})

#####
# SENSITIVITY SLOT GETTERS/SETTERS ----
#####

#' sensitivityInfo Getter
#'
#' Get the sensitivity information for the cell lines in the RadioSet
#'
#' @examples
#' sensInf<- sensitivityInfo(Cleveland_small)
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
#' sensitivityInfo(Cleveland_small) <- sensitivityInfo(Cleveland_small)
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


#' sensitivityProfiles Getter
#'
#' Get the sensitivity values for the cell lines in the RadioSet
#'
#' @examples
#' sensProf <- sensitivityProfiles(Cleveland_small)
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
#' sensitivityProfiles(Cleveland_small) <- sensitivityProfiles(Cleveland_small)
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
    callNextMethod(object, value)
})
#' @describeIn RadioSet Update the phenotypic data for the radiation dose
#'   sensitivity
#' @export
setReplaceMethod("sensitivityProfiles",
                 signature = signature(object="RadioSet",
                                       value="matrix"),
                 function(object, value) {
    callNextMethod(object, value)
})

#' sensitivityMeasures Getter
#'
#' Get the types of sensitivity measurements available in a RadioSet
#'
#' @examples
#' data(Cleveland_small)
#' sensMeas <- sensitivityMeasures(Cleveland_small)
#'
#' @param object A \code{RadioSet} object to get the types of avaialble
#'   sensitivity measurements from
#'
#' @return A \code{character} vector of all the available sensitivity measures
#'
#' @describeIn RadioSet Returns the available sensitivity profile
#'   summaries, for example, whether there are IC50 values available
#'
#' @importFrom CoreGx sensitivityMeasures
#' @importFrom methods callNextMethod
#' @export
setMethod(sensitivityMeasures, "RadioSet", function(object){
  callNextMethod(object)
})

#' radiationTypes Generic
#'
#' A generic for the radiationTypes method
#'
#' @examples
#' data(Cleveland_small)
#' radTypes <- radiationTypes(Cleveland_small)
#'
#' @param object A \code{RadioSet}
#'
#' @return A vector of the radiation names used in the RadioSet
setGeneric("radiationTypes", function(rSet) standardGeneric("radiationTypes"))
#'
#' @describeIn RadioSet Return the names of the radiations used in the RadioSet
#' @export
setMethod(radiationTypes, "RadioSet", function(rSet){
  rownames(radiationInfo(rSet))
})

#' radiationTypes<- Generic
#'
#' A generic for the radiationTypes replacement method
#'
#'
#' @examples
#' radiationTypes(Cleveland_small) <- radiationTypes(Cleveland_small)
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

#' cellNames Getter
#'
#' Get the names of all cell lines in a RadioSet object
#'
#' @examples
#' data(Cleveland_small)
#' cellNames(Cleveland_small)
#'
#' @param object The \code{RadioSet} to return cell names from
#'
#' @return A \code{vector} of the cell line names in the RadioSet
#'
#' @describeIn RadioSet Return the cell names used in the dataset
#'
#' @importFrom CoreGx cellNames
#' @importFrom methods callNextMethod
#' @export
setMethod(cellNames, "RadioSet", function(object) {
  callNextMethod(object)
})

#' cellNames<- Setter
#'
#' Set the names of all cell lines in a RadioSet object
#'
#' @examples
#' data(Cleveland_small)
#' cellNames(Cleveland_small) <- cellNames(Cleveland_small)
#'
#' @param object The \code{RadioSet} to update
#' @param value A \code{character} vector of the new cell names
#'
#' @return Updated \code{RadioSet}
#'
#' @describeIn RadioSet Update the cell names used in the dataset
#'
#' @importMethodsFrom CoreGx cellNames<-
#' @export
setReplaceMethod("cellNames",
                 signature = signature(object="RadioSet",
                                       value="character"),
                 function(object, value){
    callNextMethod(object, value)
})

#' fNames Getter
#'
#' Return the feature names for the specified molecular data type
#'
#' @examples
#' data(Cleveland_small)
#' fNames(Cleveland_small, "rna")
#'
#' @param object A \code{RadioSet}
#'
#' @return A \code{character} vector of the feature names
#'
#' @describeIn RadioSet Return the feature names used in the dataset
#'
#' @importFrom CoreGx fNames
#' @importFrom methods callNextMethod
#' @export
setMethod("fNames",
          signature("RadioSet", "character"),
          function(object=rSet, mDataType){
  callNextMethod(object, mDataType)
})

#' fNames<- Setter
#'
#' Setter for the feature names of a \code{SummarizedExperiment} in the
#'   molecularProfiles slot
#'
#' @examples
#' data(Cleveland_small)
#' fNames(Cleveland_small, 'rna') <- fNames(Cleveland_small, 'rna')
#'
#' @param object The \code{RadioSet} object to update
#' @param mDataType The molecular data type to update
#' @param value A \code{character} vector of the new cell names
#'
#' @return Updated \code{RadioSet}
#'
#' @describeIn RadioSet Set the feature names for a given molecular data type
#'
#' @importFrom CoreGx fNames<-
#' @importFrom methods callNextMethod
#' @export
setReplaceMethod("fNames",
                 signature = signature(object="RadioSet",
                                       mDataType='character',
                                       value="character"),
                 function(object, mDataType, value) {
    callNextMethod(object, mDataType, value)
})

#' dateCreated Getter
#'
#' Get the date a RadioSet object was created
#'
#' @examples
#' dateCreated(Cleveland_small)
#'
#' @param object A \code{RadioSet}
#'
#' @return The date the RadioSet was created
#'
#' @describeIn RadioSet Return the date the RadioSet was created
#'
#' @importFrom CoreGx dateCreated
#' @importFrom methods callNextMethod
#' @export
setMethod(dateCreated,
          signature = c("RadioSet"),
          function(object) {
  callNextMethod(object)
})


#' name Getter
#'
#' Get the name of a RadioSet
#'
#' @examples
#' name(Cleveland_small)
#'
#' @param object A \code{RadioSet}
#'
#' @return The name of the RadioSet
#'
#' @describeIn RadioSet Return the name of the RadioSet
#'
#' @importFrom CoreGx name
#' @importFrom methods callNextMethod
#' @export
setMethod("name", "RadioSet", function(object) {
    callNextMethod(object)
})

#' pertNumber Getter
#'
#' Get a summary of available perturbation experiments
#'
#' @examples
#' data(Cleveland_small)
#' pertNumber(Cleveland_small)
#'
#' @param object A \code{RadioSet}
#'
#' @return A 3D \code{array} with the number of perturbation experiments per
#'   radiation type and cell line, and data type
#'
#' @describeIn RadioSet Return the summary of available perturbation
#'   experiments
#'
#' @importMethodsFrom CoreGx pertNumber
#' @importFrom methods callNextMethod
#' @export
setMethod(pertNumber, "RadioSet", function(object) {
  callNextMethod(object)
})


#' sensNumber Getter
#'
#' Get a summary of the available sensitivity experiments in the RadioSet
#'
#' @examples
#' data(Cleveland_small)
#' sensNumber(Cleveland_small)
#'
#' @param object A \code{RadioSet}
#'
#' @return A \code{data.frame} with the number of sensitivity experiments per
#'   radiation type and cell line
#'
#' @describeIn RadioSet Return a summary of available sensitivity experiments
#'
#' @importFrom CoreGx sensNumber
#' @importFrom methods callNextMethod
#' @export
setMethod(sensNumber, "RadioSet", function(object){
    callNextMethod(object)
})

#' pertNumber<- Setter
#'
#' Get the summary of the available sensitivity experiments in the RadioSet
#'
#' @examples
#' pertNumber(Cleveland_small) <- pertNumber(Cleveland_small)
#'
#' @param object A \code{RadioSet}
#' @param value A new 3D \code{array} with the number of perturbation experiments
#'  per radiation type and cell line, and data type
#'
#' @return The updated \code{RadioSet}
#'
#' @describeIn RadioSet Update the summary of available perturbation
#'   experiments
#'
#' @importMethodsFrom CoreGx pertNumber<-
#' @importFrom methods callNextMethod
#' @export
setReplaceMethod('pertNumber',
                 signature = signature(object="RadioSet",
                                       value="array"),
                 function(object, value) {
  callNextMethod(object, value)
})

#' sensNumber<- Setter
#'
#'  Set the summary of the available sensitivity experiments in the RadioSet
#'
#' @examples
#' sensNumber(Cleveland_small) <- sensNumber(Cleveland_small)
#'
#' @param object A \code{RadioSet}
#' @param value A new \code{matrix} with the number of sensitivity
#'   experiments per drug and cell line
#'
#' @return The updated \code{RadioSet}
#'
#' @describeIn RadioSet Update the summary of available sensitivity
#'   experiments
#'
#' @importMethodsFrom CoreGx sensNumber<-
#' @importFrom methods callNextMethod
#' @export
setReplaceMethod('sensNumber',
                 signature = signature(object="RadioSet",
                                       value="matrix"),
                 function(object, value){
    callNextMethod(object, value)
})


#' Show a RadioSet
#'
#' @param object A \code{RadioSet} object
#'
#' @examples
#' data(Cleveland_small)
#' Cleveland_small
#'
#' @return Prints the RadioSet object to the output stream, and returns
#'   invisible NULL.
#'
#' @export
setMethod("show", signature=signature(object="RadioSet"),
    function(object) {
        cat("Name: ", name(object), "\n")
        cat("Date Created: ", dateCreated(object), "\n")
    cat("Number of cell lines: ", nrow(cellInfo(object)), "\n")
    cat("Number of radiation types: ", nrow(radiationInfo(object)), "\n")
        if("dna" %in% names(object@molecularProfiles)){cat("DNA: \n");cat("\tDim: ", dim(molecularProfiles(object, mDataType="dna")), "\n")}
      if("rna" %in% names(object@molecularProfiles)){cat("RNA: \n");cat("\tDim: ", dim(molecularProfiles(object, mDataType="rna")), "\n")}
      if("rnaseq" %in% names(object@molecularProfiles)){cat("RNASeq: \n");cat("\tDim: ", dim(molecularProfiles(object, mDataType="rnaseq")), "\n")}
      if("snp" %in% names(object@molecularProfiles)){cat("SNP: \n");cat("\tDim: ", dim(molecularProfiles(object, mDataType="snp")), "\n")}
      if("cnv" %in% names(object@molecularProfiles)){cat("CNV: \n");cat("\tDim: ", dim(molecularProfiles(object, mDataType="cnv")), "\n")}
        cat("Drug pertubation: \n")
        cat("\tPlease look at pertNumber(rSet) to determine number of experiments for each radiation-cell combination.\n")
        cat("Drug sensitivity: \n")
        cat("\tNumber of Experiments: ",nrow(sensitivityInfo(object)),"\n")
        cat("\tPlease look at sensNumber(rSet) to determine number of experiments for each radiation-cell combination.\n")
    })

#' mDataNames
#'
#' Returns the molecular data names for the RadioSet.
#'
#' @examples

#' mDataNames(Cleveland_small)
#'
#' @param object The parameter
#'
#' @return Vector of names of the molecular data types
#' @importFrom CoreGx mDataNames
#' @export
setMethod(
  "mDataNames", "RadioSet", function(object ) {
    callNextMethod(object)
})

#'`[`
#'
#'@param x a \code{RadioSet} object
#'@param i Cell lines to keep in RSet
#'@param j Drugs to keep in RSet
#'@param ... further arguments
#'@param drop A boolean flag of whether to drop single dimensions or not
#'@return Returns the subsetted RSet
#'@export
setMethod(`[`, "RadioSet", function(x, i, j, ..., drop = FALSE){
  if(is.character(i)&&is.character(j)){
    return(subsetTo(x, cells=i, radiations=j,  molecular.data.cells=i))
  }
  else if(is.numeric(i) && is.numeric(j) && (as.integer(i)==i) &&
          (as.integer(j)==j)){
    return(subsetTo(x, cells=cellNames(x)[i], radiations=radiationTypes(x)[j],
                    molecular.data.cells=cellNames(x)[i]))
  }
})

#' Get the dimensions of a RadioSet
#'
#' @param x RadioSet
#' @return A named vector with the number of Cells and Drugs in the RadioSet
#' @export
setMethod("dim", signature=signature(x="RadioSet"), function(x){
  return(c(Cells=length(cellNames(x)), Radiation=length(radiationTypes(x))))
})

## FIXED? TODO:: Subset function breaks if it doesnt find cell line in sensitivity info
#' A function to subset a RadioSet to data containing only specified radiations, cells and genes
#'
#' This is the prefered method of subsetting a RadioSet. This function allows
#' abstraction of the data to the level of biologically relevant objects: radiations
#' and cells. The function will automatically go through all of the
#' combined data in the RadioSet and ensure only the requested radiations
#' and cell lines are found in any of the slots. This allows quickly picking out
#' all the experiments for a radiation or cell of interest, as well removes the need
#' to keep track of all the metadata conventions between different datasets.
#'
#' @examples
#' clevelandRadiationTypes  <- radiationTypes(Cleveland_small)
#' clevelandCells <- cellNames(Cleveland_small)
#' RSet <- subsetTo(Cleveland_small,radiationTypes = clevelandRadiationTypes[1],
#'   cells = clevelandCells[1])
#' RSet
#'
#' @param object A \code{RadioSet} to be subsetted
#' @param cells A list or vector of cell names as used in the dataset to which
#'   the object will be subsetted. If left blank, then all cells will be left in
#'   the dataset.
#' @param radiationTypes A list or vector of radiation names as used in the dataset to which
#'   the object will be subsetted. If left blank, then all radiationTypes will be left in
#'   the dataset.
#' @param molecular.data.cells A list or vector of cell names to keep in the
#'   molecular data
#' @param keep.controls If the dataset has perturbation type experiments, should
#'   the controls be kept in the dataset? Defaults to true.
#' @param ... Other arguments passed by other function within the package
#' @return A RadioSet with only the selected radiation types and cells
#' @importFrom CoreGx .unionList
#' @export
# subsetTo <- function(object, cells=NULL, radiationTypes=NULL, exps=NULL, molecular.data.cells=NULL, keep.controls=TRUE) {
subsetTo <- function(object, cells=NULL, radiationTypes=NULL, molecular.data.cells=NULL, keep.controls=TRUE, ...) {
  drop=FALSE

  adArgs = list(...)
  if ("exps" %in% names(adArgs)) {
  	exps <- adArgs[["exps"]]
  	if(is(exps,"data.frame")){
  		exps2 <- exps[[name(object)]]
  		names(exps2) <- rownames(exps)
  		exps <- exps2
  	} else{
  		exps <- exps[[name(object)]]
  	}
  }else {
    exps <- NULL
  }
  if(!missing(cells)){
    cells <- unique(cells)
  }

  if(!missing(radiationTypes)){
    radiationTypes <- unique(radiationTypes)
  }

  if(!missing(molecular.data.cells)){
    molecular.data.cells <- unique(molecular.data.cells)
  }

    ### TODO:: implement strict subsetting at this level!!!!

    ### the function missing does not work as expected in the context below, because the arguments are passed to the anonymous
    ### function in lapply, so it does not recognize them as missing

  object@molecularProfiles <- lapply(object@molecularProfiles, function(SE, cells, radiationTypes, molecular.data.cells){

    molecular.data.type <- ifelse(length(grep("rna", S4Vectors::metadata(SE)$annotation) > 0), "rna", S4Vectors::metadata(SE)$annotation)
    if (length(grep(molecular.data.type, names(molecular.data.cells))) > 0) {
      cells <- molecular.data.cells[[molecular.data.type]]
    }

    column_indices <- NULL

    if (length(cells)==0 && length(radiationTypes)==0) {
      column_indices <- seq_len(ncol(SE)) # This still returns the number of samples in an SE, but without a label
    }
    if(length(cells)==0 && object@datasetType=="sensitivity") {
      column_indices <- seq_len(ncol(SE))
    }

    cell_line_index <- NULL
    if(length(cells)!=0) {
      if (!all(cells %in% cellNames(object))) {
        stop("Some of the cell names passed to function did not match to names in the PharmacoSet. Please ensure you are using cell names as returned by the cellNames function")
      }
      cell_line_index <- which(SummarizedExperiment::colData(SE)[["cellid"]] %in% cells)
      # if (length(na.omit(cell_line_index))==0){
      #       stop("No cell lines matched")
      #     }
    }
    radiationTypes_index <- NULL
    if(object@datasetType=="perturbation" || object@datasetType=="both"){
      if(length(radiationTypes) != 0) {
        if (!all(radiationTypes %in% drugNames(object))){
          stop("Some of the drug names passed to function did not match to names in the PharmacoSet. Please ensure you are using drug names as returned by the drugNames function")
        }
        radiationTypes_index <- which(SummarizedExperiment::colData(SE)[["drugid"]] %in% radiationTypes)
        # if (length(radiationTypes_index)==0){
        #         stop("No radiationTypes matched")
        #       }
        if(keep.controls) {
          control_indices <- which(SummarizedExperiment::colData(SE)[["xptype"]]=="control")
          radiationTypes_index <- c(radiationTypes_index, control_indices)
        }
      }
    }

    if(length(radiationTypes_index) != 0 && length(cell_line_index) != 0) {
      if(length(intersect(radiationTypes_index, cell_line_index)) == 0) {
        stop("This Drug - Cell Line combination was not tested together.")
      }
      column_indices <- intersect(radiationTypes_index, cell_line_index)
    } else {
      if(length(radiationTypes_index) !=0) {
        column_indices <- radiationTypes_index
      }
      if(length(cell_line_index) !=0) {
        column_indices <- cell_line_index
      }
    }

    row_indices <- seq_len(nrow(SummarizedExperiment::assay(SE, 1)))

    SE <- SE[row_indices, column_indices]
    return(SE)

  }, cells=cells, radiationTypes=radiationTypes, molecular.data.cells=molecular.data.cells)

  if ((object@datasetType == "sensitivity" | object@datasetType == "both") & length(exps) != 0) {
      object@sensitivity$info <- object@sensitivity$info[exps, , drop=drop]
      rownames(object@sensitivity$info) <- names(exps)
      if(length(object@sensitivity$raw) > 0) {
        object@sensitivity$raw <- object@sensitivity$raw[exps, , , drop=drop]
        dimnames(object@sensitivity$raw)[[1]] <- names(exps)
      }
      object@sensitivity$profiles <- object@sensitivity$profiles[exps, , drop=drop]
      rownames(object@sensitivity$profiles) <- names(exps)

      object@sensitivity$n <- .summarizeSensitivityNumbers(object)
  }
  else if ((object@datasetType == "sensitivity" | object@datasetType == "both") & (length(radiationTypes) != 0 | length(cells) != 0)) {

        radiationTypes_index <- which (sensitivityInfo(object)[, "radiation.type"] %in% radiationTypes)
        cell_line_index <- which (sensitivityInfo(object)[,"cellid"] %in% cells)
        if (length(radiationTypes_index) !=0 & length(cell_line_index) !=0 ) {
          if (length(intersect(radiationTypes_index, cell_line_index)) == 0) {
            stop("This Drug - Cell Line combination was not tested together.")
          }
          row_indices <- intersect(radiationTypes_index, cell_line_index)
        } else {
          if(length(radiationTypes_index)!=0 & length(cells)==0) {
                row_indices <- radiationTypes_index
          } else {
              if(length(cell_line_index)!=0 & length(radiationTypes)==0){
                  row_indices <- cell_line_index
              } else {
              row_indices <- vector()
              }
          }
       }
        object@sensitivity[names(object@sensitivity)[names(object@sensitivity)!="n"]] <- lapply(object@sensitivity[names(object@sensitivity)[names(object@sensitivity)!="n"]], function(x,i, drop){
            #browser()
          if (length(dim(x))==2){
            return(x[i,,drop=drop])
          }
          if (length(dim(x))==3){
            return(x[i,,,drop=drop])
          }
          }, i=row_indices, drop=drop)
  }

	if (length(radiationTypes)==0) {
		if(object@datasetType == "sensitivity" | object@datasetType == "both"){
			radiationTypes <- unique(sensitivityInfo(object)[["radiation.type"]])
		}
		if(object@datasetType == "perturbation" | object@datasetType == "both"){
			radiationTypes <- union(radiationTypes, na.omit(.unionList(lapply(object@molecularProfiles, function(SE){unique(colData(SE)[["radiation.type"]])}))))
		}
	}
	if (length(cells)==0) {
		cells <- union(cells, na.omit(.unionList(lapply(object@molecularProfiles, function(SE){unique(colData(SE)[["cellid"]])}))))
        if (object@datasetType =="sensitivity" | object@datasetType == "both"){
            cells <- union(cells, sensitivityInfo(object)[["cellid"]])
        }
	}
	radiationInfo(object) <- radiationInfo(object)[radiationTypes , , drop=drop]
	cellInfo(object) <- cellInfo(object)[cells , , drop=drop]
	object@curation$radiation <- object@curation$radiation[radiationTypes , , drop=drop]
	object@curation$cell <- object@curation$cell[cells , , drop=drop]
	object@curation$tissue <- object@curation$tissue[cells , , drop=drop]
	if (object@datasetType == "sensitivity" | object@datasetType == "both"  & length(exps) == 0) {
	  object@sensitivity$n <- object@sensitivity$n[cells, radiationTypes , drop=drop]
	}
	if (object@datasetType == "perturbation" | object@datasetType == "both") {
	    object@perturbation$n <- object@perturbation$n[cells,radiationTypes, , drop=drop]
    }
      return(object)
}
### TODO:: Add updating of sensitivity Number tables
#' @importFrom CoreGx updateCellId
updateCellId <- function(object, new.ids = vector("character")){
  CoreGx::updateCellId(object, new.ids)
}

# updateFeatureNames <- function(rSet, new.ids = vector("character")){
#
#   if (length(new.ids)!=nrow(cellInfo(rSet))){
#     stop("Wrong number of cell identifiers")
#   }
#
#   if(rSet@datasetType=="sensitivity"|rSet@datasetType=="both"){
#     myx <- match(sensitivityInfo(rSet)[,"cellid"],rownames(cellInfo(rSet)))
#     sensitivityInfo(rSet)[,"cellid"] <- new.ids[myx]
#
#   }
#
#   rSet@molecularProfiles <- lapply(rSet@molecularProfiles, function(eset){
#
#     myx <- match(pData(eset)[["cellid"]],rownames(cellInfo(rSet)))
#     pData(eset)[["cellid"]]  <- new.ids[myx]
#     return(eset)
#       })
#   myx <- match(rownames(rSet@curation$cell),rownames(cellInfo(rSet)))
#   rownames(rSet@curation$cell) <- new.ids[myx]
#   rownames(rSet@curation$tissue) <- new.ids[myx]
#   if (dim(pertNumber(rSet))[[1]]>0){
#     myx <- match(dimnames(pertNumber(rSet))[[1]], rownames(cellInfo(rSet)))
#     dimnames(pertNumber(rSet))[[1]] <- new.ids[myx]
#   }
#   if (nrow(sensNumber(rSet))>0){
#     myx <- match(rownames(sensNumber(rSet)), rownames(cellInfo(rSet)))
#     rownames(sensNumber(rSet)) <- new.ids[myx]
#   }
#   rownames(cellInfo(rSet)) <- new.ids
#   return(rSet)
#
# }

### TODO:: Add updating of sensitivity Number tables
updateRadId <- function(rSet, new.ids = vector("character")){

  if (length(new.ids)!=nrow(radiationInfo(rSet))){
     stop("Wrong number of radiation identifiers")
  }

   if(rSet@datasetType=="sensitivity"|rSet@datasetType=="both"){
     myx <- match(sensitivityInfo(rSet)[,"radiation.type"],rownames(radiationInfo(rSet)))
     sensitivityInfo(rSet)[,"radiation.type"] <- new.ids[myx]

   }
   if(rSet@datasetType=="perturbation"|rSet@datasetType=="both"){
     rSet@molecularProfiles <- lapply(rSet@molecularProfiles, function(SE){

       myx <- match(SummarizedExperiment::colData(SE)[["drugid"]],rownames(drugInfo(object)))
       SummarizedExperiment::colData(SE)[["drugid"]]  <- new.ids[myx]
       return(SE)
     })
   }


   if(any(duplicated(new.ids))){
     warning("Duplicated ids passed to updateDrugId. Merging old ids into the same identifier")

     if(ncol(sensNumber(rSet))>0){
       sensMatch <- match(colnames(sensNumber(rSet)), rownames(radiationInfo(rSet)))
     }
     if(dim(pertNumber(rSet))[[2]]>0){
       pertMatch <- match(dimnames(pertNumber(rSet))[[2]], rownames(radiationInfo(rSet)))
     }
     curMatch <- match(rownames(rSet@curation$radiation),rownames(radiationInfo(rSet)))

     duplId <- unique(new.ids[duplicated(new.ids)])
     for(id in duplId){

       if (ncol(sensNumber(rSet))>0){
         myx <- which(new.ids[sensMatch] == id)
         sensNumber(rSet)[,myx[1]] <- apply(sensNumber(rSet)[,myx], 1, sum)
         sensNumber(rSet) <- sensNumber(rSet)[,-myx[-1]]
         sensMatch <- sensMatch[-myx[-1]]
       }
       if (dim(pertNumber(rSet))[[2]]>0){
         myx <- which(new.ids[pertMatch] == id)
         pertNumber(rSet)[,myx[1],] <- apply(pertNumber(rSet)[,myx,], c(1,3), sum)
         pertNumber(rSet) <- pertNumber(rSet)[,-myx[-1],]
         pertMatch <- pertMatch[-myx[-1]]
       }

       myx <- which(new.ids[curMatch] == id)
       rSet@curation$radiation[myx[1],] <- apply(rSet@curation$radiation[myx,], 2, paste, collapse="///")
       rSet@curation$radiation <- rSet@curation$radiation[-myx[-1],]
       curMatch <- curMatch[-myx[-1]]

       myx <- which(new.ids == id)
       radiationInfo(rSet)[myx[1],] <- apply(radiationInfo(rSet)[myx,], 2, paste, collapse="///")
       radiationInfo(rSet) <- radiationInfo(rSet)[-myx[-1],]
       new.ids <- new.ids[-myx[-1]]
       if(ncol(sensNumber(rSet))>0){
         sensMatch <- match(colnames(sensNumber(rSet)), rownames(radiationInfo(rSet)))
       }
       if(dim(pertNumber(rSet))[[2]]>0){
         pertMatch <- match(dimnames(pertNumber(rSet))[[2]], rownames(radiationInfo(rSet)))
       }
       curMatch <- match(rownames(rSet@curation$radiation),rownames(radiationInfo(rSet)))
     }
   } else {
     if (dim(pertNumber(rSet))[[2]]>0){
       pertMatch <- match(dimnames(pertNumber(rSet))[[2]], rownames(radiationInfo(rSet)))
     }
     if (ncol(sensNumber(rSet))>0){
       sensMatch <- match(colnames(sensNumber(rSet)), rownames(radiationInfo(rSet)))
     }
     curMatch <- match(rownames(rSet@curation$radiation),rownames(radiationInfo(rSet)))
   }

   if (dim(pertNumber(rSet))[[2]]>0){
     dimnames(pertNumber(rSet))[[2]] <- new.ids[pertMatch]
   }
   if (ncol(sensNumber(rSet))>0){
     colnames(sensNumber(rSet)) <- new.ids[sensMatch]
   }
   #rownames(rSet@curation$radiation) <- new.ids[curMatch]
   rownames(radiationInfo(rSet)) <- new.ids


   return(rSet)
 }

.summarizeSensitivityNumbers <- function(object) {

  if (object@datasetType != "sensitivity" && object@datasetType != "both") {
    stop ("Data type must be either sensitivity or both")
  }

  ## unique radiation identifiers
  # radiationn <- sort(unique(object@sensitivity$info[ , "radiation.type"]))

  ## consider all radiations
  radiationn <- rownames(object@radiation)

  ## unique radiation identifiers
  # celln <- sort(unique(object@sensitivity$info[ , "cellid"]))

  ## consider all cell lines
  celln <- rownames(object@cell)

  sensitivity.info <- matrix(0, nrow=length(celln), ncol=length(radiationn), dimnames=list(celln, radiationn))
  radiation.types <- object@sensitivity$info[ , "radiation.type"]
  cellids <- object@sensitivity$info[ , "cellid"]
  cellids <- cellids[grep("///", radiation.types, invert=TRUE)]
  radiation.types <- radiation.types[grep("///", radiation.types, invert=TRUE)]


  tt <- table(cellids, radiation.types)
  sensitivity.info[rownames(tt), colnames(tt)] <- tt

    return(sensitivity.info)
}

.summarizeMolecularNumbers <- function(object) {

  ## consider all molecular types
  mDT <- mDataNames(object)

  ## consider all cell lines
  celln <- rownames(object@cell)

  molecular.info <- matrix(0, nrow=length(celln), ncol=length(mDT), dimnames=list(celln, mDT))

  for(mDataType in mDT) {
    tt <- table(phenoInfo(object, mDataType)$cellid)
    molecular.info[names(tt), mDataType] <- tt

  }
  return(molecular.info)
}


.summarizePerturbationNumbers <- function(object) {

  if (object@datasetType != "perturbation" && object@datasetType != "both") {
    stop ("Data type must be either perturbation or both")
  }

  ## unique radiation identifiers
  # radiationn <- sort(unique(unlist(lapply(object@molecularProfiles, function (x) {
  #   res <- NULL
  #   if (nrow(pData(x)) > 0 & "radiation.type" %in% colnames(pData(x))) {
  #     res <- pData(x)[ , "radiation.type"]
  #   }
  #   return (res)
  # }))))

  ## consider all radiations
  radiationn <- rownames(object@radiation)

  ## unique cell line identifiers
  # celln <- sort(unique(unlist(lapply(object@molecularProfiles, function (x) {
  #   res <- NULL
  #   if (nrow(pData(x)) > 0 & "cellid" %in% colnames(pData(x))) {
  #     res <- pData(x)[ , "cellid"]
  #   }
  #   return (res)
  # }))))

  ## consider all cell lines
  celln <- rownames(object@cell)

  perturbation.info <- array(0, dim=c(length(celln), length(radiationn), length(object@molecularProfiles)), dimnames=list(celln, radiationn, names((object@molecularProfiles))))

  for (i in seq_len(length(object@molecularProfiles))) {
    if (nrow(SummarizedExperiment::colData(object@molecularProfiles[[i]])) > 0 && all(is.element(c("cellid", "drugid"), colnames(SummarizedExperiment::colData(object@molecularProfiles[[i]]))))) {
      tt <- table(SummarizedExperiment::colData(object@molecularProfiles[[i]])[ , "cellid"], SummarizedExperiment::colData(object@molecularProfiles[[i]])[ , "drugid"])
      perturbation.info[rownames(tt), colnames(tt), names(object@molecularProfiles)[i]] <- tt
    }
  }

    return(perturbation.info)
}

#' A function to verify the structure of a RadioSet
#'
#' This function checks the structure of a PharamcoSet, ensuring that the
#' correct annotations are in place and all the required slots are filled so
#' that matching of cells and radiations can be properly done across different types
#' of data and with other studies.
#'
#' @examples

#'
#' checkRSetStructure(Cleveland_small)
#'
#' @param object A \code{RadiOSet} object
#' @param plotDist Should the function also plot the distribution of molecular data?
#' @param result.dir The path to the directory for saving the plots as a string, defaults to `tempdir()``
#' @return Prints out messages whenever describing the errors found in the structure of the pset object passed in.
#' @export
#' @importFrom graphics hist
#' @importFrom grDevices dev.off pdf
checkRSetStructure <-
  function(object, plotDist=FALSE, result.dir=tempdir()) {
    # Make directory to store results if it doesn't exist
    if(!file.exists(result.dir) & plotDist) { dir.create(result.dir, showWarnings=FALSE, recursive=TRUE) }

    #####
    # Checking molecularProfiles
    #####
    # Can this be parallelized or does it mess with the order of printing warnings?
    for( i in seq_along(object@molecularProfiles)) {
      profile <- object@molecularProfiles[[i]]
      nn <- names(object@molecularProfiles)[i]

      # Testing plot rendering for rna and rnaseq
      if((S4Vectors::metadata(profile)$annotation == "rna" | S4Vectors::metadata(profile)$annotation == "rnaseq") & plotDist)
      {
        pdf(file=file.path(result.dir, sprintf("%s.pdf", nn)))
        hist(assays(profile)[[1]], breaks = 100)
        dev.off()
      }


      ## Test if sample and feature annotations dimensions match the assay
      warning(ifelse(nrow(rowData(profile)) != nrow(assays(profile)[[1]]),
                     sprintf("%s: number of features in fData is different from
                             SummarizedExperiment slots", nn),
                     sprintf("%s: rowData dimension is OK", nn)
      )
      )
      warning(ifelse(nrow(colData(profile)) != ncol(assays(profile)[[1]]),
                     sprintf("%s: number of cell lines in pData is different
                             from expression slots", nn),
                     sprintf("%s: colData dimension is OK", nn)
      )
      )


      # Checking sample metadata for required columns
      warning(ifelse("cellid" %in% colnames(colData(profile)), "",
                     sprintf("%s: cellid does not exist in colData (samples)
                             columns", nn)))
      warning(ifelse("batchid" %in% colnames(colData(profile)), "",
                     sprintf("%s: batchid does not exist in colData (samples)
                             columns", nn)))

      # Checking mDataType of the SummarizedExperiment for required columns
      if(S4Vectors::metadata(profile)$annotation == "rna" |
         S4Vectors::metadata(profile)$annotation == "rnaseq")
      {
        warning(ifelse("BEST" %in% colnames(rowData(profile)), "BEST is OK",
                       sprintf("%s: BEST does not exist in rowData (features)
                               columns", nn)))
        warning(ifelse("Symbol" %in% colnames(rowData(profile)), "Symbol is OK",
                       sprintf("%s: Symbol does not exist in rowData (features)
                               columns", nn)))
      }

      # Check that all cellids from the object are included in molecularProfiles
      if("cellid" %in% colnames(rowData(profile))) {
        if(!all(colData(profile)[,"cellid"] %in% rownames(object@cell))) {
          warning(sprintf("%s: not all the cell lines in this profile are in
                          cell lines slot", nn))
        }
      }else {
        warning(sprintf("%s: cellid does not exist in colData (samples)", nn))
      }
    }

    ###
    # CHECKING CELL
    ###
    if("tissueid" %in% colnames(object@cell)) {
      if("unique.tissueid" %in% colnames(object@curation$tissue))
      {
        if(length(intersect(rownames(object@curation$tissue), rownames(object@cell))) != nrow(object@cell)) {
          message("rownames of curation tissue slot should be the same as cell slot (curated cell ids)")
        } else{
          if(length(intersect(object@cell$tissueid, object@curation$tissue$unique.tissueid)) != length(table(object@cell$tissueid))){
            message("tissueid should be the same as unique tissue id from tissue curation slot")
          }
        }
      } else {
        message("unique.tissueid which is curated tissue id across data set should be a column of tissue curation slot")
      }
      if(any(is.na(object@cell[,"tissueid"]) | object@cell[,"tissueid"]=="", na.rm=TRUE)){
        message(sprintf("There is no tissue type for this cell line(s): %s", paste(rownames(object@cell)[which(is.na(object@cell[,"tissueid"]) | object@cell[,"tissueid"]=="")], collapse=" ")))
      }
    } else {
      warning("tissueid does not exist in cell slot")
    }

    if("unique.cellid" %in% colnames(object@curation$cell)) {
      if(length(intersect(object@curation$cell$unique.cellid, rownames(object@cell))) != nrow(object@cell)) {
        print("rownames of cell slot should be curated cell ids")
      }
    } else {
      print("unique.cellid which is curated cell id across data set should be a column of cell curation slot")
    }
#     if("cellid" %in% colnames(object@cell)) {
#       if(length(intersect(object@curation$cell$cellid, rownames(object@cell))) != nrow(object@cell)) {
#         print("values of cellid column should be curated cell line ids")
#       }
#     } else {
#       print("cellid which is curated cell id across data set should be a column of cell slot")
#     }

    if(length(intersect(rownames(object@curation$cell), rownames(object@cell))) != nrow(object@cell)) {
      print("rownames of curation cell slot should be the same as cell slot (curated cell ids)")
    }

    ##TODO:: Determine if object@curation$radiation is intended to be defined in object objects
    ## It is not currently defined in object class defintion of this package
    #if("unique.radiation.type" %in% colnames(object@curation$radiation)) {
    #  if(length(intersect(object@curation$radiation$unique.radiation.type, rownames(object@radiation))) != nrow(object@radiation)) {
    #    print("rownames of radiation slot should be curated radiation ids")
    #  }
    #} else {
    #  print("unique.radiation.type which is curated radiation id across data set should be a column of radiation curation slot")
    #}

#     if("radiation.type" %in% colnames(object@radiation)) {
#       if(length(intersect(object@curation$radiation$radiation.type, rownames(object@radiation))) != nrow(object@radiation)) {
#         print("values of radiation.type column should be curated radiation ids")
#       }
#     } else {
#       print("radiation.type which is curated radiation id across data set should be a column of radiation slot")
#     }

    if(length(intersect(rownames(object@curation$cell), rownames(object@cell))) != nrow(object@cell)) {
      print("rownames of curation radiation slot should be the same as radiation slot (curated radiation ids)")
    }

    if(!is(object@cell, "data.frame")) {
      warning("cell slot class type should be dataframe")
    }
    if(!is(object@radiation, "data.frame")) {
      warning("radiation slot class type should be dataframe")
    }
    if(object@datasetType %in% c("sensitivity", "both"))
    {
      if(!is(object@sensitivity$info, "data.frame")) {
        warning("sensitivity info slot class type should be dataframe")
      }
      if("cellid" %in% colnames(object@sensitivity$info)) {
        if(!all(object@sensitivity$info[,"cellid"] %in% rownames(object@cell))) {
          warning("not all the cell lines in sensitivity data are in cell slot")
        }
      }else {
        warning("cellid does not exist in sensitivity info")
      }

      ###
      # CHECKING RADIATION
      ###
      if("radiation.type" %in% colnames(object@sensitivity$info)) {
        radiation.ids <- unique(object@sensitivity$info[,"radiation.type"])
        radiation.ids <- radiation.ids[grep("///",radiation.ids, invert=TRUE)]
        if(!all(radiation.ids %in% rownames(object@radiation))) {
          print("not all the radiations in sensitivity data are in radiation slot")
        }
      }else {
        warning("radiation.type does not exist in sensitivity info")
      }

      if(any(!is.na(object@sensitivity$raw))) {
        if(!all(dimnames(object@sensitivity$raw)[[1]] %in% rownames(object@sensitivity$info))) {
          warning("For some experiments there is raw sensitivity data but no experimet information in sensitivity info")
        }
      }
      if(!all(rownames(object@sensitivity$profiles) %in% rownames(object@sensitivity$info))) {
        warning("For some experiments there is sensitivity profiles but no experimet information in sensitivity info")
      }
    }
  }

