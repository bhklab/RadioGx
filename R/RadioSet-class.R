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
.RadioSet <- setClass("RadioSet", slots=list(radiation="data.frame"),
                      contains = "CoreSet")

# The default constructor above does a poor job of explaining the required structure of a RadioSet.
# The constructor function defined below guides the user into providing the required components of the curation and senstivity lists
# and hides the annotation slot which the user does not need to manually fill.
# This also follows the design of the Expression Set class.

### -------------------------------------------------------------------------
### Constructor -------------------------------------------------------------
### -------------------------------------------------------------------------

#' RadioSet constructor
#'
#' A constructor that simplifies the process of creating RadioSets, as well
#' as creates empty objects for data not provided to the constructor. Only
#' objects returned by this constructor are expected to work with the RadioSet
#' methods. For a much more detailed instruction on creating RadioSets, please
#' see the "CreatingRadioSet" vignette.
#'
##TODO:: Figure out how to inherit constructor parameters?
#' @param name A \code{character} string detailing the name of the dataset
#' @param molecularProfiles A \code{list} of ExpressionSet objects containing
#'   molecular profiles
#' @param cell A \code{data.frame} containg the annotations for all the cell
#'   lines profiled in the data set, across all data types
#' @param radiation A \code{data.frame} containg the annotations for all the radiations
#'   profiled in the data set, across all data types
#' @param sensitivityInfo A \code{data.frame} containing the information for the
#'   sensitivity experiments
#' @param sensitivityRaw A 3 Dimensional \code{array} contaning the raw radiation
#'   dose – response data for the sensitivity experiments
#' @param sensitivityProfiles \code{data.frame} containing radiation sensitivity profile
#'   statistics such as IC50 and AUC
#' @param sensitivityN,perturbationN A \code{data.frame} summarizing the
#'   available sensitivity/perturbation data
#' @param curationCell,curationTissue A \code{data.frame} mapping
#'   the names for radiations, cells and tissues used in the data set to universal
#'   identifiers used between different RadioSet objects
#' @param datasetType A \code{character} string of 'sensitivity',
#'   'perturbation', or both detailing what type of data can be found in the
#'   RadioSet, for proper processing of the data
#' @param verify \code{boolean} Should the function verify the RadioSet and
#'   print out any errors it finds after construction?

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

    ## TODO:: If the colnames and rownames are not found below, it will fill with NAs. This is undersirable behaviour.
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
    sensNumber(rSet) <- .summarizeSensitivityNumbers(rSet)
  }
    if(length(perturbationN) == 0  & datasetType %in% c("perturbation", "both")) {
      pertNumber(rSet) <- .summarizePerturbationNumbers(rSet)
    }
  return(rSet)
}


# Constructor Helper Functions ----------------------------------------------

.summarizeSensitivityNumbers <- function(object) {
  
  if (datasetType(object) != "sensitivity" && datasetType(object) != "both") {
    stop ("Data type must be either sensitivity or both")
  }
  
  ## unique radiation identifiers
  # radiationn <- sort(unique(sensitivityInfo(object)[ , "radiation.type"]))
  
  ## consider all radiations
  radiationn <- rownames(radiationInfo(object))
  
  ## unique radiation identifiers
  # celln <- sort(unique(sensitivityInfo(object)[ , "cellid"]))
  
  ## consider all cell lines
  celln <- rownames(cellInfo(object))
  
  sensitivity.info <- matrix(0, nrow=length(celln), ncol=length(radiationn), dimnames=list(celln, radiationn))
  radiation.types <- sensitivityInfo(object)[ , "radiation.type"]
  cellids <- sensitivityInfo(object)[ , "cellid"]
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
  celln <- rownames(cellInfo(object))
  
  molecular.info <- matrix(0, nrow=length(celln), ncol=length(mDT), dimnames=list(celln, mDT))
  
  for(mDataType in mDT) {
    tt <- table(phenoInfo(object, mDataType)$cellid)
    molecular.info[names(tt), mDataType] <- tt
    
  }
  return(molecular.info)
}


.summarizePerturbationNumbers <- function(object) {
  
  if (datasetType(object) != "perturbation" && datasetType(object) != "both") {
    stop ("Data type must be either perturbation or both")
  }
  
  radiationn <- rownames(radiationInfo(object))
  
  celln <- rownames(cellInfo(object))
  
  perturbation.info <- array(0, dim=c(length(celln), length(radiationn), length(molecularProfilesSlot(object))), dimnames=list(celln, radiationn, names((molecularProfilesSlot(object)))))
  
  for (i in seq_len(length(molecularProfilesSlot(object)))) {
    if (nrow(SummarizedExperiment::colData(molecularProfilesSlot(object)[[i]])) > 0 && all(is.element(c("cellid", "drugid"), colnames(SummarizedExperiment::colData(molecularProfilesSlot(object)[[i]]))))) {
      tt <- table(SummarizedExperiment::colData(molecularProfilesSlot(object)[[i]])[ , "cellid"], SummarizedExperiment::colData(molecularProfilesSlot(object)[[i]])[ , "drugid"])
      perturbation.info[rownames(tt), colnames(tt), names(molecularProfilesSlot(object))[i]] <- tt
    }
  }
  
  return(perturbation.info)
}

### -------------------------------------------------------------------------
### Class Validity ----------------------------------------------------------
### -------------------------------------------------------------------------

#' A function to verify the structure of a RadioSet
#'
#' This function checks the structure of a PharamcoSet, ensuring that the
#' correct annotations are in place and all the required slots are filled so
#' that matching of cells and radiations can be properly done across different 
#' types of data and with other studies.
#'
#' @examples
#' checkRSetStructure(clevelandSmall)
#'
#' @param object A \code{RadioSet} object
#' @param plotDist Should the function also plot the distribution of molecular 
#'     data?
#' @param result.dir The path to the directory for saving the plots as a string, 
#'     defaults to `tempdir()``
#' 
#' @return Prints out messages whenever describing the errors found in the 
#'     structure of the pset object passed in.
#' 
#' @importFrom graphics hist
#' @importFrom grDevices dev.off pdf
#' @export
checkRSetStructure <- function(object, plotDist=FALSE, result.dir=tempdir()) {
    # Make directory to store results if it doesn't exist
    if(!file.exists(result.dir) & plotDist) { dir.create(result.dir, showWarnings=FALSE, recursive=TRUE) }
    
    #####
    # Checking molecularProfiles
    #####
    # Can this be parallelized or does it mess with the order of printing warnings?
    for( i in seq_along(molecularProfilesSlot(object))) {
      profile <- molecularProfilesSlot(object)[[i]]
      nn <- names(molecularProfilesSlot(object))[i]
      
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
        if(!all(colData(profile)[,"cellid"] %in% rownames(cellInfo(object)))) {
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
    if("tissueid" %in% colnames(cellInfo(object))) {
      if("unique.tissueid" %in% colnames(curation(object)$tissue))
      {
        if(length(intersect(rownames(curation(object)$tissue), rownames(cellInfo(object)))) != nrow(cellInfo(object))) {
          message("rownames of curation tissue slot should be the same as cell slot (curated cell ids)")
        } else{
          if(length(intersect(cellInfo(object)$tissueid, curation(object)$tissue$unique.tissueid)) != length(table(cellInfo(object)$tissueid))){
            message("tissueid should be the same as unique tissue id from tissue curation slot")
          }
        }
      } else {
        message("unique.tissueid which is curated tissue id across data set should be a column of tissue curation slot")
      }
      if(any(is.na(cellInfo(object)[,"tissueid"]) | cellInfo(object)[,"tissueid"]=="", na.rm=TRUE)){
        message(sprintf("There is no tissue type for this cell line(s): %s", paste(rownames(cellInfo(object))[which(is.na(cellInfo(object)[,"tissueid"]) | cellInfo(object)[,"tissueid"]=="")], collapse=" ")))
      }
    } else {
      warning("tissueid does not exist in cell slot")
    }
    
    if("unique.cellid" %in% colnames(curation(object)$cell)) {
      if(length(intersect(curation(object)$cell$unique.cellid, rownames(cellInfo(object)))) != nrow(cellInfo(object))) {
        message("rownames of cell slot should be curated cell ids")
      }
    } else {
      message("unique.cellid which is curated cell id across data set should be a column of cell curation slot")
    }
    
    if(length(intersect(rownames(curation(object)$cell), rownames(cellInfo(object)))) != nrow(cellInfo(object))) {
      message("rownames of curation cell slot should be the same as cell slot (curated cell ids)")
    }
    
    if(length(intersect(rownames(curation(object)$cell), rownames(cellInfo(object)))) != nrow(cellInfo(object))) {
      message("rownames of curation radiation slot should be the same as radiation slot (curated radiation ids)")
    }
    
    if(!is(cellInfo(object), "data.frame")) {
      warning("cell slot class type should be dataframe")
    }
    if(!is(radiationInfo(object), "data.frame")) {
      warning("radiation slot class type should be dataframe")
    }
    if(datasetType(object) %in% c("sensitivity", "both"))
    {
      if(!is(sensitivityInfo(object), "data.frame")) {
        warning("sensitivity info slot class type should be dataframe")
      }
      if("cellid" %in% colnames(sensitivityInfo(object))) {
        if(!all(sensitivityInfo(object)[,"cellid"] %in% rownames(cellInfo(object)))) {
          warning("not all the cell lines in sensitivity data are in cell slot")
        }
      }else {
        warning("cellid does not exist in sensitivity info")
      }
      
      ###
      # CHECKING RADIATION
      ###
      if("radiation.type" %in% colnames(sensitivityInfo(object))) {
        radiation.ids <- unique(sensitivityInfo(object)[,"radiation.type"])
        radiation.ids <- radiation.ids[grep("///",radiation.ids, invert=TRUE)]
        if(!all(radiation.ids %in% rownames(radiationInfo(object)))) {
          message("not all the radiations in sensitivity data are in radiation slot")
        }
      }else {
        warning("radiation.type does not exist in sensitivity info")
      }
      
      if(any(!is.na(sensitivityRaw(object)))) {
        if(!all(dimnames(sensitivityRaw(object))[[1]] %in% rownames(sensitivityInfo(object)))) {
          warning("For some experiments there is raw sensitivity data but no experimet information in sensitivity info")
        }
      }
      if(!all(rownames(sensitivityProfiles(object)) %in% rownames(sensitivityInfo(object)))) {
        warning("For some experiments there is sensitivity profiles but no experimet information in sensitivity info")
      }
    }
  }

### -------------------------------------------------------------------------
### Method Definitions ------------------------------------------------------
### -------------------------------------------------------------------------

#' Show a RadioSet
#'
#' @examples
#' data(clevelandSmall)
#' clevelandSmall
#'
#' @param object A \code{RadioSet} object
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
        if("dna" %in% names(molecularProfilesSlot(object))){
          cat("DNA: \n");cat("\tDim: ", dim(
            molecularProfiles(object, mDataType="dna")), "\n")}
      if("rna" %in% names(molecularProfilesSlot(object))){cat("RNA: \n");
        cat("\tDim: ", dim(molecularProfiles(object, mDataType="rna")), "\n")}
      if("rnaseq" %in% names(molecularProfilesSlot(object))){cat("RNASeq: \n");
        cat("\tDim: ", dim(molecularProfiles(object, mDataType="rnaseq")),
            "\n")}
      if("snp" %in% names(molecularProfilesSlot(object))){cat("SNP: \n");
        cat("\tDim: ", dim(molecularProfiles(object, mDataType="snp")), "\n")}
      if("cnv" %in% names(molecularProfilesSlot(object))){cat("CNV: \n");
        cat("\tDim: ", dim(molecularProfiles(object, mDataType="cnv")), "\n")}
        cat("Drug pertubation: \n")
        cat("\tPlease look at pertNumber(rSet) to determine number of
            experiments for each radiation-cell combination.\n")
        cat("Drug sensitivity: \n")
        cat("\tNumber of Experiments: ",nrow(sensitivityInfo(object)),"\n")
        cat("\tPlease look at sensNumber(rSet) to determine number of
            experiments for each radiation-cell combination.\n")
})

#' Get the dimensions of a RadioSet
#'
#' @examples
#' data(clevelandSmall)
#' dim(clevelandSmall)
#'
#' @param x RadioSet
#' @return A named vector with the number of Cells and Drugs in the RadioSet
#' @export
setMethod("dim", signature=signature(x="RadioSet"), function(x){
  return(c(Cells=length(cellNames(x)), Radiation=length(radiationTypes(x))))
})