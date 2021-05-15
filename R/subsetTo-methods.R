#' `[`
#'
#' @param x a \code{RadioSet} object
#' @param i Cell lines to keep in RSet
#' @param j Drugs to keep in RSet
#' @param ... further arguments
#' @param drop A boolean flag of whether to drop single dimensions or not
#' @return Returns the subsetted RSet
#' @export
setMethod(`[`, "RadioSet", function(x, i, j, ..., drop = FALSE) {
    if(is.character(i)&&is.character(j)){
        return(subsetTo(x, cells=i, radiations=j,  molecular.data.cells=i))
    }
    else if (is.numeric(i) && is.numeric(j) && (as.integer(i)==i) &&
            (as.integer(j)==j)){
        return(subsetTo(x, cells=cellNames(x)[i], radiations=radiationTypes(x)[j],
                        molecular.data.cells=cellNames(x)[i]))
    }
})

## FIXED? TODO:: Subset function breaks if it doesnt find cell line in sensitivity info
#' A function to subset a RadioSet to data containing only specified radiations,
#'   cells and genes
#'
#' This is the prefered method of subsetting a RadioSet. This function allows
#' abstraction of the data to the level of biologically relevant objects:
#'   radiations and cells. The function will automatically go through all of the
#' combined data in the RadioSet and ensure only the requested radiations
#' and cell lines are found in any of the slots. This allows quickly picking out
#' all the experiments for a radiation or cell of interest, as well removes the
#' need to keep track of all the metadata conventions between different
#' datasets.
#'
#' @examples
#' clevelandRadiationTypes  <- radiationTypes(clevelandSmall)
#' clevelandCells <- cellNames(clevelandSmall)
#' RSet <- subsetTo(clevelandSmall, radiationTypes = clevelandRadiationTypes[1],
#'   cells = clevelandCells[1])
#' RSet
#'
#' @param object A \code{RadioSet} to be subsetted
#' @param cells A list or vector of cell names as used in the dataset to which
#'   the object will be subsetted. If left blank, then all cells will be left in
#'   the dataset.
#' @param radiationTypes A list or vector of radiation names as used in the
#'   dataset to which the object will be subsetted. If left blank, then all
#'   radiationTypes will be left in the dataset.
#' @param molecular.data.cells A list or vector of cell names to keep in the
#'   molecular data
#' @param keep.controls If the dataset has perturbation type experiments, should
#'   the controls be kept in the dataset? Defaults to true.
#' @param ... Other arguments passed by other function within the package
#'
#' @return A RadioSet with only the selected radiation types and cells
#'
#' @importMethodsFrom CoreGx subsetTo
#' @export
setMethod("subsetTo",
          signature(object="RadioSet"),
          function(object , cells=NULL, radiationTypes=NULL, molecular.data.cells=NULL, keep.controls=TRUE, ...){
              .subsetToRadioSet(object, cells, radiationTypes, 
              molecular.data.cells, keep.controls, ...)
          })

# @param object A `RadioSet` to be subsetted
# @param cells A list or vector of cell names as used in the dataset to which
#   the object will be subsetted. If left blank, then all cells will be left in
#   the dataset.
# @param radiationTypes A list or vector of radiation names as used in the
#   dataset to which the object will be subsetted. If left blank, then all
#   radiationTypes will be left in the dataset.
# @param molecular.data.cells A list or vector of cell names to keep in the
#   molecular data
# @param keep.controls If the dataset has perturbation type experiments, should
#   the controls be kept in the dataset? Defaults to true.
# @param ... Other arguments passed by other function within the package
# @return A RadioSet with only the selected radiation types and cells
#' @importFrom CoreGx .unionList .message .warning .error
#' @keywords internals
.subsetToRadioSet <- function(object,
                     cells=NULL,
                     radiationTypes=NULL,
                     molecular.data.cells=NULL,
                     keep.controls=TRUE,
                     ...)
{
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
    
    ### the function missing does not work as expected in the context below,
    ### because the arguments are passed to the anonymous
    ### function in lapply, so it does not recognize them as missing
    
    molecularProfilesSlot(object) <- 
        lapply(molecularProfilesSlot(object), 
            function(SE, cells, radiationTypes, molecular.data.cells) {                          
                molecular.data.type <- 
                    if (length(grep("rna", S4Vectors::metadata(SE)$annotation) > 0))
                        "rna"
                    else
                        S4Vectors::metadata(SE)$annotation
                if (length(grep(molecular.data.type, names(molecular.data.cells))) > 0)
                    cells <- molecular.data.cells[[molecular.data.type]]
                
                column_indices <- NULL
                
                if (length(cells)==0 && length(radiationTypes) == 0) {
                    column_indices <- seq_len(ncol(SE)) # This still returns the number of samples in an SE, but without a label
                }
                if (length(cells) == 0 && datasetType(object) == "sensitivity") {
                    column_indices <- seq_len(ncol(SE))
                }
                
                cell_line_index <- NULL
                if(length(cells)!=0) {
                    if (!all(cells %in% cellNames(object))) {
                        stop("Some of the cell names passed to function did not match to names in the RadoSet. Please ensure you are using cell names as returned by the cellNames function")
                    }
                    cell_line_index <- which(SummarizedExperiment::colData(SE)[["cellid"]] %in% cells)
                }
                radiationTypes_index <- NULL
                if(datasetType(object)=="perturbation" || datasetType(object)=="both"){
                    if(length(radiationTypes) != 0) {
                        if (!all(radiationTypes %in% radiationTypes(object))) {
                            stop("Some of the radiation types passed to function did not match
               to names in the RadioSet. Please ensure you are using radiation
               names as returned by the radiations function")
                                                        }
                                                        radiationTypes_index <- which(SummarizedExperiment::colData(SE)[["radiation.type"]] %in% radiationTypes)
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
    
    if ((datasetType(object) == "sensitivity" | datasetType(object) == "both") & length(exps) != 0) {
        sensitivityInfo(object) <- sensitivityInfo(object)[exps, , drop=drop]
        rownames(sensitivityInfo(object)) <- names(exps)
        if(length(sensitivityRaw(object)) > 0) {
            sensitivityRaw(object) <- sensitivityRaw(object)[exps, , , drop=drop]
            dimnames(sensitivityRaw(object))[[1]] <- names(exps)
        }
        sensitivityProfiles(object) <- sensitivityProfiles(object)[exps, , drop=drop]
        rownames(sensitivityProfiles(object)) <- names(exps)
        
        sensNumber(object) <- .summarizeSensitivityNumbers(object)
    }
    else if ((datasetType(object) == "sensitivity" | datasetType(object) == "both") & (length(radiationTypes) != 0 | length(cells) != 0)) {
        
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
        sensitivitySlot(object)[names(sensitivitySlot(object))[names(sensitivitySlot(object))!="n"]] <-
            lapply(sensitivitySlot(object)[names(sensitivitySlot(object))[names(sensitivitySlot(object))!="n"]],
                   function(x,i, drop){
                       
                       if (length(dim(x))==2){
                           return(x[i,,drop=drop])
                       }
                       if (length(dim(x))==3){
                           return(x[i,,,drop=drop])
                       }
                   }, i=row_indices, drop=drop)
    }
    
    if (length(radiationTypes)==0) {
        if(datasetType(object) == "sensitivity" | datasetType(object) == "both"){
            radiationTypes <- unique(sensitivityInfo(object)[["radiation.type"]])
        }
        if(datasetType(object) == "perturbation" | datasetType(object) == "both"){
            radiationTypes <- union(radiationTypes, na.omit(.unionList(lapply(molecularProfilesSlot(object), function(SE){unique(colData(SE)[["radiation.type"]])}))))
        }
    }
    if (length(cells)==0) {
        cells <- union(cells, na.omit(.unionList(lapply(molecularProfilesSlot(object), function(SE){unique(colData(SE)[["cellid"]])}))))
        if (datasetType(object) =="sensitivity" | datasetType(object) == "both"){
            cells <- union(cells, sensitivityInfo(object)[["cellid"]])
        }
    }
    radiationInfo(object) <- radiationInfo(object)[radiationTypes , , drop=drop]
    cellInfo(object) <- cellInfo(object)[cells , , drop=drop]
    curation(object)$radiation <- curation(object)$radiation[radiationTypes , , drop=drop]
    curation(object)$cell <- curation(object)$cell[cells , , drop=drop]
    curation(object)$tissue <- curation(object)$tissue[cells , , drop=drop]
    if (datasetType(object) == "sensitivity" | datasetType(object) == "both"  & length(exps) == 0) {
        sensNumber(object) <- sensNumber(object)[cells, radiationTypes , drop=drop]
    }
    if (datasetType(object) == "perturbation" | datasetType(object) == "both") {
        object@perturbation$n <- object@perturbation$n[cells,radiationTypes, , drop=drop]
    }
    return(object)
}