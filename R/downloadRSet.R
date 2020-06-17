#' Return a table of RadioSets available for download
#'
#' The function fetches a table of all RadioSets available for download from
#' the PharmacoGx server. The table includes the names of the PharamcoSet, the
#' types of data available in the object, and the date of last update.
#'
#' @examples
#' if (interactive()){
#' availableRSets()
#' }
#'
#' @param saveDir \code{character} Directory to save the table of rSets
#' @param fileName \code{character} The filename for the table of rSets
#' @param verbose \code{bool} Should status messages be printed during download.
#'
#' @return A data.frame with details about the available RadioSet objects
#'
#' @export
#' @import downloader
#' @importFrom utils read.csv write.table
availableRSets <- function(saveDir=tempdir(), fileName="availableRadioSets.csv", verbose=TRUE) {

    if (missing(saveDir) && verbose) {
        message("Downloading to temporary folder... Use saveDir parameter to
                save to a specific path")
        }

    if (!file.exists(saveDir)) {
        dir.create(saveDir, recursive = TRUE)
    }

    downloader::download("https://zenodo.org/record/3897599/files/availableRSets.csv?download=1",
                         destfile = file.path(saveDir, fileName),
                         quiet = !verbose)

    rSetTable <- read.csv(file.path(saveDir, fileName), header = TRUE, stringsAsFactors = FALSE)
    return(rSetTable)
}

#' Download a RadioSet object
#'
#' This function allows you to download a \code{RadioSet} object for use with this
#' package. The \code{RadioSets} have been extensively curated and organized within
#' a PharacoSet class, enabling use with all the analysis tools provided in
#' \code{PharmacoGx}.
#'
#' @examples
#' if (interactive()) {
#' drugMatrix_rat <- downloadRSet("Cleveland")
#' }
#'
#' @param name \code{Character} string, the name of the PhamracoSet to download.
#' @param saveDir \code{Character} string with the folder path where the
#'     RadioSet should be saved. Defaults to \code{'./rSets/'}. Will create
#'     directory if it does not exist.
#' @param rSetFileName \code{character} string, the file name to save the dataset under
#' @param verbose \code{bool} Should status messages be printed during download.
#'   Defaults to TRUE.
#'
#' @return A rSet object with the dataset, downloaded from our server
#'
#' @export
#' @import downloader
downloadRSet <- function(name, saveDir = tempdir(), rSetFileName = NULL, verbose = TRUE) {

    if (missing(saveDir)) {message("Downloading to temporary folder... Use saveDir parameter to save to a specific path")}
    rSetTable <- availableRSets(saveDir = saveDir)

    whichx <- match(name, rSetTable[, 1])
    if (is.na(whichx)) {
        stop('Unknown Dataset. Please use the availableRSets() function for the
         table of available RadicoSets.')
    }

    if (!file.exists(saveDir)) {
        dir.create(saveDir, recursive = TRUE)
    }

    if (is.null(rSetFileName)) {
        rSetFileName <- paste0(rSetTable[whichx,"RadioSet_name"], ".rds")
    }
    if (!file.exists(file.path(saveDir, rSetFileName))) {
        downloader::download(url = as.character(rSetTable[whichx, "URL"]),
                             destfile = file.path(saveDir, rSetFileName),
                             quiet = !verbose, mode='wb')
    }

    rSet <- readRDS(file.path(saveDir, rSetFileName))

    return(rSet)
}