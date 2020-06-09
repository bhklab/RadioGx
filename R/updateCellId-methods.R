### TODO:: Add updating of sensitivity Number tables
#' @importFrom CoreGx updateCellId
updateCellId <- function(object, new.ids=vector("character")){
    CoreGx::updateCellId(object, new.ids)
}