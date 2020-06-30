### TODO:: Add updating of sensitivity Number tables
updateRadId <- function(object, new.ids = vector("character")){
    
    if (length(new.ids) != nrow(radiationInfo(object))){
        stop("Wrong number of radiation identifiers")
    }
    
    if(datasetType(object)=="sensitivity"|datasetType(object)=="both"){
        myx <- match(sensitivityInfo(object)[,"radiation.type"],rownames(radiationInfo(object)))
        sensitivityInfo(object)[,"radiation.type"] <- new.ids[myx]
        
    }
    if(datasetType(object)=="perturbation"|datasetType(object)=="both"){
        molecularProfilesSlot(object) <- lapply(molecularProfilesSlot(object), function(SE){
            
            myx <- match(SummarizedExperiment::colData(SE)[["radiation.type"]],rownames(radiationInfo(object)))
            SummarizedExperiment::colData(SE)[["radiation.type"]]  <- new.ids[myx]
            return(SE)
        })
    }
    
    if(any(duplicated(new.ids))){
        warning("Duplicated ids passed to updateRadId. Merging old ids into the same identifier")
        
        if(ncol(sensNumber(object))>0){
            sensMatch <- match(colnames(sensNumber(object)), rownames(radiationInfo(object)))
        }
        if(dim(pertNumber(object))[[2]]>0){
            pertMatch <- match(dimnames(pertNumber(object))[[2]], rownames(radiationInfo(object)))
        }
        curMatch <- match(rownames(curation(object)$radiation),rownames(radiationInfo(object)))
        
        duplId <- unique(new.ids[duplicated(new.ids)])
        for(id in duplId){
            
            if (ncol(sensNumber(object))>0){
                myx <- which(new.ids[sensMatch] == id)
                sensNumber(object)[,myx[1]] <- apply(sensNumber(object)[,myx], 1, sum)
                sensNumber(object) <- sensNumber(object)[,-myx[-1]]
                sensMatch <- sensMatch[-myx[-1]]
            }
            if (dim(pertNumber(object))[[2]]>0){
                myx <- which(new.ids[pertMatch] == id)
                pertNumber(object)[,myx[1],] <- apply(pertNumber(object)[,myx,], c(1,3), sum)
                pertNumber(object) <- pertNumber(object)[,-myx[-1],]
                pertMatch <- pertMatch[-myx[-1]]
            }
            
            myx <- which(new.ids[curMatch] == id)
            curation(object)$radiation[myx[1],] <- apply(curation(object)$radiation[myx,], 2, paste, collapse="///")
            curation(object)$radiation <- curation(object)$radiation[-myx[-1],]
            curMatch <- curMatch[-myx[-1]]
            
            myx <- which(new.ids == id)
            radiationInfo(object)[myx[1],] <- apply(radiationInfo(object)[myx,], 2, paste, collapse="///")
            radiationInfo(object) <- radiationInfo(object)[-myx[-1],]
            new.ids <- new.ids[-myx[-1]]
            if(ncol(sensNumber(object))>0){
                sensMatch <- match(colnames(sensNumber(object)), rownames(radiationInfo(object)))
            }
            if(dim(pertNumber(object))[[2]]>0){
                pertMatch <- match(dimnames(pertNumber(object))[[2]], rownames(radiationInfo(object)))
            }
            curMatch <- match(rownames(curation(object)$radiation),rownames(radiationInfo(object)))
        }
    } else {
        if (dim(pertNumber(object))[[2]]>0){
            pertMatch <- match(dimnames(pertNumber(object))[[2]], rownames(radiationInfo(object)))
        }
        if (ncol(sensNumber(object))>0){
            sensMatch <- match(colnames(sensNumber(object)), rownames(radiationInfo(object)))
        }
        curMatch <- match(rownames(curation(object)$radiation),rownames(radiationInfo(object)))
    }
    
    if (dim(pertNumber(object))[[2]]>0){
        dimnames(pertNumber(object))[[2]] <- new.ids[pertMatch]
    }
    if (ncol(sensNumber(object))>0){
        colnames(sensNumber(object)) <- new.ids[sensMatch]
    }
    rownames(radiationInfo(object)) <- new.ids
    
    
    return(object)
}