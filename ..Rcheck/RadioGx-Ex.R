pkgname <- "RadioGx"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "RadioGx-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('RadioGx')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("RadioSet-class")
### * RadioSet-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RadioSet-class
### Title: A Class to Contain RadioGenomic datasets together with their
###   curations
### Aliases: RadioSet-class .RadioSet cellInfo,RadioSet-method
###   cellInfo<-,RadioSet,data.frame-method radiationInfo,RadioSet-method
###   radiationInfo<-,RadioSet,data.frame-method
###   phenoInfo,RadioSet,character-method
###   phenoInfo<-,RadioSet,character,data.frame-method
###   molecularProfiles,RadioSet,character-method
###   molecularProfiles<-,RadioSet,character,matrix-method
###   featureInfo,RadioSet,character-method
###   featureInfo<-,RadioSet,character,data.frame-method
###   sensitivityInfo,RadioSet-method
###   sensitivityInfo<-,RadioSet,data.frame-method
###   sensitivityProfiles,RadioSet-method
###   sensitivityProfiles<-,RadioSet,data.frame-method
###   sensitivityProfiles<-,RadioSet,matrix-method
###   sensitivityMeasures,RadioSet-method radiationTypes,RadioSet-method
###   radiationTypes<-,RadioSet,character-method cellNames,RadioSet-method
###   cellNames<-,RadioSet,character-method
###   fNames,RadioSet,character-method dateCreated,RadioSet-method
###   cSetName,RadioSet-method pertNumber,RadioSet-method
###   sensNumber,RadioSet-method pertNumber<-,RadioSet,array-method
###   sensNumber<-,RadioSet,matrix-method

### ** Examples

data(Cleveland_small)
phenoInfo(Cleveland_small, mDataType="rna")


data(Cleveland_small)
phenoInfo(Cleveland_small, mDataType="rna") <- phenoInfo(Cleveland_small, mDataType="rna")

data(Cleveland_small)
Cleveland_mProf <- molecularProfiles(Cleveland_small, "rna")
Cleveland_mProf[1:10,]

data(Cleveland_small)
molecularProfiles(Cleveland_small, "rna") <- molecularProfiles(Cleveland_small, "rna")

data(Cleveland_small)
featureInfo(Cleveland_small, "rna")[1:10,]

data(Cleveland_small)
featureInfo(Cleveland_small, "rna") <- featureInfo(Cleveland_small, "rna")

data(Cleveland_small)
sensInf<- sensitivityInfo(Cleveland_small)
sensInf[1:10,]

data(Cleveland_small)
sensitivityInfo(Cleveland_small) <- sensitivityInfo(Cleveland_small)

data(Cleveland_small)
sensitivityProfiles(Cleveland_small)

data(Cleveland_small)
sensitivityProfiles(Cleveland_small) <- sensitivityProfiles(Cleveland_small)

data(Cleveland_small)
sensitivityMeasures(Cleveland_small)


data(Cleveland_small)
cellNames(Cleveland_small)

data(Cleveland_small)
cellNames(Cleveland_small) <- cellNames(Cleveland_small)

data(Cleveland_small)
fNames(Cleveland_small, "rna")[1:10]

data(Cleveland_small)
dateCreated(Cleveland_small)

data(Cleveland_small)
rSetName <- cSetName
rSetName(Cleveland_small)

data(Cleveland_small)
pertNumber(Cleveland_small)

data(Cleveland_small)
sensNumber(Cleveland_small)

data(Cleveland_small)
pertNumber(Cleveland_small) <- pertNumber(Cleveland_small)

data(Cleveland_small)
sensNumber(Cleveland_small) <- sensNumber(Cleveland_small)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RadioSet-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RadioSet")
### * RadioSet

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RadioSet
### Title: RadioSet constructor
### Aliases: RadioSet

### ** Examples

## For help creating a RadioSet object, please see the following vignette:
browseVignettes("PharmacoGx")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RadioSet", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("checkRSetStructure")
### * checkRSetStructure

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: checkRSetStructure
### Title: A function to verify the structure of a RadioSet
### Aliases: checkRSetStructure

### ** Examples

data(Cleveland_small)

checkRSetStructure(Cleveland_small)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("checkRSetStructure", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("computeAUC")
### * computeAUC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: computeAUC
### Title: computeAUC: computes AUC
### Aliases: computeAUC

### ** Examples

computeAUC(D=c(0.1, 0.5, 0.7, 0.9), pars=c(0.2, 0.1), lower = 0, upper = 4) # Returns 0.7039296




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("computeAUC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("computeD10")
### * computeD10

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: computeD10
### Title: Compute D10
### Aliases: computeD10

### ** Examples

computeD10(c(0.2, 0.1))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("computeD10", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("computeSF2")
### * computeSF2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: computeSF2
### Title: Compute SF2
### Aliases: computeSF2

### ** Examples

computeSF2(c(0.2, 0.1))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("computeSF2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("doseResponseCurve")
### * doseResponseCurve

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: doseResponseCurve
### Title: Plot drug response curve of a given drug and a given cell for a
###   list of rSets (objects of the RadioSet class).
### Aliases: doseResponseCurve

### ** Examples

if (interactive()) {
drugDoseResponseCurve(Ds=list("Experiment 1" = c(0, 2, 4, 6)),
 SFs=list("Experiment 1" = c(1,.6,.4,.2)), plot.type="Both")
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("doseResponseCurve", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("linearQuadraticModel")
### * linearQuadraticModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: linearQuadraticModel
### Title: Fit linear-quadratic curves to dose-response data
### Aliases: linearQuadraticModel

### ** Examples

linearQuadraticModel(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
 c(1.1, 0.8, 0.7, 0.45, 0.15, -0.1, -0.1, -0.4, -0.65, -0.75, -1.1))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("linearQuadraticModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mDataNames-RadioSet-method")
### * mDataNames-RadioSet-method

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mDataNames,RadioSet-method
### Title: mDataNames
### Aliases: mDataNames,RadioSet-method

### ** Examples

data(Cleveland_small)
mDataNames(Cleveland_small)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mDataNames-RadioSet-method", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotCurve")
### * plotCurve

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotCurve
### Title: Plot radiation dose-response curve
### Aliases: plotCurve

### ** Examples

plotCurve(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    c(1.1, 0.8, 0.7, 0.45, 0.15, -0.1, -0.1, -0.4, -0.65, -0.75, -1.1))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotCurve", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("radSensitivitySig")
### * radSensitivitySig

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: radSensitivitySig
### Title: Creates a signature representing the association between gene
###   expression (or other molecular profile) and radiation dose response,
###   for use in radiation sensitivity analysis.
### Aliases: radSensitivitySig

### ** Examples

data(Cleveland_small)
rad.sensitivity <- radSensitivitySig(Cleveland_small, mDataType="rna",
             nthread=1, features = fNames(Cleveland_small, "rna")[1],
             radiation.types=radiationTypes(Cleveland_small))
print(rad.sensitivity)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("radSensitivitySig", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("radiationInfo-set")
### * radiationInfo-set

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: radiationInfo<-
### Title: radiationInfo<- Generic
### Aliases: radiationInfo<-

### ** Examples

data(Cleveland_small)
radiationInfo(Cleveland_small) <- radiationInfo(Cleveland_small)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("radiationInfo-set", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("radiationInfo")
### * radiationInfo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: radiationInfo
### Title: radiationInfo Generic
### Aliases: radiationInfo

### ** Examples

data(Cleveland_small)
radiationInfo(Cleveland_small)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("radiationInfo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("radiationTypes-set")
### * radiationTypes-set

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: radiationTypes<-
### Title: radiationTypes<- Generic
### Aliases: radiationTypes<-

### ** Examples

data(Cleveland_small)
radiationTypes(Cleveland_small) <- radiationTypes(Cleveland_small)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("radiationTypes-set", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("radiationTypes")
### * radiationTypes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: radiationTypes
### Title: radiationTypes Generic
### Aliases: radiationTypes

### ** Examples

data(Cleveland_small)
radType <- radiationTypes(Cleveland_small)
radType[1:10]




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("radiationTypes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("show-RadioSet-method")
### * show-RadioSet-method

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: show,RadioSet-method
### Title: Show a RadioSet
### Aliases: show,RadioSet-method

### ** Examples

data(Cleveland_small)
Cleveland_small




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("show-RadioSet-method", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("show-RadioSig-method")
### * show-RadioSig-method

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: show,RadioSig-method
### Title: Show RadioGx Signatures
### Aliases: show,RadioSig-method

### ** Examples

data(Cleveland_small)
rad.sensitivity <- radSensitivitySig(Cleveland_small, mDataType="rna",
             nthread=1, features = fNames(Cleveland_small, "rna")[1])
rad.sensitivity




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("show-RadioSig-method", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("showSigAnnot")
### * showSigAnnot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: showSigAnnot
### Title: Show the Annotations of a signature object
### Aliases: showSigAnnot

### ** Examples

data(Cleveland_small)
rad.sensitivity <- radSensitivitySig(Cleveland_small, mDataType="rna",
             nthread=1, features = fNames(Cleveland_small, "rna")[1])
showSigAnnot(rad.sensitivity)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("showSigAnnot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("subsetTo")
### * subsetTo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: subsetTo
### Title: A function to subset a RadioSet to data containing only
###   specified radiations, cells and genes
### Aliases: subsetTo

### ** Examples

data(Cleveland_small)
clevelandRadiationTypes  <- radiationTypes(Cleveland_small)
clevelandCells <- cellNames(Cleveland_small)
RSet <- subsetTo(Cleveland_small,radiationTypes = clevelandRadiationTypes[1],
  cells = clevelandCells[1])
RSet




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("subsetTo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summarizeMolecularProfiles")
### * summarizeMolecularProfiles

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summarizeMolecularProfiles
### Title: Takes molecular data from a PharmacoSet, and summarises them
###   into one entry per drug
### Aliases: summarizeMolecularProfiles

### ** Examples

data(Cleveland_small)
Cleveland_small <- summarizeMolecularProfiles(Cleveland_small,
                    mDataType = "rna", cell.lines=cellNames(Cleveland_small),
                    summary.stat = 'median', fill.missing = TRUE, verbose=TRUE)
Cleveland_small




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summarizeMolecularProfiles", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summarizeSensitivityProfiles")
### * summarizeSensitivityProfiles

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summarizeSensitivityProfiles
### Title: Takes the sensitivity data from a RadioSet, and summarises them
###   into a drug vs cell line table
### Aliases: summarizeSensitivityProfiles

### ** Examples

data(Cleveland_small)
GDSCauc <- summarizeSensitivityProfiles(Cleveland_small, sensitivity.measure='AUC_published')




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summarizeSensitivityProfiles", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
