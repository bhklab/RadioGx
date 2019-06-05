# RadioGx Package

This is a record of development on the RadioGx package while it is being prepared for CRAN.

## 31.05.19 

### Build 1

**Errors**

```R [RESOLVED]
** testing if installed package keeps a record of temporary installation path
ERROR: hard-coded installation path: please report to the package maintainer and use '--no-staged-install'
```
- R 3.6.0 Staged Install causes problem
  - `--no-staged-install` doesn't work
  - See: 
    1. [R Community - hard-coded installation path](https://community.rstudio.com/t/error-installing-r-package-from-local-file-hard-coded-installation-path/31197/3)
    2. [Stack Overflow - hard-coded installation path?](https://stackoverflow.com/questions/56365021/hisafer-installation-how-do-i-solve-hard-coded-installation-path)
    3. [R-Project Blog - staged instal feature](https://developer.r-project.org/Blog/public/2019/02/14/staged-install/index.html)
  - Workaround: `Tools > Project Options > Build-tools` add `--no-staged-install` to `Install and Restart` options
    - This issue will likely be patched by R
    - Will investigate further once CRAN check is completed
    - Solution was to change `sessionInfo()` to `sessionInfo` in conversion of S3 to S4 object
        - sessionInfo() returns a path which the staged-installl feature considers 'hard-coded'

```R
* checking CRAN incoming feasibility ... WARNING
Maintainer: 'Benjamin Haibe-Kains <benjamin.haibe.kains@utoronto.ca>'

New submission

Version contains large components (0.0.0.0.9000)

Non-FOSS package license (GNU GPL-3.0)

Strong dependencies not in mainstream repositories:
  CoreGx

The Title field should be in title case. Current version is:
'What the Package Does (one line, title case)'
In title case that is:
'What the Package Does (One Line, Title Case)'
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Namespace dependencies not required:
  'Biobase', 'RColorBrewer', 'caTools', 'magicaxis', 'methods',
  'reshape2'
```
- Changed title to `Analysis of Large-Scale Radiogenomic Data`; same format as PharmacoGx
- Added `Author` and `Maintainer` fields for BHK
- Added `Imports: Biobase, RColorBrewer, caTolls, magicaxis, methods, reshape2`
- Added `Date: 2019-05-31`

**Warnings**

```R [RESOLVED]
* looking to see if a 'data/datalist' file should be added
  NB: this package now depends on R (>= 3.5.0)
  WARNING: Added dependency on R >= 3.5.0 because serialized objects in  serialize/load version 3 cannot be read in older versions of R.  File(s) containing such objects: 'RadioGx/data/Cleveland_small.RData'
* building 'RadioGx_0.0.0.0.9000.tar.gz'
```
- Changed to `Depends: R (>=3.5.0)`

### Build 2

**Errors**

```R
* checking whether package 'RadioGx' can be installed ... ERROR
Installation failed.
See 'C:/Users/ChrisEeles/OneDrive - UHN/R_Packages/RadioGx.Rcheck/00install.out' for details.
* DONE
Status: 1 ERROR, 1 WARNING

See
  'C:/Users/ChrisEeles/OneDrive - UHN/R_Packages/RadioGx.Rcheck/00check.log'
for details.
```
- Log has no additional details
- I believe this is due to the staged installation
    - Can we force --no-staged-install in a released package?
    - Can we resolve what ever is causing the staged install issue?
        - May need to wait for R patch?
- Solution was to change `sessionInfo()` to `sessionInfo` in conversion of S3 to S4 object
  - sessionInfo() returns a path which the staged-installl feature considers 'hard-coded'

**Warnings**

```R
* checking CRAN incoming feasibility ... WARNING
Maintainer: 'Benjamin Haibe-Kains <benjamin.haibe.kains@utoronto.ca>'

New submission

Version contains large components (0.0.0.0.9000)

Non-FOSS package license (GNU GPL-3.0)

Strong dependencies not in mainstream repositories:
  CoreGx
```
- This likely can't be resolved until CoreGx is on CRAN
- Work around: add `zzz.R` with `.onAttach <- function(libname=NULL, pkgname){ devtools::install_github("BHKLab/CoreGx,ref="CRAN_Debug") }`


```R
Warning messages:
1: For function ‘cellInfo’, signature ‘RadioSet’: argument in method definition changed from (rSet) to (cSet) 
2: For function ‘sensitivityInfo’, signature ‘RadioSet’: argument in method definition changed from (rSet) to (cSet) 
3: For function ‘sensitivityProfiles’, signature ‘RadioSet’: argument in method definition changed from (rSet) to (cSet) 
4: For function ‘sensitivityMeasures’, signature ‘RadioSet’: argument in method definition changed from (rSet) to (cSet) 
5: For function ‘cellNames’, signature ‘RadioSet’: argument in method definition changed from (rSet) to (cSet) 
6: For function ‘dateCreated’, signature ‘RadioSet’: argument in method definition changed from (rSet) to (cSet) 
7: For function ‘pertNumber’, signature ‘RadioSet’: argument in method definition changed from (rSet) to (cSet) 
8: For function ‘sensNumber’, signature ‘RadioSet’: argument in method definition changed from (rSet) to (cSet) 
```
- Tried changing `"RadioSet"` to `signature(rSet = '"RadioSet"`)
    - This resolves the warning but causes an error

### Build 2 

**Errors**

```R
* checking examples ... ERROR
Running examples in 'RadioGx-Ex.R' failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: computeAUC
> ### Title: computeAUC: computes AUC
> ### Aliases: computeAUC
> 
> ### ** Examples
> 
> computeAUC(pars = c(0.2, 0.1), lower = 0, upper = 4)
Error in CoreGx::.reformatData(x = D, pars = pars, x_to_log = FALSE,  : 
  argument "D" is missing, with no default
Calls: computeAUC -> <Anonymous> -> is.unsorted
Execution halted
```
- Added `pars = pars` to `CoreGx::.sanitizeInput()` call line 66
- Added `SF_as_log` to `computeAUC()` definition and modified `if(SF_as_log)` to `if(SF_as_log == TRUE)`
- Check function for correct output

**Warnings**

```R
* checking Rd \usage sections ... WARNING
Undocumented arguments in documentation object 'RadioSet-class'
  'cSet'
Duplicated \argument entries in documentation object 'RadioSet-class':
  'rSet' 'mDataType' 'object' 'mDataType' 'value' 'rSet' 'mDataType'
  'object' 'mDataType' 'value' 'rSet' 'mDataType' 'object' 'mDataType'
  'value' 'rSet' 'object' 'value' 'rSet' 'object' 'value' 'rSet' 'rSet'
  'object' 'value' 'rSet' 'mDataType' 'rSet' 'rSet' 'rSet' 'object'
  'value' 'object' 'value'

Undocumented arguments in documentation object 'computeAUC'
  'SF_as_log'

Documented arguments not in \usage in documentation object 'plotCurve':
  'SF_as_log'

Undocumented arguments in documentation object 'summarizeSensitivityProfiles'
  'radiation.types'
Documented arguments not in \usage in documentation object 'summarizeSensitivityProfiles':
  'drugs'

Functions with \usage entries need to have the appropriate \alias
entries, and all their arguments documented.
The \usage entries must correspond to syntactically valid R code.
See chapter 'Writing R documentation files' in the 'Writing R
```
****
- Need to update documentation

### Build 3

**Errors**
```R
 Error in callNextMethod(rSet) : 
  a call to callNextMethod() appears in a call to ‘callNextMethod’, but the call does not seem to come from either a generic function or another 'callNextMethod' 
3.
stop(gettextf("a call to callNextMethod() appears in a call to %s, but the call does not seem to come from either a generic function or another 'callNextMethod'", 
    sQuote(f)), domain = NA) 
2.
callNextMethod(rSet) at RadioSet.R#790
1.
mDataNames(Cleveland_small) 
```
- Requirements to import functions from another class
  1. Import generic with `@importFrom package method`
  2. Import method with `@importMethodsFrom package method`
  3. Export the method in the new package with `@export`
  4. Define the new method with `setMethod()` and `callNextMethods`
    - Didn't work
    - Syntax for importing generic function from another package?


**Warnings**

```R
Loading RadioGx
Warning message:
For function ‘sensNumber’, signature ‘RadioSet’: argument in method definition changed from (rSet) to (cSet) 
```
- Occurs because parameter name in CoreGx is `cSet` and `callNextMethod(rSet)` passes the wrong parameter name
- Need to figure out if/how we can rename parameters in extended methods
- Similar problem RE: generic names
  - Work around is to use CoreGx generic name and use `rMethodName <- cMethodName` after the method is extended in RadioGx

### Build 4

**Errors**
```R
* checking examples ... ERROR
Running examples in 'RadioGx-Ex.R' failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: radSensitivitySig
> ### Title: Creates a signature representing the association between gene
> ###   expression (or other molecular profile) and radiation dose response,
> ###   for use in radiation sensitivity analysis.
> ### Aliases: radSensitivitySig
> 
> ### ** Examples
> 
> data(Cleveland_small)
> rad.sensitivity <- radSensitivitySig(Cleveland_small, mDataType="rna",
+              nthread=1, features = fNames(Cleveland_small, "rna")[1])
Error in dim(ordered) <- ns : 
  dims [product 1] do not match the length of object [0]
Calls: radSensitivitySig ... summarizeSensitivityProfiles -> <Anonymous> -> cast
Execution **halted**
```
- `Cleveland_small@radiation` was empty, added with `Cleveland_small@radiation <- Cleveland_mut@radiation`
- Also returned error for `radiation.types` missing, fixed by adding `radiation.types = as.character(Cleveland_small@radiation)`
- Changed to `radiationTypes(Cleveland_small`

**Errors**
```R

```

**Warnings**


