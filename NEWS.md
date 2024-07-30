#NEWS 

## caretEnsemble 2.0.3
- Fix broken package documentation with new roxygen2
- Replace deprecated linters with the new versions

## caretEnsemble 2.0.2
- Fix broken tests on r-devel

## caretEnsemble 2.0.1
- Minor fixes to support R 4.0

## caretEnsemble 2.0.0
- caretEnsemble now inherits from caretStack
- Removed the optimizers and now use a glm for caretEnsemble (optimizers will be added back as caret.train models in a future release)
- Cleaned up namespace (all dependencies are explicit imports, rather than implicit imports or dependencies)
- Removed S3 functions that are not really S3 functions (e.g. autoplot and fortify). We will either make those true S3 classes, or inherit from the packages that define them in a future release
- Fixed the build on travis and locally

## caretEnsemble 1.0.5
- Change output for predict functions to better align with other predict methods 
in R (predict.caretEnsemble and predict.caretStack)
- Update documentation for predict methods to better explain the model disagreement 
calculation
- Speed and memory improvements by switching to data.table for some internals
- Modified the formula for a weighted standard deviation in the model disagreement 
calculation

## caretEnsemble 1.0 - First CRAN release
- caretEnsemble is a new package for making ensembles of [caret](https://CRAN.R-project.org/package=caret/) models.
