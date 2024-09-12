# NEWS 

## caretEnsemble 4.0.1
- Speed up the example for autoplot so it runs in <1 second on most platforms

## caretEnsemble 4.0.0
- Multiclass support! caretList, caretStack, and caretEnsemble 
- The greedy optimizer is back! caretEnsemble now uses a greedy optimizer by default. This optimizer can never be worse than the worst single model. caretStack still support all caret models, including glm.
- Refactored some internals for scalability (e.g. data.table for predictions, trim some un-needed data by default).
- Moved all the S3 methods to caretStack, which now supports print, summary, plot, dotplot, and autoplot. caretEnsemble inherits from caretStack, and therefore also supports all of these methods.
- Allow ensembling of mixed lists of classification and regression models.
- Allow ensemble of models with different resampling strategies, so long as they were trained on the same data.
- Allow transfer learning for ensembling models trained on different datasets.
- Added permutation importance as the default importance method for caretLists and caretStacks.
- Add a default trainControl constructor to make it easier to build good controls for training caretLists for stacking with caretStack.
- Expanded test coverage to 100%.
- Sped up test suite (unit tests now run in 20 seconds).
- Delinted codebase: now conforms with all available linters save the object name linter.
- Added a makefile for easier local package development.
- Fixed badges in the readme.
- Added a pkgdown site.
- Switched to github actions (from travis) for CI.
- Internal refactoring, optimization, and bug fixes.

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
